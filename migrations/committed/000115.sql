--! Previous: sha1:4e3a400f98a649a2e67ba7f1aeb85fc25657b266
--! Hash: sha1:a990ad64a25c3571324f09c10ec2bcc7e792d8a7

--! split: 1-current.sql
do $$
begin
  if exists (
    select 1
    from public.event_instance instance
    where instance.event_id is not null
      and (
        instance.type is null
        or instance.location_text is null
        or instance.is_visible is null
        or instance.is_public is null
        or instance.capacity is null
        or instance.description is null
        or instance.summary is null
        or instance.is_locked is null
        or instance.enable_notes is null
        or instance.files_legacy is null
      )
  ) then
    raise exception 'LEGACY_LINKED_INSTANCE_WITH_INCOMPLETE_DATA';
  end if;
end
$$;

drop function if exists public.event_overlaps_attendee_report(timestamptz, timestamptz);
drop function if exists public.event_overlaps_trainer_report(timestamptz, timestamptz);
alter type public.event_overlaps_conflict drop attribute if exists first_event_id;
alter type public.event_overlaps_conflict drop attribute if exists second_event_id;

do $do$
begin
  if exists (
    select 1
    from pg_catalog.pg_enum enum
    join pg_catalog.pg_type type on type.oid = enum.enumtypid
    join pg_catalog.pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public'
      and type.typname = 'attendance_type'
      and enum.enumlabel = 'cancelled'
  ) then
    execute 'drop trigger if exists _100_timestamps on public.event_instance_registration';
    execute 'drop trigger if exists _200_eir_attendance_timestamps on public.event_instance_registration';
    execute 'alter table public.event_instance_registration disable trigger _500_eir_refresh_stats_upd';

    update public.event_instance_registration
    set status = 'unknown'
    where status = 'cancelled';

    execute 'drop function if exists public.update_attendance(bigint, public.attendance_type, text)';
    execute 'alter type public.attendance_type rename to attendance_type_with_cancelled';
    execute $$create type public.attendance_type as enum ('unknown', 'attended', 'not-excused')$$;
    execute $sql$
      alter table public.event_instance_registration
      alter column status type public.attendance_type
      using status::text::public.attendance_type
    $sql$;
    execute 'drop type public.attendance_type_with_cancelled';

    execute $trigger$
      create trigger _100_timestamps
      before insert or update of
        tenant_id, instance_id, parent_registration_id, couple_id, person_id,
        target_cohort_id, status, note, attendance_note, registration_status,
        source, created_at, updated_at
      on public.event_instance_registration
      for each row execute function app_private.tg__timestamps()
    $trigger$;
    execute $trigger$
      create trigger _200_eir_attendance_timestamps
      before insert or update of person_id, status, attendance_note
      on public.event_instance_registration
      for each row execute function app_private.tg_eir__attendance_timestamps()
    $trigger$;
    execute 'alter table public.event_instance_registration enable trigger _500_eir_refresh_stats_upd';
  end if;
end
$do$;

--! Included functions/update_attendance.sql
create or replace function update_attendance(eir_id bigint, status attendance_type, note text)
  returns event_instance_registration
  language sql as $$
  update event_instance_registration eir
  set status = $2, attendance_note = $3
  where eir.id = $1 and eir.person_id is not null and eir.registration_status = 'active'
  returning eir.*;
$$;
--! EndIncluded functions/update_attendance.sql
--! Included functions/activity_timeline.sql
create or replace view public.activity_timeline_item as
select
  null::text as id,
  null::public.activity_timeline_kind as kind,
  null::timestamptz as sort_at,
  null::date as activity_date,
  null::bigint as person_id,
  null::text as person_name,
  null::bigint as event_attendance_id,
  null::bigint as event_instance_id,
  null::text as federation,
  null::text as federated_person_id,
  null::text as competitor_id,
  null::text as competitor_name,
  null::federated.competitor_type as competitor_type,
  null::bigint as competition_event_id,
  null::text as competition_event_name,
  null::text as competition_event_location,
  null::bigint as competition_id,
  null::date as competition_date,
  null::time as check_in_end,
  null::federated.category as category,
  null::text[] as dances,
  null::integer as participants,
  null::integer as ranking,
  null::integer as ranking_to,
  null::numeric(10, 3) as point_gain,
  null::boolean as is_final,
  null::federated.competition_type as competition_type,
  null::text as competition_event_external_id,
  null::text as competition_external_id
where false;

comment on view public.activity_timeline_item is $$
@primaryKey id
@interface mode:single type:kind
@type EVENT_ATTENDANCE name:ActivityEventAttendance attributes:event_attendance_id,event_instance_id
@type COMPETITION_BRIEF name:ActivityCompetitionBrief attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,check_in_end,category,dances,participants,competition_type,competition_event_external_id,competition_external_id
@type COMPETITION_RESULT name:ActivityCompetitionResult attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,dances,participants,ranking,ranking_to,point_gain,is_final,competition_type,competition_event_external_id,competition_external_id
@type JUDGING name:ActivityJudging attributes:federation,federated_person_id,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,competition_type,competition_event_external_id,competition_external_id
@type BIRTHDAY name:ActivityBirthday
@foreignKey (person_id) references person (id)|@fieldName person|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_attendance_id) references event_instance_registration (id)|@fieldName eventAttendance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_instance_id) references event_instance (id)|@fieldName eventInstance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@behavior -query:resource:list -query:resource:connection -query:resource:single
$$;

grant select on public.activity_timeline_item to anonymous;

CREATE OR REPLACE FUNCTION public.activity_timeline(p_since timestamp with time zone, p_until timestamp with time zone, p_person_ids bigint[] DEFAULT NULL::bigint[], p_cohort_id bigint DEFAULT NULL::bigint, p_kinds activity_timeline_kind[] DEFAULT NULL::activity_timeline_kind[], p_event_types event_type[] DEFAULT NULL::event_type[])
 RETURNS SETOF activity_timeline_item
 LANGUAGE plpgsql
 STABLE
AS $function$
declare
  include_event_attendance boolean;
  include_competition_brief boolean;
  include_competition_result boolean;
  include_judging boolean;
  include_birthday boolean;
begin
  if p_since is null or p_until is null or p_until <= p_since then
    return;
  end if;

  if cardinality(p_kinds) = 0 then
    return;
  end if;

  include_event_attendance =
    (p_kinds is null or 'EVENT_ATTENDANCE'::activity_timeline_kind = any(p_kinds))
    and (p_event_types is null or cardinality(p_event_types) > 0);
  include_competition_brief =
    p_kinds is null or 'COMPETITION_BRIEF'::activity_timeline_kind = any(p_kinds);
  include_competition_result =
    p_kinds is null or 'COMPETITION_RESULT'::activity_timeline_kind = any(p_kinds);
  include_judging =
    p_kinds is null or 'JUDGING'::activity_timeline_kind = any(p_kinds);
  include_birthday =
    p_kinds is null or 'BIRTHDAY'::activity_timeline_kind = any(p_kinds);

  if include_event_attendance then
    return query
      with scoped_people as (
        select distinct p.id, p.name
        from person p
        where (exists (select 1 from current_tenant_membership where person_id = p.id)
           or exists (select 1 from current_tenant_trainer where person_id = p.id)
           or exists (select 1 from current_tenant_administrator where person_id = p.id))
          and (p_person_ids is null or p.id = any(p_person_ids))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
      )
      select
        ('event_attendance:' || ea.id)::text as id,
        'EVENT_ATTENDANCE'::activity_timeline_kind as kind,
        ei.since as sort_at,
        ei.since::date as activity_date,
        ea.person_id,
        sp.name as person_name,
        ea.id as event_attendance_id,
        ei.id as event_instance_id,
        null::text as federation,
        null::text as federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        null::bigint as competition_event_id,
        null::text as competition_event_name,
        null::text as competition_event_location,
        null::bigint as competition_id,
        null::date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::federated.competition_type as competition_type,
        null::text as competition_event_external_id,
        null::text as competition_external_id
      from event_instance_registration ea
      join event_instance ei on ei.id = ea.instance_id
      join scoped_people sp on sp.id = ea.person_id
      where ea.registration_status = 'active'
        and ei.since >= p_since
        and ei.since < p_until
        and (p_event_types is null or ei.type = any(p_event_types));
  end if;

  if include_birthday then
    return query
      with scoped_people as (
        select distinct p.id, p.name, p.birth_date
        from person p
        where (exists (select 1 from current_tenant_membership where person_id = p.id)
           or exists (select 1 from current_tenant_trainer where person_id = p.id)
           or exists (select 1 from current_tenant_administrator where person_id = p.id))
          and (p_person_ids is null or p.id = any(p_person_ids))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
          and p.birth_date is not null
      ),
      birthdays as (
        select
          sp.id as person_id,
          sp.name as person_name,
          make_date(
            years.year,
            extract(month from sp.birth_date)::int,
            least(
              extract(day from sp.birth_date)::int,
              extract(day from (
                make_date(years.year, extract(month from sp.birth_date)::int, 1)
                + interval '1 month - 1 day'
              ))::int
            )
          ) as birthday_date
        from scoped_people sp
        cross join generate_series(extract(year from p_since)::int, extract(year from p_until)::int) as years(year)
      )
      select
        ('birthday:' || b.person_id || ':' || b.birthday_date)::text as id,
        'BIRTHDAY'::activity_timeline_kind as kind,
        ((b.birthday_date + time '12:00')::timestamp)::timestamptz as sort_at,
        b.birthday_date as activity_date,
        b.person_id,
        b.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        null::text as federation,
        null::text as federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        null::bigint as competition_event_id,
        null::text as competition_event_name,
        null::text as competition_event_location,
        null::bigint as competition_id,
        null::date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::federated.competition_type as competition_type,
        null::text as competition_event_external_id,
        null::text as competition_external_id
      from birthdays b
      join person p on p.id = b.person_id
      where b.birthday_date >= p.birth_date
        and ((b.birthday_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((b.birthday_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;

  if include_competition_result then
    return query
      select
        (
          'competition_result:' ||
          coalesce(cr.competition_id::text, '') || ':' ||
          coalesce(cr.competitor_id, '') || ':' ||
          coalesce(cr.person_id::text, '') || ':' ||
          coalesce((cr.category).id::text, '')
        )::text as id,
        'COMPETITION_RESULT'::activity_timeline_kind as kind,
        ((cr.competition_date + time '12:00')::timestamp)::timestamptz as sort_at,
        cr.competition_date as activity_date,
        cr.person_id,
        cr.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        cr.federation,
        cr.federated_person_id,
        cr.competitor_id,
        cr.competitor_name,
        cr.competitor_type,
        cr.event_id as competition_event_id,
        cr.event_name as competition_event_name,
        cr.event_location as competition_event_location,
        cr.competition_id,
        cr.competition_date,
        null::time as check_in_end,
        cr.category,
        cr.dances,
        cr.participants,
        cr.ranking,
        cr.ranking_to,
        cr.point_gain,
        cr.is_final,
        cr.competition_type,
        cr.event_external_id as competition_event_external_id,
        cr.competition_external_id
      from (
        select * from competition_report(p_since::date, p_until::date, p_cohort_id, p_person_ids)
      ) as cr
      where cr.competition_date is not null
        and ((cr.competition_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((cr.competition_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;

  if include_competition_brief then
    return query
      with reports as (
        select
          cr.person_id,
          cr.competition_id,
          cr.competitor_id,
          (cr.category).id as category_id
        from (
          select * from competition_report(p_since::date, p_until::date, p_cohort_id, p_person_ids)
        ) as cr
        where include_competition_result
      )
      select
        (
          'competition_brief:' ||
          coalesce(cb.competition_id::text, '') || ':' ||
          coalesce(cb.competitor_id, '') || ':' ||
          coalesce(cb.person_id::text, '') || ':' ||
          coalesce((cb.category).id::text, '')
        )::text as id,
        'COMPETITION_BRIEF'::activity_timeline_kind as kind,
        ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz as sort_at,
        cb.competition_date as activity_date,
        cb.person_id,
        cb.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        cb.federation,
        cb.federated_person_id,
        cb.competitor_id,
        cb.competitor_name,
        cb.competitor_type,
        cb.event_id as competition_event_id,
        cb.event_name as competition_event_name,
        cb.event_location as competition_event_location,
        cb.competition_id,
        cb.competition_date,
        cb.check_in_end,
        cb.category,
        cb.dances,
        cb.participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        cb.competition_type,
        cb.event_external_id as competition_event_external_id,
        cb.competition_external_id
      from (
        select * from competition_brief(p_since::date, p_until::date, p_cohort_id, p_person_ids)
      ) as cb
      where cb.competition_date is not null
        and ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz >= p_since
        and ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz < p_until
        and not exists (
          select 1
          from reports r
          where r.person_id is not distinct from cb.person_id
            and r.competition_id is not distinct from cb.competition_id
            and r.competitor_id is not distinct from cb.competitor_id
            and r.category_id is not distinct from (cb.category).id
        );
  end if;

  if include_judging then
    return query
      with scoped_people as (
        select distinct p.id, p.name, p.csts_id, p.wdsf_id
        from person p
        where (exists (select 1 from current_tenant_membership where person_id = p.id)
           or exists (select 1 from current_tenant_trainer where person_id = p.id)
           or exists (select 1 from current_tenant_administrator where person_id = p.id))
          and (p_person_ids is null or p.id = any(p_person_ids))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
      ),
      federated_people as (
        select
          sp.id as person_id,
          sp.name as person_name,
          fp.id as federated_person_id,
          fp.federation
        from scoped_people sp
        join federated.person fp
          on (fp.federation = 'csts' and fp.external_id = sp.csts_id)
          or (fp.federation = 'wdsf' and fp.external_id = sp.wdsf_id)
      )
      select distinct
        ('judging:event:' || e.id || ':' || fp.person_id)::text as id,
        'JUDGING'::activity_timeline_kind as kind,
        ((e.start_date + time '12:00')::timestamp)::timestamptz as sort_at,
        e.start_date as activity_date,
        fp.person_id,
        fp.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        e.federation,
        fp.federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        e.id as competition_event_id,
        e.name as competition_event_name,
        coalesce(e.location, e.city) as competition_event_location,
        null::bigint as competition_id,
        e.start_date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::federated.competition_type as competition_type,
        e.external_id as competition_event_external_id,
        null::text as competition_external_id
      from federated.event_official eo
      join federated.event e on e.id = eo.event_id
      join federated_people fp on fp.federated_person_id = eo.person_id
      where eo.role = 'adjudicator'
        and e.start_date >= current_date
        and ((e.start_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((e.start_date + time '12:00')::timestamp)::timestamptz < p_until
      union all
      select
        ('judging:competition:' || c.id || ':' || fp.person_id)::text as id,
        'JUDGING'::activity_timeline_kind as kind,
        ((c.start_date + time '12:00')::timestamp)::timestamptz as sort_at,
        c.start_date as activity_date,
        fp.person_id,
        fp.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        c.federation,
        fp.federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        e.id as competition_event_id,
        e.name as competition_event_name,
        coalesce(e.location, e.city) as competition_event_location,
        c.id as competition_id,
        c.start_date as competition_date,
        null::time as check_in_end,
        cat as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        c.competition_type,
        e.external_id as competition_event_external_id,
        c.external_id as competition_external_id
      from federated.competition_official co
      join federated.competition c on c.id = co.competition_id
      join federated.event e on e.id = c.event_id
      join federated.category cat on cat.id = c.category_id
      join federated_people fp on fp.federated_person_id = co.person_id
      where co.role = 'adjudicator'
        and c.start_date < current_date
        and ((c.start_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((c.start_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;
end;
$function$;

select verify_function('public.activity_timeline');
comment on function public.activity_timeline(
  timestamptz,
  timestamptz,
  bigint[],
  bigint,
  public.activity_timeline_kind[],
  public.event_type[]
) is '@behavior +queryField:resource:list -queryField:resource:connection';
grant all on function public.activity_timeline(
  timestamptz,
  timestamptz,
  bigint[],
  bigint,
  public.activity_timeline_kind[],
  public.event_type[]
) to anonymous;
--! EndIncluded functions/activity_timeline.sql
--! Included functions/create_event_instance_payment.sql
create or replace function public.create_event_instance_payment(i public.event_instance)
  returns public.payment
  language plpgsql
as $$
declare
  created_payment public.payment;
  duration numeric(19, 4);
begin
  if current_tenant_id() <> 2 then
    return null;
  end if;

  select p.* into created_payment
  from public.payment p
  where p.event_instance_id = i.id;

  if found then
    return created_payment;
  end if;

  if i.type <> 'lesson' or not exists (
    select 1
    from public.event_instance_registration registration
    where registration.instance_id = i.id
      and registration.person_id is not null
      and registration.registration_status = 'active'
  ) then
    return null;
  end if;

  duration := extract(epoch from (i.until - i.since)) / 60;

  insert into public.payment (accounting_period_id, status, event_instance_id, due_at)
  values (
    (select id from public.accounting_period where tenant_id = current_tenant_id() and range @> now()),
    'tentative', i.id, now() + interval '2 weeks'
  )
  on conflict (event_instance_id) where event_instance_id is not null do nothing
  returning * into created_payment;

  if not found then
    return (select p from public.payment p where p.event_instance_id = i.id);
  end if;

  insert into public.payment_recipient (payment_id, account_id, amount)
  select distinct on (account.id)
    created_payment.id,
    account.id,
    trainer.member_price_45min_amount / 45 * duration
  from public.event_instance_trainers(i) trainer
  cross join lateral public.person_account(trainer.person_id, trainer.currency) account
  where trainer.tenant_id = current_tenant_id()
    and trainer.member_price_45min_amount is not null;

  insert into public.payment_debtor (payment_id, person_id)
  select distinct created_payment.id, registration.person_id
  from public.event_instance_registration registration
  where registration.instance_id = i.id
    and registration.person_id is not null
    and registration.registration_status = 'active';

  return created_payment;
end
$$;

select verify_function('create_event_instance_payment');

COMMENT ON FUNCTION create_event_instance_payment IS '@omit';
GRANT ALL ON FUNCTION create_event_instance_payment TO anonymous;
--! EndIncluded functions/create_event_instance_payment.sql
--! Included functions/event_instance_stats.sql
create or replace function app_private.refresh_event_instance_stats(p_instance_id bigint)
  returns void
  language sql volatile security definer
  set search_path = pg_catalog, pg_temp
as $$
  select 1 from public.event_instance where id = p_instance_id for no key update;

  update public.event_instance instance
  set stats = actual.stats
  from (
    select jsonb_build_object(
      'TOTAL', count(*)::int,
      'UNKNOWN', count(*) filter (where status = 'unknown')::int,
      'ATTENDED', count(*) filter (where status = 'attended')::int,
      'NOT_EXCUSED', count(*) filter (where status = 'not-excused')::int
    ) as stats
    from public.event_instance_registration
    where instance_id = p_instance_id
      and person_id is not null
      and registration_status = 'active'
  ) actual
  where instance.id = p_instance_id and instance.stats is distinct from actual.stats;
$$;
revoke execute on function app_private.refresh_event_instance_stats(bigint)
  from public, anonymous;

create or replace function app_private.tg_eir__refresh_stats_stmt()
  returns trigger
  language plpgsql security definer
  set search_path = pg_catalog, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
begin
  if tg_op = 'INSERT' then
    perform app_private.refresh_event_instance_stats(affected.instance_id)
    from (
      select distinct instance_id from changed_rows
      where person_id is not null order by instance_id
    ) affected;
  elsif tg_op = 'DELETE' then
    perform app_private.refresh_event_instance_stats(affected.instance_id)
    from (
      select distinct instance_id from deleted_rows
      where person_id is not null order by instance_id
    ) affected;
  else
    perform app_private.refresh_event_instance_stats(affected.instance_id)
    from (
      select distinct affected.instance_id
      from deleted_rows old_row
      join changed_rows new_row using (id)
      cross join lateral (values
        (old_row.instance_id, old_row.person_id),
        (new_row.instance_id, new_row.person_id)
      ) affected(instance_id, person_id)
      where row(old_row.instance_id, old_row.person_id, old_row.status, old_row.registration_status)
        is distinct from
        row(new_row.instance_id, new_row.person_id, new_row.status, new_row.registration_status)
        and affected.person_id is not null
      order by affected.instance_id
    ) affected;
  end if;
  return null;
end;
$$;
select verify_function(
  'app_private.tg_eir__refresh_stats_stmt',
  'public.event_instance_registration'
);
revoke execute on function app_private.tg_eir__refresh_stats_stmt()
  from public, anonymous;
--! EndIncluded functions/event_instance_stats.sql
--! Included functions/event_instances_for_range.sql
CREATE or replace FUNCTION public.event_instances_for_range(
  only_type public.event_type,
  start_range timestamp with time zone,
  end_range timestamp with time zone DEFAULT NULL::timestamp with time zone,
  trainer_ids bigint[] = null,
  participant_ids bigint[] = null,
  only_mine boolean = false,
  parent_id bigint = null
) RETURNS SETOF event_instance as $$
  select i.*
  from event_instance i
  where i.tenant_id = current_tenant_id()
    and (only_type IS NULL OR i.type = only_type)
    and i.parent_id is not distinct from $7
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any (participant_ids) and registration_status = 'active'))
    and (only_mine is false
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any ((select current_person_ids())::bigint[]) and registration_status = 'active')
      or i.manager_person_ids && ((select current_person_ids())::bigint[]));
$$ stable language sql;

COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;
--! EndIncluded functions/event_instances_for_range.sql
--! Included functions/event_overlaps_reports.sql
create or replace function public.event_overlaps_attendee_report(
  p_since timestamptz,
  p_until timestamptz
)
returns setof public.event_overlaps_conflict
language sql
stable
as $$
  with target_range as (
    select tstzrange(
      coalesce(p_since, '-infinity'::timestamptz),
      coalesce(p_until, 'infinity'::timestamptz),
      '[]'
    ) as range
  ),
  instances as (
    select
      ea.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from public.event_instance_registration ea
    join public.event_instance ei on ei.id = ea.instance_id
    join public.person p on p.id = ea.person_id
    join target_range tr on true
    where
      ei.tenant_id = public.current_tenant_id()
      and ea.person_id is not null
      and ea.registration_status = 'active'
      and not ei.is_cancelled
      and ei.range && tr.range
  )
  select
    i1.person_id,
    i1.person_name,
    i1.instance_id as first_instance_id,
    i1.event_name as first_event_name,
    i1.since as first_since,
    i1.until as first_until,
    i2.instance_id as second_instance_id,
    i2.event_name as second_event_name,
    i2.since as second_since,
    i2.until as second_until,
    tstzrange(
      greatest(i1.since, i2.since),
      least(i1.until, i2.until),
      '[]'
    ) as overlap_range
  from instances i1
  join instances i2 on i1.person_id = i2.person_id
    and i1.instance_id < i2.instance_id
    and i1.range && i2.range
    and greatest(i1.since, i2.since) < least(i1.until, i2.until);
$$;

comment on function public.event_overlaps_attendee_report is '@simpleCollections only';
grant all on function public.event_overlaps_attendee_report to anonymous;

create or replace function public.event_overlaps_trainer_report(
  p_since timestamptz,
  p_until timestamptz
)
returns setof public.event_overlaps_conflict
language sql
stable
as $$
  with target_range as (
    select tstzrange(
      coalesce(p_since, '-infinity'::timestamptz),
      coalesce(p_until, 'infinity'::timestamptz),
      '[]'
    ) as range
  ),
  trainer_instances as (
    select
      trainer.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from public.event_instance ei
    cross join lateral app_private.event_instance_trainers_at(ei, ei.since) trainer
    join public.person p on p.id = trainer.person_id
    join target_range tr on true
    where
      ei.tenant_id = public.current_tenant_id()
      and not ei.is_cancelled
      and ei.range && tr.range
  )
  select
    ti1.person_id,
    ti1.person_name,
    ti1.instance_id as first_instance_id,
    ti1.event_name as first_event_name,
    ti1.since as first_since,
    ti1.until as first_until,
    ti2.instance_id as second_instance_id,
    ti2.event_name as second_event_name,
    ti2.since as second_since,
    ti2.until as second_until,
    tstzrange(
      greatest(ti1.since, ti2.since),
      least(ti1.until, ti2.until),
      '[]'
    ) as overlap_range
  from trainer_instances ti1
  join trainer_instances ti2 on ti1.person_id = ti2.person_id
    and ti1.instance_id < ti2.instance_id
    and ti1.range && ti2.range
    and greatest(ti1.since, ti2.since) < least(ti1.until, ti2.until);
$$;

comment on function public.event_overlaps_trainer_report is '@simpleCollections only';
grant all on function public.event_overlaps_trainer_report to anonymous;
--! EndIncluded functions/event_overlaps_reports.sql
--! Included functions/event_remaining_x.sql
CREATE or replace FUNCTION event_instance_trainer_lessons_remaining(e event_instance_trainer) RETURNS integer AS $$
  select case
    when e.lessons_offered is null then null
    else e.lessons_offered - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_demand demand
      join event_instance_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      where demand.trainer_id = e.id
    )
  end;
$$ LANGUAGE sql STABLE;

CREATE or replace FUNCTION event_instance_remaining_person_spots(inst event_instance) RETURNS integer AS $$
  select case
    when inst.capacity is null or inst.capacity <= 0 then null
    else inst.capacity - case inst.capacity_unit
        when 'people' then (
          select count(*)::integer
          from event_instance_registration registration
          where registration.instance_id = inst.id
            and registration.person_id is not null
            and registration.registration_status = 'active'
        )
        when 'registrations' then (
          select count(*)::integer
          from event_instance_registration registration
          where registration.instance_id = inst.id
            and registration.parent_registration_id is null
            and registration.registration_status = 'active'
        )
      end - (
        select count(*)::integer
        from event_external_registration external_registration
        where external_registration.instance_id = inst.id
      )
  end;
$$ LANGUAGE sql STABLE security definer set search_path = public, pg_catalog, pg_temp;

GRANT ALL ON FUNCTION event_instance_trainer_lessons_remaining(e event_instance_trainer) TO anonymous;
GRANT ALL ON FUNCTION event_instance_remaining_person_spots(inst event_instance) TO anonymous;
--! EndIncluded functions/event_remaining_x.sql
--! Included functions/quick_create_event_instances.sql
drop function if exists public.quick_create_event_instances(
  public.quick_event_input[], bigint
);
drop function if exists public.quick_create_event_instances(
  public.quick_event_input[], bigint, boolean, boolean, boolean, boolean
);
drop function if exists public.quick_create_event_instances(
  public.quick_event_input[], bigint, boolean, boolean, boolean, boolean,
  bigint, text, integer, public.event_capacity_unit, text, text, text,
  bigint[], integer[]
);
drop function if exists public.quick_create_event_instances(
  public.quick_event_input[], bigint, boolean, boolean, boolean, boolean,
  bigint, text, integer, public.event_capacity_unit, text, text, text,
  bigint[], integer[], boolean
);

create or replace function quick_create_event_instances(
  events quick_event_input[],
  parent_id bigint default null,
  p_is_visible boolean default true,
  p_is_public boolean default false,
  p_is_locked boolean default false,
  p_enable_notes boolean default false,
  p_series_id bigint default null,
  p_name text default null,
  p_capacity integer default null,
  p_capacity_unit event_capacity_unit default 'people',
  p_description text default '',
  p_summary text default '',
  p_files_legacy text default '',
  p_cohort_ids bigint[] default null,
  p_trainer_lessons_offered integer[] default null,
  p_copies quick_event_input[] default null
) returns setof event_instance
  language plpgsql
as $$
declare
  quick_event quick_event_input;
  created_instance event_instance;
  v_series_id bigint := p_series_id;
  instances quick_event_input[] := coalesce(events, '{}'::quick_event_input[]);
begin
  if cardinality(coalesce(p_copies, '{}'::quick_event_input[])) > 0 then
    if p_series_id is not null then
      raise exception 'cannot copy into an existing event series';
    end if;
    if cardinality(instances) <> 1 then
      raise exception 'event copies require exactly one source event';
    end if;

    insert into event_series (name)
    values (p_name)
    returning id into v_series_id;

    instances := instances || p_copies;
  end if;

  foreach quick_event in array instances loop
    insert into event_instance (
      parent_id, series_id, since, until, name, type, location_id, location_text,
      capacity, capacity_unit, is_visible, is_public, is_locked, enable_notes,
      description, summary, files_legacy
    ) values (
      parent_id, v_series_id, quick_event.since, quick_event.until, p_name,
      coalesce(quick_event.type, 'lesson'), quick_event.location_id,
      coalesce(quick_event.location_text, ''),
      coalesce(p_capacity,
        case when coalesce(quick_event.type, 'lesson') = 'lesson' then 2 else 0 end),
      coalesce(p_capacity_unit, 'people'),
      p_is_visible, p_is_public, p_is_locked, p_enable_notes,
      coalesce(p_description, ''), coalesce(p_summary, ''), coalesce(p_files_legacy, '')
    )
    returning * into created_instance;

    with roots as (
      insert into event_instance_registration (
        instance_id, person_id, couple_id, source, status
      )
      select created_instance.id, registration.person_id, registration.couple_id,
        'manager',
        case when registration.person_id is not null then 'unknown'::attendance_type end
      from unnest(quick_event.registrations) registration
      returning id, couple_id
    )
    insert into event_instance_registration (
      instance_id, parent_registration_id, person_id, status
    )
    select created_instance.id, root.id, person.person_id, 'unknown'
    from roots root
    join couple couple on couple.id = root.couple_id
    cross join lateral unnest(array[couple.man_id, couple.woman_id]) person(person_id)
    where root.couple_id is not null;

    insert into event_instance_trainer (instance_id, person_id, lessons_offered)
    select distinct on (trainer.person_id)
      created_instance.id,
      trainer.person_id,
      case when p_trainer_lessons_offered is null then 0
        else p_trainer_lessons_offered[trainer.ordinality] end
    from unnest(quick_event.trainer_person_ids) with ordinality
      trainer(person_id, ordinality)
    where trainer.person_id is not null;

    insert into event_instance_target_cohort (tenant_id, instance_id, cohort_id)
    select distinct created_instance.tenant_id, created_instance.id, cohort_id
    from unnest(coalesce(p_cohort_ids, '{}'::bigint[])) cohort(cohort_id)
    where cohort_id is not null;

    return next created_instance;
  end loop;
end;
$$;

select verify_function('quick_create_event_instances');
grant execute on function quick_create_event_instances to anonymous;
--! EndIncluded functions/quick_create_event_instances.sql
--! Included functions/scoreboard.sql
drop view if exists scoreboard;
drop function if exists scoreboard_entries;

create or replace function scoreboard_entries(
  since date,
  until date,
  cohort_id bigint default null
)
returns setof scoreboard_record
language sql
stable
as $$
  with membership as (
    select cm.person_id, cm.cohort_id
    from current_cohort_membership cm
    where (cm.cohort_id = scoreboard_entries.cohort_id or scoreboard_entries.cohort_id is null)
  ),
  instances as (
    select
      i.id as instance_id,
      i.since,
      i.until,
      i.type as event_type
    from event_instance i
    where not i.is_cancelled
      and i.since >= scoreboard_entries.since::timestamptz
      and i.until  < scoreboard_entries.until::timestamptz
      and i.type <> 'reservation'
  ),
  member_people as (
    select distinct person_id
    from membership
  ),
  attendance as materialized (
    select
      ea.person_id,
      scoreboard_entries.cohort_id as cohort_id,
      case when inst.event_type = 'lesson' then 1 else 0 end as lesson_score,
      case when inst.event_type = 'group' then floor((extract(epoch from (inst.until - inst.since)) / 60)::numeric / 45::numeric) else 0 end as group_score,
      case when inst.event_type = 'camp'  then 3 + 2 * ((extract(epoch from (inst.until - inst.since)) > 86400)::int) else 0 end as event_score,
      date_trunc('day', inst.since)::date as day
    from instances inst
    join event_instance_registration ea
      on ea.instance_id = inst.instance_id and ea.person_id is not null
    left join event_instance_registration registration
      on registration.id = ea.parent_registration_id
    left join cohort tc
      on tc.id = coalesce(ea.target_cohort_id, registration.target_cohort_id)
    where
      ea.registration_status = 'active'
      and (ea.status = 'attended' or inst.event_type = 'lesson')
      and ea.person_id = any (select person_id from member_people)
      and (scoreboard_entries.cohort_id is null or tc.id = scoreboard_entries.cohort_id)
  ),
  per_day as (
    select
      person_id,
      cohort_id,
      least(sum(lesson_score), 4) as lesson_score,
      sum(group_score) as group_score,
      sum(event_score) as event_score
    from attendance
    group by person_id, cohort_id, day
  ),
  aggregated as (
    select
      person_id,
      cohort_id,
      coalesce(sum(lesson_score), 0)::bigint as lesson_total_score,
      coalesce(sum(group_score), 0)::bigint as group_total_score,
      coalesce(sum(event_score), 0)::bigint as event_total_score
    from per_day
    group by person_id, cohort_id
  ),
  manual as (
    select
      sma.person_id,
      case
        when scoreboard_entries.cohort_id is null then null
        else coalesce(sma.cohort_id, scoreboard_entries.cohort_id)
      end as cohort_id,
      sum(sma.points)::bigint as manual_total_score
    from scoreboard_manual_adjustment sma
    where sma.tenant_id = current_tenant_id()
      and sma.awarded_at >= scoreboard_entries.since
      and sma.awarded_at < scoreboard_entries.until
      and (
        scoreboard_entries.cohort_id is null or
        sma.cohort_id is null or
        sma.cohort_id = scoreboard_entries.cohort_id
      )
      and exists (
        select 1
        from membership mem
        where mem.person_id = sma.person_id
          and (
            (scoreboard_entries.cohort_id is null and sma.cohort_id is null)
            or mem.cohort_id is not distinct from coalesce(sma.cohort_id, scoreboard_entries.cohort_id)
          )
      )
    group by 1, 2
  ),
  totals as (
    select
      coalesce(a.person_id, m.person_id) as person_id,
      coalesce(a.cohort_id, m.cohort_id) as cohort_id,
      coalesce(a.lesson_total_score, 0)::bigint as lesson_total_score,
      coalesce(a.group_total_score, 0)::bigint as group_total_score,
      coalesce(a.event_total_score, 0)::bigint as event_total_score,
      coalesce(m.manual_total_score, 0)::bigint as manual_total_score,
      (
        coalesce(a.lesson_total_score, 0)::bigint +
        coalesce(a.group_total_score, 0)::bigint +
        coalesce(a.event_total_score, 0)::bigint +
        coalesce(m.manual_total_score, 0)::bigint
      ) as total_score
    from aggregated a
    full join manual m using (person_id, cohort_id)
  )
  select
    person_id,
    cohort_id,
    lesson_total_score,
    group_total_score,
    event_total_score,
    manual_total_score,
    total_score,
    rank() over (order by total_score desc, person_id) as ranking
  from totals
  order by total_score desc, person_id;
$$;

comment on function scoreboard_entries is '@simpleCollections only';

grant all on function scoreboard_entries to anonymous;
--! EndIncluded functions/scoreboard.sql
--! Included functions/trainer_group_attendance_completion.sql
create or replace function public.trainer_group_attendance_completion(
  since timestamp with time zone default null,
  until timestamp with time zone default null
) returns setof public.trainer_group_attendance_completion
  language sql stable as $_$
  with filtered_instances as (
    select ei.id
    from event_instance ei
    where ei.tenant_id = current_tenant_id()
      and not ei.is_cancelled
      and ei.type = 'group'
      and coalesce(ei.until, ei.since) < coalesce($2, now())
      and ($1 is null or coalesce(ei.since, ei.until) >= $1)
      and ($2 is null or coalesce(ei.until, ei.since) < $2)
  ),
  trainer_instances as (
    select effective_trainer.person_id, fi.id as instance_id
    from filtered_instances fi
    join event_instance instance on instance.id = fi.id
    cross join lateral app_private.event_instance_trainers_at(
      instance,
      instance.since
    ) effective_trainer
  ),
  attendance_stats as (
    select
      ti.person_id,
      ti.instance_id,
      coalesce(stats.attendance_count, 0) as attendance_count,
      coalesce(stats.unknown_count, 0) as unknown_count
    from trainer_instances ti
    left join lateral (
      select
        count(*) as attendance_count,
        count(*) filter (where eir.status = 'unknown') as unknown_count
      from event_instance_registration eir
      where eir.instance_id = ti.instance_id
        and eir.person_id is not null
        and eir.registration_status = 'active'
    ) stats on true
  ),
  per_trainer as (
    select
      person_id,
      count(*) as total_instances,
      count(*) filter (where attendance_count > 0 and unknown_count = 0) as filled_instances,
      count(*) filter (
        where attendance_count > 0
          and unknown_count > 0
          and unknown_count < attendance_count
      ) as partially_filled_instances,
      count(*) filter (where attendance_count = 0 or unknown_count = attendance_count) as unfilled_instances,
      coalesce(sum(attendance_count), 0) as total_attendances,
      coalesce(sum(unknown_count), 0) as pending_attendances
    from attendance_stats
    group by person_id
  )
  select
    person_id,
    total_instances,
    filled_instances,
    partially_filled_instances,
    unfilled_instances,
    case
      when total_instances > 0 then (filled_instances + partially_filled_instances)::double precision / total_instances
      else null
    end as filled_ratio,
    total_attendances,
    pending_attendances
  from per_trainer
  order by filled_ratio asc nulls last, person_id;
$_$;

comment on function public.trainer_group_attendance_completion(timestamp with time zone, timestamp with time zone) is '@simpleCollections only';
grant all on function public.trainer_group_attendance_completion(timestamp with time zone, timestamp with time zone) to anonymous;
--! EndIncluded functions/trainer_group_attendance_completion.sql
--! Included functions/update_event_instance_details.sql
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[]
);
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[], public.quick_event_registration_input[]
);
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[], public.quick_event_registration_input[],
  integer, public.event_capacity_unit, boolean
);
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[], public.quick_event_registration_input[],
  integer, public.event_capacity_unit, boolean, integer[]
);
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[], public.quick_event_registration_input[],
  integer, public.event_capacity_unit, boolean, integer[], bigint[]
);
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[], public.quick_event_registration_input[],
  integer, public.event_capacity_unit, boolean, integer[], bigint[], boolean
);

create or replace function public.update_event_instance_details(
  p_instance_id bigint,
  p_since timestamptz,
  p_until timestamptz,
  p_name text,
  p_type public.event_type,
  p_location_id bigint,
  p_location_text text,
  p_is_visible boolean,
  p_is_public boolean,
  p_is_cancelled boolean,
  p_trainer_person_ids bigint[] default null,
  p_registrations public.quick_event_registration_input[] default null,
  p_capacity integer default null,
  p_capacity_unit public.event_capacity_unit default null,
  p_is_locked boolean default null,
  p_trainer_lessons_offered integer[] default null,
  p_cohort_ids bigint[] default null,
  p_enable_notes boolean default null,
  p_copies public.quick_event_input[] default null
) returns public.event_instance
  language plpgsql
as $$
declare
  updated_instance public.event_instance;
  v_series_id bigint;
begin
  if p_registrations is not null then
    perform registration.id
    from public.event_instance_registration registration
    where registration.instance_id = p_instance_id
    order by registration.id
    for update;
  end if;

  update public.event_instance
  set
    since = p_since,
    until = p_until,
    name = p_name,
    type = p_type,
    location_id = p_location_id,
    location_text = coalesce(p_location_text, ''),
    is_visible = coalesce(p_is_visible, is_visible),
    is_public = coalesce(p_is_public, is_public),
    capacity = coalesce(p_capacity, capacity),
    capacity_unit = coalesce(p_capacity_unit, capacity_unit),
    is_locked = coalesce(p_is_locked, is_locked),
    enable_notes = coalesce(p_enable_notes, enable_notes),
    is_cancelled = p_is_cancelled
  where id = p_instance_id
  returning * into updated_instance;

  if not found then
    raise exception 'event instance % not found', p_instance_id;
  end if;

  if p_registrations is not null then
    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(p_registrations) registration
    ), roots as (
      select existing.id
      from public.event_instance_registration existing
      where existing.instance_id = p_instance_id
        and existing.parent_registration_id is null
        and not exists (
          select 1
          from desired
          where desired.person_id is not distinct from existing.person_id
            and desired.couple_id is not distinct from existing.couple_id
        )
    )
    update public.event_instance_registration registration
    set registration_status = 'cancelled',
        target_cohort_id = null,
        source = case when registration.id = roots.id
          then 'manager'::public.event_registration_source end
    from roots
    where registration.registration_status <> 'cancelled'
      and (
        registration.id = roots.id
        or registration.parent_registration_id = roots.id
      );

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(p_registrations) registration
    ), roots as (
      select existing.id
      from public.event_instance_registration existing
      join desired
        on desired.person_id is not distinct from existing.person_id
        and desired.couple_id is not distinct from existing.couple_id
      where existing.instance_id = p_instance_id
        and existing.parent_registration_id is null
    )
    update public.event_instance_registration registration
    set registration_status = 'active',
        target_cohort_id = null,
        source = case when registration.id = roots.id
          then 'manager'::public.event_registration_source end
    from roots
    where registration.registration_status <> 'active'
      and (
        registration.id = roots.id
        or registration.parent_registration_id = roots.id
      );

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(p_registrations) registration
    ), roots as (
      insert into public.event_instance_registration (
        instance_id, person_id, couple_id, source, status
      )
      select
        p_instance_id,
        desired.person_id,
        desired.couple_id,
        'manager',
        case when desired.person_id is not null then 'unknown'::public.attendance_type end
      from desired
      where not exists (
        select 1
        from public.event_instance_registration existing
        where existing.instance_id = p_instance_id
          and existing.parent_registration_id is null
          and existing.person_id is not distinct from desired.person_id
          and existing.couple_id is not distinct from desired.couple_id
      )
      returning id, couple_id
    )
    insert into public.event_instance_registration (
      instance_id, parent_registration_id, person_id, status
    )
    select p_instance_id, roots.id, person.person_id, 'unknown'
    from roots
    join public.couple couple on couple.id = roots.couple_id
    cross join lateral unnest(array[couple.man_id, couple.woman_id]) person(person_id);
  end if;

  if p_cohort_ids is not null then
    insert into public.event_instance_target_cohort (tenant_id, instance_id, cohort_id)
    select distinct updated_instance.tenant_id, p_instance_id, desired.cohort_id
    from unnest(p_cohort_ids) desired(cohort_id)
    where desired.cohort_id is not null
    on conflict (instance_id, cohort_id) do nothing;

    delete from public.event_instance_target_cohort target
    where target.instance_id = p_instance_id
      and not exists (
        select 1
        from unnest(p_cohort_ids) desired(cohort_id)
        where desired.cohort_id = target.cohort_id
      );
  end if;

  if p_trainer_person_ids is not null then
    if p_trainer_lessons_offered is not null
      and cardinality(p_trainer_lessons_offered) <> cardinality(p_trainer_person_ids) then
      raise exception 'trainer lesson offers must match trainers';
    end if;

    delete from public.event_instance_trainer
    where instance_id = p_instance_id
      and not exists (
        select 1 from unnest(p_trainer_person_ids) person(id)
        where person.id = event_instance_trainer.person_id
      );

    insert into public.event_instance_trainer (instance_id, person_id, lessons_offered)
    select distinct on (input.person_id)
      p_instance_id,
      input.person_id,
      case when p_trainer_lessons_offered is null then 0
        else input.lessons_offered end
    from (
      select p_trainer_person_ids[i] person_id,
        p_trainer_lessons_offered[i] lessons_offered
      from generate_subscripts(p_trainer_person_ids, 1) item(i)
    ) input
    where input.person_id is not null
    order by input.person_id
    on conflict (instance_id, person_id) do update
    set lessons_offered = case when p_trainer_lessons_offered is null
      then event_instance_trainer.lessons_offered
      else excluded.lessons_offered end;
  end if;

  if p_registrations is not null or p_cohort_ids is not null then
    select * into updated_instance
    from public.event_instance
    where id = p_instance_id;
  end if;

  if cardinality(coalesce(p_copies, '{}'::public.quick_event_input[])) > 0 then
    if updated_instance.series_id is not null then
      raise exception 'event instance % already belongs to a series', p_instance_id;
    end if;

    insert into public.event_series (name)
    values (updated_instance.name)
    returning id into v_series_id;

    update public.event_instance
    set series_id = v_series_id
    where id = p_instance_id
    returning * into updated_instance;

    perform public.quick_create_event_instances(
      events => p_copies,
      parent_id => updated_instance.parent_id,
      p_is_visible => updated_instance.is_visible,
      p_is_public => updated_instance.is_public,
      p_is_locked => updated_instance.is_locked,
      p_enable_notes => updated_instance.enable_notes,
      p_series_id => v_series_id,
      p_name => updated_instance.name,
      p_capacity => updated_instance.capacity,
      p_capacity_unit => updated_instance.capacity_unit,
      p_description => updated_instance.description,
      p_summary => updated_instance.summary,
      p_files_legacy => updated_instance.files_legacy,
      p_cohort_ids => p_cohort_ids,
      p_trainer_lessons_offered => p_trainer_lessons_offered
    );
  end if;

  return updated_instance;
end;
$$;

select verify_function('public.update_event_instance_details');
grant all on function public.update_event_instance_details to anonymous;
--! EndIncluded functions/update_event_instance_details.sql
--! Included functions/set_event_instance_registration.sql
drop function if exists public.set_event_instance_registration(bigint, bigint, bigint, boolean);
drop function if exists public.set_event_instance_registration(bigint, bigint, bigint, boolean, text);

create or replace function public.set_event_instance_registration(
  p_instance_id bigint,
  p_person_id bigint,
  p_couple_id bigint,
  p_is_registered boolean,
  p_note text default null,
  p_lesson_trainer_ids bigint[] default null,
  p_lesson_counts integer[] default null
) returns public.event_instance_registration
  language plpgsql
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
declare
  target_instance event_instance;
  registration event_instance_registration;
  registration_found boolean;
  required_capacity integer;
  remaining_capacity integer;
  lesson_demand record;
begin
  if p_is_registered is null or num_nonnulls(p_person_id, p_couple_id) <> 1 then
    raise exception 'INVALID_REGISTRANT' using errcode = '22023';
  end if;

  if (p_person_id is not null
      and p_person_id <> all(coalesce(current_person_ids(), '{}'::bigint[])))
    or (p_couple_id is not null
      and p_couple_id <> all(coalesce(current_couple_ids(), '{}'::bigint[]))) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  if num_nonnulls(p_lesson_trainer_ids, p_lesson_counts) = 1
    or cardinality(p_lesson_trainer_ids) <> cardinality(p_lesson_counts)
    or exists (
      select 1
      from unnest(p_lesson_trainer_ids, p_lesson_counts) demand(trainer_id, lesson_count)
      where demand.trainer_id is null
        or demand.lesson_count is null
        or demand.lesson_count < 0
    ) then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;
  if not p_is_registered and p_lesson_trainer_ids is not null then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;

  select * into target_instance
  from event_instance
  where id = p_instance_id
    and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;
  if target_instance.is_locked then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  select * into registration
  from event_instance_registration
  where instance_id = p_instance_id
    and parent_registration_id is null
    and person_id is not distinct from p_person_id
    and couple_id is not distinct from p_couple_id;
  registration_found := found;

  if not p_is_registered then
    if not registration_found then
      raise exception 'REGISTRATION_NOT_FOUND' using errcode = '22023';
    end if;

    update event_instance_registration
    set registration_status = 'cancelled',
        target_cohort_id = null,
        source = case when id = registration.id
          then 'self'::event_registration_source end
    where id = registration.id or parent_registration_id = registration.id;

    select * into registration from event_instance_registration where id = registration.id;
    return registration;
  end if;

  if registration_found and registration.registration_status = 'active' then
    if p_note is not null then
      update event_instance_registration set note = p_note where id = registration.id
      returning * into registration;
    end if;
  else
    if target_instance.is_cancelled
      or target_instance.until <= now()
      or not (
        coalesce(target_instance.is_public, false)
        or (
          coalesce(target_instance.is_visible, false)
          and current_tenant_id() = any(my_tenants_array())
        )
      ) then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;

    remaining_capacity := event_instance_remaining_person_spots(target_instance);
    required_capacity := case
      when target_instance.capacity_unit = 'people' and p_couple_id is not null then 2
      else 1
    end;
    if remaining_capacity is not null and remaining_capacity < required_capacity then
      raise exception 'CAPACITY_EXCEEDED' using errcode = '22023';
    end if;

    if registration_found then
      update event_instance_registration
      set registration_status = 'active',
          target_cohort_id = null,
          source = case when id = registration.id
            then 'self'::event_registration_source end
      where id = registration.id or parent_registration_id = registration.id;

      if p_note is not null then
        update event_instance_registration set note = p_note where id = registration.id;
      end if;
    else
      insert into event_instance_registration (
        instance_id, person_id, couple_id, source, status, note
      ) values (
        p_instance_id, p_person_id, p_couple_id, 'self',
        case when p_person_id is not null then 'unknown'::attendance_type end,
        p_note
      ) returning * into registration;

      insert into event_instance_registration (
        instance_id, parent_registration_id, person_id, status
      )
      select p_instance_id, registration.id, person.person_id, 'unknown'
      from couple
      cross join lateral unnest(array[couple.man_id, couple.woman_id]) person(person_id)
      where couple.id = p_couple_id;
    end if;
  end if;

  select * into registration from event_instance_registration where id = registration.id;

  if p_lesson_trainer_ids is not null then
    delete from event_lesson_demand
    where registration_id = registration.id
      and trainer_id <> all(p_lesson_trainer_ids);

    for lesson_demand in
      select demand.trainer_id, demand.lesson_count
      from unnest(p_lesson_trainer_ids, p_lesson_counts)
        demand(trainer_id, lesson_count)
      order by demand.trainer_id
    loop
      perform set_lesson_demand(
        registration.id,
        lesson_demand.trainer_id,
        lesson_demand.lesson_count
      );
    end loop;
  end if;

  return registration;
end;
$$;

select verify_function('public.set_event_instance_registration');
grant execute on function public.set_event_instance_registration(
  bigint, bigint, bigint, boolean, text, bigint[], integer[]
)
  to anonymous;
--! EndIncluded functions/set_event_instance_registration.sql
