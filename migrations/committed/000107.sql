--! Previous: sha1:975be5b05d8bf602949c1570d4a6a4f17d2e86e5
--! Hash: sha1:50184d5b63499e7ac945f05c8c5b480259cd3255

--! split: 1-current.sql
drop index if exists public.event_instance_registration_person_key;
create unique index event_instance_registration_person_key
  on public.event_instance_registration (instance_id, person_id)
  where person_id is not null
    and legacy_registration_id is null
    and registration_status = 'active';

drop index if exists public.payment_event_instance_id_idx;
create unique index payment_event_instance_id_idx
  on public.payment (event_instance_id)
  where event_instance_id is not null;

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
      and registration.status <> 'cancelled'
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
    and registration.registration_status = 'active'
    and registration.status <> 'cancelled';

  return created_payment;
end
$$;

select verify_function('create_event_instance_payment');

COMMENT ON FUNCTION create_event_instance_payment IS '@omit';
GRANT ALL ON FUNCTION create_event_instance_payment TO anonymous;
--! EndIncluded functions/create_event_instance_payment.sql

--! Included functions/tg_event_instance__delete_payment_on_cancellation.sql
CREATE or replace FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
declare
  payment_id bigint;
begin
  delete from payment where event_instance_id = OLD.id;

  if not new.is_cancelled then
    select (create_event_instance_payment(i)).id into payment_id
    from event_instance i
    where i.type='lesson'
      and i.id = NEW.id
      and not i.is_cancelled
      and i.since < now()
      and not exists (
        select * from payment where event_instance_id=i.id
      );

    update payment set status ='unpaid' where id = payment_id;
    perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
  end if;

  return OLD;
end;
$$;

select verify_function('app_private.tg_event_instance__delete_payment_on_cancellation', 'event_instance');

DROP TRIGGER IF EXISTS _500_delete_on_cancellation on public.event_instance;

CREATE TRIGGER _500_delete_on_cancellation
  AFTER UPDATE OF is_cancelled ON public.event_instance
  FOR EACH ROW
  WHEN (OLD.is_cancelled IS DISTINCT FROM NEW.is_cancelled)
  EXECUTE FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation();
--! EndIncluded functions/tg_event_instance__delete_payment_on_cancellation.sql

--! Included functions/create_latest_lesson_payments.sql
CREATE or replace FUNCTION app_private.create_latest_lesson_payments() RETURNS SETOF payment
  LANGUAGE plpgsql
AS $$
begin
  perform set_config('jwt.claims.tenant_id', '2', true);
  perform set_config('jwt.claims.my_tenant_ids', '[2]', true);
  if not (select row_security_active('event_instance')) then
    set local role to administrator;
  end if;

  return query WITH created AS (
    SELECT p.*
    FROM event_instance ei
      JOIN LATERAL create_event_instance_payment(ei) p ON true
    WHERE ei.type = 'lesson'
      AND NOT ei.is_cancelled
      AND ei.since < now()
      AND NOT EXISTS (
        SELECT 1
        FROM payment p
        WHERE p.event_instance_id = ei.id AND p.status = 'paid'
      )
      AND p.status in ('unpaid', 'tentative')
  ),
  unpaid AS (
    UPDATE payment p
      SET status = 'unpaid'
      FROM created
      WHERE p.id = created.id
      RETURNING p.*
  )
  SELECT p.*
  FROM unpaid
    CROSS JOIN LATERAL resolve_payment_with_credit(unpaid.*) p
  WHERE p IS NOT NULL;
end;
$$;

select verify_function('app_private.create_latest_lesson_payments');
--! EndIncluded functions/create_latest_lesson_payments.sql

--! Included functions/event_instance_approx_price.sql
create or replace function public.event_instance_approx_price(v_instance event_instance)
  returns table (amount numeric(19,4), currency text)
  language sql stable
as $$
  with stats as (
    select
      (select count(distinct registration.person_id)
       from public.event_instance_registration registration
       where registration.instance_id = v_instance.id
         and registration.person_id is not null
         and registration.registration_status = 'active'
         and registration.status <> 'cancelled')::bigint as num_participants,
      extract(epoch from (v_instance.until - v_instance.since)) / 60.0 as duration
  )
  select
    sum(tt.member_price_45min_amount * s.duration / 45 / s.num_participants) as amount,
    tt.currency as currency
  from stats s
  join lateral public.event_instance_trainers(v_instance) tt on true
  where
    s.num_participants > 0
    and s.duration > 0
    and tt.member_price_45min_amount is not null
    and tt.currency is not null
  group by tt.currency;
$$;

grant all on function event_instance_approx_price to anonymous;
COMMENT ON FUNCTION event_instance_approx_price IS '@simpleCollections only';
--! EndIncluded functions/event_instance_approx_price.sql

--! Included functions/update_event_instance_details.sql
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[]
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
  p_registrations public.quick_event_registration_input[] default null
) returns public.event_instance
  language plpgsql
as $$
declare
  updated_instance public.event_instance;
begin
  if p_registrations is not null then
    perform registration.id
    from public.event_instance_registration registration
    where registration.instance_id = p_instance_id
      and registration.legacy_registration_id is null
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
    is_cancelled = p_is_cancelled
  where id = p_instance_id
  returning * into updated_instance;

  if not found then
    raise exception 'event instance % not found', p_instance_id;
  end if;

  if p_registrations is not null then
    if updated_instance.event_id is not null then
      raise exception 'event-backed instance % registrations must be edited through event_registration', p_instance_id;
    end if;

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(p_registrations) registration
    ), roots as (
      select existing.id
      from public.event_instance_registration existing
      where existing.instance_id = p_instance_id
        and existing.parent_registration_id is null
        and existing.legacy_registration_id is null
        and not exists (
          select 1
          from desired
          where desired.person_id is not distinct from existing.person_id
            and desired.couple_id is not distinct from existing.couple_id
        )
    )
    update public.event_instance_registration registration
    set registration_status = 'cancelled'
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
        and existing.legacy_registration_id is null
    )
    update public.event_instance_registration registration
    set registration_status = 'active'
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
        instance_id, person_id, couple_id, status
      )
      select
        p_instance_id,
        desired.person_id,
        desired.couple_id,
        case when desired.person_id is not null then 'unknown'::public.attendance_type end
      from desired
      where not exists (
        select 1
        from public.event_instance_registration existing
        where existing.instance_id = p_instance_id
          and existing.parent_registration_id is null
          and existing.legacy_registration_id is null
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

  if p_trainer_person_ids is not null then
    delete from public.event_instance_trainer
    where instance_id = p_instance_id;

    insert into public.event_instance_trainer (instance_id, person_id)
    select distinct p_instance_id, input.person_id
    from unnest(p_trainer_person_ids) as input(person_id)
    where input.person_id is not null
    on conflict (instance_id, person_id) do nothing;
  end if;

  if p_registrations is not null then
    select * into updated_instance
    from public.event_instance
    where id = p_instance_id;
  end if;

  return updated_instance;
end;
$$;

select verify_function('public.update_event_instance_details');
grant all on function public.update_event_instance_details to anonymous;
--! EndIncluded functions/update_event_instance_details.sql

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
        and ea.status <> 'cancelled'
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

drop function if exists public.event_registration_last_attended(
  public.event_registration
);

drop function if exists public.update_event_attendance(
  bigint, bigint, public.attendance_type, text
);
drop view if exists public.event_attendance;
