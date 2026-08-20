--! Previous: sha1:b499247341eac6e73485b0eacc895be99fe08453
--! Hash: sha1:ec2e1175abf5b4b80a2875abd9c4c27f9b474fd7

--! split: 1-current.sql
do $$
begin
  if not exists (
    select 1
    from pg_catalog.pg_type type
    join pg_catalog.pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'event_registration_input'
  ) then
    create type public.event_registration_input as (person_id bigint, couple_id bigint);
  end if;

  if not exists (
    select 1
    from pg_catalog.pg_type type
    join pg_catalog.pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'event_trainer_input'
  ) then
    create type public.event_trainer_input as (person_id bigint, lessons_offered integer);
  end if;

  if not exists (
    select 1
    from pg_catalog.pg_type type
    join pg_catalog.pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'event_details_input'
  ) then
    create type public.event_details_input as (
      parent_id bigint,
      name text,
      type public.event_type,
      location_id bigint,
      location_text text,
      capacity integer,
      capacity_unit public.event_capacity_unit,
      is_visible boolean,
      is_public boolean,
      has_public_details boolean,
      is_locked boolean,
      enable_notes boolean
    );
  end if;

  if not exists (
    select 1
    from pg_catalog.pg_type type
    join pg_catalog.pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'event_input'
  ) then
    create type public.event_input as (
      id bigint,
      since timestamptz,
      until timestamptz,
      is_cancelled boolean,
      registrations public.event_registration_input[]
    );
  end if;

  if not exists (
    select 1
    from pg_catalog.pg_type type
    join pg_catalog.pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'event_series_input'
  ) then
    create type public.event_series_input as (id bigint, name text);
  end if;
end
$$;

--! Included functions/event_overlaps_reports.sql
drop function if exists event_overlaps_attendee_report;
drop function if exists event_overlaps_trainer_report;
drop type if exists event_overlaps_conflict;
drop type if exists event_conflict;

create type event_conflict as (
  person_id bigint,
  person_name text,
  first_instance_id bigint,
  first_event_name text,
  first_since timestamptz,
  first_until timestamptz,
  second_instance_id bigint,
  second_event_name text,
  second_since timestamptz,
  second_until timestamptz,
  overlap_range tstzrange
);

comment on type event_conflict is E'
@foreignKey (person_id) references person (id)
@foreignKey (first_instance_id) references event_instance (id)
@foreignKey (second_instance_id) references event_instance (id)
';
comment on column event_conflict.person_id is '@notNull';
comment on column event_conflict.first_instance_id is '@notNull';
comment on column event_conflict.second_instance_id is '@notNull';
comment on column event_conflict.overlap_range is '@notNull';

create or replace function event_overlaps_attendee_report(p_since timestamptz, p_until timestamptz)
  returns setof event_conflict
  language sql stable
as $$
  with instances as (
    select
      ea.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.parent_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from event_instance_registration ea
    join event_instance ei on ei.id = ea.instance_id
    join person p on p.id = ea.person_id
    where not ei.is_cancelled
      and ea.person_id is not null
      and ea.registration_status = 'active'
      and ei.range && tstzrange(coalesce(p_since, '-infinity'::timestamptz), coalesce(p_until, 'infinity'::timestamptz), '[]')
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
    tstzrange(greatest(i1.since, i2.since), least(i1.until, i2.until), '[]') as overlap_range
  from instances i1
  join instances i2 on i1.person_id = i2.person_id
    and i1.instance_id < i2.instance_id
    and i1.parent_id is distinct from i2.instance_id
    and i2.parent_id is distinct from i1.instance_id
    and i1.range && i2.range
    and greatest(i1.since, i2.since) < least(i1.until, i2.until);
$$;

comment on function event_overlaps_attendee_report is '@simpleCollections only';
grant all on function event_overlaps_attendee_report to anonymous;

create or replace function event_overlaps_trainer_report(p_since timestamptz, p_until timestamptz)
  returns setof event_conflict
  language sql stable
as $$
  with trainer_instances as (
    select
      eit.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.parent_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from event_instance ei
    join event_instance_trainer eit on ei.id = eit.instance_id
    join person p on p.id = eit.person_id
    where not ei.is_cancelled
      and ei.range && tstzrange(coalesce(p_since, '-infinity'::timestamptz), coalesce(p_until, 'infinity'::timestamptz), '[]')
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
    tstzrange(greatest(ti1.since, ti2.since), least(ti1.until, ti2.until), '[]') as overlap_range
  from trainer_instances ti1
  join trainer_instances ti2 on ti1.person_id = ti2.person_id
    and ti1.instance_id < ti2.instance_id
    and ti1.parent_id is distinct from ti2.instance_id
    and ti2.parent_id is distinct from ti1.instance_id
    and ti1.range && ti2.range
    and greatest(ti1.since, ti2.since) < least(ti1.until, ti2.until);
$$;

comment on function event_overlaps_trainer_report is '@simpleCollections only';
grant all on function event_overlaps_trainer_report to anonymous;
--! EndIncluded functions/event_overlaps_reports.sql
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

comment on function activity_timeline is '@behavior +queryField:resource:list -queryField:resource:connection';
grant all on function activity_timeline to anonymous;
--! EndIncluded functions/activity_timeline.sql
--! Included functions/event_instances_for_range.sql
do $$
begin
  create type public.event_instance_range_scope as enum ('all', 'top_level', 'mine', 'relevant');
exception when duplicate_object then null;
end
$$;

drop function if exists event_instances_for_range;

create or replace function event_instances_for_range(
  only_type event_type,
  start_range timestamptz,
  end_range timestamptz = null,
  trainer_ids bigint[] = null,
  participant_ids bigint[] = null,
  only_mine boolean = false,
  any_parent boolean = true,
  parent_id bigint = null,
  scope event_instance_range_scope = null
) returns setof event_instance as $$
  with mine as (
    select instance_id from event_instance_registration
    where person_id = any (current_person_ids()) and registration_status = 'active'
    union all
    select instance_id from event_instance_trainer
    where person_id = any (current_person_ids())
  )
  select i.*
  from event_instance i
  cross join (values (coalesce($9, case
    when only_mine then 'mine'
    when not any_parent then 'top_level'
    else 'all'
  end::event_instance_range_scope))) args(scope)
  where i.tenant_id = current_tenant_id()
    and (only_type is null or i.type = only_type)
    and case
      when $8 is not null then i.parent_id = $8
        and (args.scope <> 'mine' or i.id in (select instance_id from mine))
      when args.scope = 'all' then true
      when args.scope = 'top_level' then i.parent_id is null
      when args.scope = 'mine' then i.id in (select instance_id from mine)
      when args.scope = 'relevant' then i.parent_id is null
        or i.id in (select instance_id from mine)
        or i.parent_id in (select instance_id from mine)
    end
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any (participant_ids) and registration_status = 'active'))
  ;
$$ stable language sql;

COMMENT ON FUNCTION event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION event_instances_for_range TO anonymous;
--! EndIncluded functions/event_instances_for_range.sql
--! Included functions/event_instance_registration_info.sql
drop function if exists event_instance_registration_info;
drop type if exists event_instance_registration_info;

create type event_instance_registration_info as (
  registrations integer,
  people integer,
  remaining_capacity integer,
  my boolean
);

create or replace function event_instance_registration_info(inst event_instance)
  returns event_instance_registration_info
  language sql stable security definer
  set search_path = public, pg_catalog, pg_temp
as $$
  with registration_counts as (
    select
      count(*) filter (where parent_registration_id is null)::integer
        as registrations,
      count(*) filter (where person_id is not null)::integer
        as people,
      count(*) filter (where person_id = any(current_person_ids())) > 0 as my
    from event_instance_registration
    where instance_id = inst.id and registration_status = 'active'
  ), external_counts as (
    select count(*)::integer as registrations
    from event_external_registration
    where instance_id = inst.id
  )
  select row(
    registrations.registrations + external.registrations,
    registrations.people + external.registrations,
    case
      when inst.capacity is null or inst.capacity <= 0 then null
      else inst.capacity
        - case inst.capacity_unit
            when 'people' then registrations.people
            when 'registrations' then registrations.registrations
          end
        - external.registrations
    end,
    registrations.my
  )::event_instance_registration_info
  from registration_counts registrations
  cross join external_counts external;
$$;

grant all on function event_instance_registration_info to anonymous;
--! EndIncluded functions/event_instance_registration_info.sql
--! Included functions/save_events.sql
create or replace function save_events(
  details event_details_input,
  events event_input[],
  trainers event_trainer_input[] default '{}'::event_trainer_input[],
  cohort_ids bigint[] default '{}'::bigint[],
  series event_series_input default null
) returns setof event_instance
  language plpgsql
as $$
declare
  event_to_save event_input;
  v_saved_event event_instance;
  v_saved_event_ids bigint[] := '{}'::bigint[];
  v_tenant_id bigint := current_tenant_id();
  v_series_id bigint;
  v_assign_series boolean := (series).id is not null or (series).name is not null;
  v_is_visible boolean := coalesce((details).is_visible, true);
  v_is_public boolean := coalesce((details).is_public, false);
  v_has_public_details boolean := v_is_public and coalesce((details).has_public_details, false);
  v_is_locked boolean := coalesce((details).is_locked, false);
  v_enable_notes boolean := coalesce((details).enable_notes, false);
  v_expected_existing_count bigint;
  v_locked_existing_count bigint;
begin
  if details is null then
    raise exception 'event details are required';
  end if;

  if (details).type is null
    or (details).capacity is null
    or (details).capacity < 0
    or (details).capacity_unit is null then
    raise exception 'event details are incomplete';
  end if;

  if cardinality(coalesce(events, '{}'::event_input[])) = 0 then
    raise exception 'at least one event is required';
  end if;

  if exists (
    select 1 from unnest(events) i
    where i is null or i.since is null or i.until is null or i.until <= i.since
  ) then
    raise exception 'every event requires a valid time range';
  end if;

  if exists (select i.id from unnest(events) i where i.id is not null group by i.id having count(*) > 1) then
    raise exception 'an event may only be submitted once';
  end if;

  if exists (
    select 1
    from unnest(events) i
    cross join lateral unnest(coalesce(i.registrations, '{}'::event_registration_input[])) registration
    where (registration.person_id is null) = (registration.couple_id is null)
  ) then
    raise exception 'an event registration requires exactly one person or couple';
  end if;

  if exists (
    select 1
    from unnest(coalesce(trainers, '{}'::event_trainer_input[])) trainer
    where trainer.person_id is null or trainer.lessons_offered < 0
  ) then
    raise exception 'an event trainer requires a person and a non-negative lesson limit';
  end if;

  if exists (
    select 1
    from unnest(coalesce(cohort_ids, '{}'::bigint[])) i(cohort_id)
    left join cohort cohort on cohort.id = i.cohort_id and cohort.tenant_id = v_tenant_id
    where i.cohort_id is not null and cohort.id is null
  ) then
    raise exception 'event cohort not found';
  end if;

  if exists (
    select 1 from unnest(events) i where i.id is null
  ) and (details).parent_id is not null and not exists (
    select 1
    from event_instance parent
    where parent.id = (details).parent_id and parent.tenant_id = v_tenant_id
  ) then
    raise exception 'event parent % not found or not editable', (details).parent_id;
  end if;

  if v_assign_series then
    if (series).id is null then
      insert into event_series (name)
      values (coalesce((series).name, (details).name))
      returning id into v_series_id;
    else
      select e.id into v_series_id
      from event_series e where e.id = (series).id and e.tenant_id = v_tenant_id
      for update;

      if not found then
        raise exception 'event series % not found or not editable', (series).id;
      end if;
    end if;
  end if;

  select count(*) into v_expected_existing_count from unnest(events) i where i.id is not null;

  perform e.id
  from event_instance e
  join unnest(events) i on i.id = e.id
  where i.id is not null and e.tenant_id = v_tenant_id
  order by e.id
  for update of e;

  get diagnostics v_locked_existing_count = row_count;
  if v_locked_existing_count <> v_expected_existing_count then
    raise exception 'one or more events were not found or are not editable';
  end if;

  foreach event_to_save in array events loop
    if event_to_save.id is null then
      insert into event_instance (
        parent_id,
        series_id,
        since,
        until,
        is_cancelled,
        name,
        type,
        location_id,
        location_text,
        capacity,
        capacity_unit,
        is_visible,
        is_public,
        has_public_details,
        is_locked,
        enable_notes,
        description,
        summary,
        files_legacy
      ) values (
        (details).parent_id,
        v_series_id,
        event_to_save.since,
        event_to_save.until,
        coalesce(event_to_save.is_cancelled, false),
        (details).name,
        (details).type,
        (details).location_id,
        coalesce((details).location_text, ''),
        (details).capacity,
        (details).capacity_unit,
        v_is_visible,
        v_is_public,
        v_has_public_details,
        v_is_locked,
        v_enable_notes,
        '',
        '',
        ''
      )
      returning * into v_saved_event;
    else
      update event_instance e
      set since = event_to_save.since,
          until = event_to_save.until,
          is_cancelled = coalesce(event_to_save.is_cancelled, false),
          name = (details).name,
          type = (details).type,
          location_id = (details).location_id,
          location_text = coalesce((details).location_text, ''),
          capacity = (details).capacity,
          capacity_unit = (details).capacity_unit,
          is_visible = v_is_visible,
          is_public = v_is_public,
          has_public_details = v_has_public_details,
          is_locked = v_is_locked,
          enable_notes = v_enable_notes,
          series_id = case
            when v_assign_series then v_series_id
            else e.series_id
          end
      where e.id = event_to_save.id and e.tenant_id = v_tenant_id
      returning * into v_saved_event;

      if not found then
        raise exception 'event % not found or not editable', event_to_save.id;
      end if;
    end if;

    v_saved_event_ids := array_append(v_saved_event_ids, v_saved_event.id);

    perform registration.id
    from event_instance_registration registration
    where registration.instance_id = v_saved_event.id
    order by registration.id
    for update;

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(coalesce(event_to_save.registrations, '{}'::event_registration_input[])) registration
    ), roots as (
      select e.id
      from event_instance_registration e
      where e.instance_id = v_saved_event.id
        and e.parent_registration_id is null
        and not exists (
          select 1 from desired
          where desired.person_id is not distinct from e.person_id
            and desired.couple_id is not distinct from e.couple_id
        )
    )
    update event_instance_registration registration
    set registration_status = 'cancelled',
        target_cohort_id = null,
        source = case when registration.id = roots.id
          then 'manager'::event_registration_source end
    from roots
    where registration.registration_status <> 'cancelled'
      and (registration.id = roots.id or registration.parent_registration_id = roots.id);

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(
        coalesce(event_to_save.registrations, '{}'::event_registration_input[])
      ) registration
    ), roots as (
      select e.id
      from event_instance_registration e
      join desired
        on desired.person_id is not distinct from e.person_id
        and desired.couple_id is not distinct from e.couple_id
      where e.instance_id = v_saved_event.id
        and e.parent_registration_id is null
    )
    update event_instance_registration registration
    set registration_status = 'active',
        target_cohort_id = null,
        source = case when registration.id = roots.id
          then 'manager'::event_registration_source end
    from roots
    where registration.registration_status <> 'active'
      and (registration.id = roots.id or registration.parent_registration_id = roots.id);

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(
        coalesce(event_to_save.registrations, '{}'::event_registration_input[])
      ) registration
    ), roots as (
      insert into event_instance_registration (
        instance_id, person_id, couple_id, source, status
      )
      select v_saved_event.id,
        desired.person_id,
        desired.couple_id,
        'manager',
        case when desired.person_id is not null
          then 'unknown'::attendance_type end
      from desired
      where not exists (
        select 1
        from event_instance_registration e
        where e.instance_id = v_saved_event.id
          and e.parent_registration_id is null
          and e.person_id is not distinct from desired.person_id
          and e.couple_id is not distinct from desired.couple_id
      )
      returning id, couple_id
    )
    insert into event_instance_registration (instance_id, parent_registration_id, person_id, status)
    select v_saved_event.id, roots.id, person.person_id, 'unknown'
    from roots
    join couple couple on couple.id = roots.couple_id
    cross join lateral unnest(array[couple.man_id, couple.woman_id]) person(person_id);
  end loop;

  delete from event_instance_trainer e
  where e.instance_id = any(v_saved_event_ids)
    and not exists (
      select 1
      from unnest(coalesce(trainers, '{}'::event_trainer_input[])) trainer
      where trainer.person_id = e.person_id
    );

  with desired as (
    select distinct on (trainer.person_id) trainer.person_id, trainer.lessons_offered
    from unnest(coalesce(trainers, '{}'::event_trainer_input[]))
      with ordinality trainer(person_id, lessons_offered, position)
    order by trainer.person_id, trainer.position
  )
  insert into event_instance_trainer (tenant_id, instance_id, person_id, lessons_offered)
  select stored_event.tenant_id, stored_event.id, desired.person_id, desired.lessons_offered
  from event_instance stored_event
  join unnest(v_saved_event_ids) saved(id) on saved.id = stored_event.id
  cross join desired
  on conflict (instance_id, person_id) do update
  set lessons_offered = excluded.lessons_offered;

  with desired as (
    select distinct i.cohort_id
    from unnest(coalesce(cohort_ids, '{}'::bigint[])) i(cohort_id)
    where i.cohort_id is not null
  )
  insert into event_instance_target_cohort (tenant_id, instance_id, cohort_id)
  select stored_event.tenant_id, stored_event.id, desired.cohort_id
  from event_instance stored_event
  join unnest(v_saved_event_ids) saved(id) on saved.id = stored_event.id
  cross join desired
  on conflict (instance_id, cohort_id) do nothing;

  delete from event_instance_target_cohort e
  where e.instance_id = any(v_saved_event_ids)
    and not exists (
      select 1
      from unnest(coalesce(cohort_ids, '{}'::bigint[])) i(cohort_id)
      where i.cohort_id = e.cohort_id
    );

  return query
  select stored_event.*
  from unnest(v_saved_event_ids) with ordinality saved(event_id, position)
  join event_instance stored_event on stored_event.id = saved.event_id
  order by saved.position;
end;
$$;

comment on function save_events is '@simpleCollections only';
grant execute on function save_events to anonymous;
--! EndIncluded functions/save_events.sql

drop function if exists quick_create_event_instances;
drop function if exists update_event_instance_details;
drop type if exists quick_event_input;
drop type if exists quick_event_registration_input;

alter type public.jwt_token drop attribute if exists username;
alter type public.jwt_token drop attribute if exists is_member;
alter type public.jwt_token drop attribute if exists is_trainer;
alter type public.jwt_token drop attribute if exists is_admin;

--! Included functions/create_jwt_token.sql
create or replace function app_private.create_jwt_token(u users) returns jwt_token
    language sql stable
as $$
with
  person_ids as (
    select distinct up.person_id from user_proxy up where up.user_id = u.id
  ),
  tenant_memberships as (
    select distinct tm.tenant_id from tenant_membership tm join person_ids p on p.person_id = tm.person_id where tm.status = 'active'
  ),
  tenant_trainers as (
    select distinct tt.tenant_id from tenant_trainer tt join person_ids p on p.person_id = tt.person_id where tt.status = 'active'
  ),
  tenant_admins as (
    select distinct ta.tenant_id from tenant_administrator ta join person_ids p on p.person_id = ta.person_id where ta.status = 'active'
    union
    select id from tenant where app_private.is_system_admin(u.id)
  ),
  tenant_ids as (
    select tenant_id from tenant_memberships
    union
    select tenant_id from tenant_trainers
    union
    select tenant_id from tenant_admins
  ),
  cohort_ids as (
    select distinct cm.cohort_id from cohort_membership cm join person_ids p on p.person_id = cm.person_id where cm.status = 'active'
  ),
  couple_ids as (
    select distinct c.id from couple c join person_ids p on p.person_id = c.man_id where c.status = 'active'
    union all
    select distinct c.id from couple c join person_ids p on p.person_id = c.woman_id where c.status = 'active'
  )
  select
    extract(epoch from now() + interval '7 days')::integer as exp,
    u.id as user_id,
    (select current_tenant_id()) as tenant_id,
    u.u_email as email,

    coalesce((select array_agg(p.person_id) from person_ids p), '{}'::bigint[]) as my_person_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_ids p), '{}'::bigint[]) as my_tenant_ids,
    coalesce((select array_agg(p.cohort_id) from cohort_ids p), '{}'::bigint[]) as my_cohort_ids,
    coalesce((select array_agg(p.id) from couple_ids p), '{}'::bigint[]) as my_couple_ids,

    app_private.is_system_admin(u.id) as is_system_admin,

    '{}'::bigint[] as guest_tenant_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_memberships p), '{}'::bigint[]) as member_tenant_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_trainers p), '{}'::bigint[]) as trainer_tenant_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_admins p), '{}'::bigint[]) as admin_tenant_ids;
$$;
--! EndIncluded functions/create_jwt_token.sql
--! Included functions/current_claims.sql
create or replace function public.current_claims() returns jsonb
    language sql stable security definer
as $$
  select to_jsonb(app_private.create_jwt_token(users)) - 'exp'
  from users
  where id = nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$;

grant all on function public.current_claims() to anonymous;
--! EndIncluded functions/current_claims.sql
