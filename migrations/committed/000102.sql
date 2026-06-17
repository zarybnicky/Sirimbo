--! Previous: sha1:1169ec974719321231773f9e804d8cda503e2516
--! Hash: sha1:d707b4e496e707f569195e6ee1d76902bf8a1184

--! split: 1-current.sql
drop function if exists public.filtered_people(boolean, boolean, bigint[], text);

comment on table public.dokumenty is '@omit';

comment on table public.cohort_membership is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.couple is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.event_instance is '@omit create,delete
@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.payment is '@omit create
@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.payment_debtor is '@omit create,update,delete
@simpleCollections only';

comment on table public.payment_recipient is '@omit create,update,delete
@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.person_invitation is '@omit update
@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.scoreboard_manual_adjustment is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant is '@omit create,delete
@behavior -singularRelation:resource:single -query:resource:list -query:resource:connection
@simpleCollections only';

comment on table public.tenant_administrator is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant_location is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant_membership is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant_settings is '@simpleCollections only
@omit create,delete
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant_trainer is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.user_proxy is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

do $$
begin
  if not exists (
    select 1
    from pg_catalog.pg_type t
    join pg_catalog.pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public'
      and t.typname = 'quick_event_registration_input'
  ) then
    create type public.quick_event_registration_input as (
      person_id bigint,
      couple_id bigint
    );
  end if;

  if not exists (
    select 1
    from pg_catalog.pg_type t
    join pg_catalog.pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public'
      and t.typname = 'quick_event_input'
  ) then
    create type public.quick_event_input as (
      since timestamptz,
      until timestamptz,
      type public.event_type,
      location_id bigint,
      location_text text,
      trainer_person_ids bigint[],
      registrations public.quick_event_registration_input[]
    );
  end if;
end
$$;

create or replace function public.quick_create_events(
  events public.quick_event_input[]
) returns setof public.event
  language plpgsql
as $$
declare
  quick_event public.quick_event_input;
  created_event public.event;
  created_ids bigint[] := array[]::bigint[];
begin
  foreach quick_event in array coalesce(events, '{}'::public.quick_event_input[]) loop
    insert into public.event (
      name,
      description,
      type,
      location_id,
      location_text,
      capacity,
      is_visible
    )
    values (
      '',
      '',
      coalesce(quick_event.type, 'lesson'::public.event_type),
      quick_event.location_id,
      coalesce(quick_event.location_text, ''),
      case when coalesce(quick_event.type, 'lesson'::public.event_type) = 'lesson' then 2 else 0 end,
      true
    )
    returning * into created_event;

    insert into public.event_instance (event_id, since, until, is_cancelled)
    values (created_event.id, quick_event.since, quick_event.until, false);

    insert into public.event_trainer (event_id, person_id, lessons_offered)
    select distinct created_event.id, input.person_id, 0
    from unnest(coalesce(quick_event.trainer_person_ids, '{}'::bigint[])) as input(person_id)
    where input.person_id is not null
    on conflict (event_id, person_id) do nothing;

    insert into public.event_registration (event_id, person_id, couple_id)
    select created_event.id, input.person_id, input.couple_id
    from unnest(
      coalesce(quick_event.registrations, '{}'::public.quick_event_registration_input[])
    ) as input(person_id, couple_id)
    on conflict (event_id, person_id, couple_id) do nothing;

    created_ids := created_ids || created_event.id;
  end loop;

  return query select * from public.event where id = any (created_ids) order by id;
end;
$$;

select verify_function('public.quick_create_events');

comment on function public.quick_create_events is null;
grant all on function public.quick_create_events to anonymous;

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
  p_trainer_person_ids bigint[] default null
) returns public.event_instance language plpgsql
as $$
declare
  updated_instance public.event_instance;
begin
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

  if p_trainer_person_ids is not null then
    delete from public.event_instance_trainer
    where instance_id = p_instance_id;

    insert into public.event_instance_trainer (instance_id, person_id)
    select distinct p_instance_id, input.person_id
    from unnest(p_trainer_person_ids) as input(person_id)
    where input.person_id is not null
    on conflict (instance_id, person_id) do nothing;
  end if;

  return updated_instance;
end;
$$;

select verify_function('public.update_event_instance_details');

comment on function public.update_event_instance_details is null;
grant all on function public.update_event_instance_details to anonymous;

do $$
begin
  if not exists (
    select 1
    from pg_catalog.pg_attribute a
    join pg_catalog.pg_type t on t.typrelid = a.attrelid
    join pg_catalog.pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public'
      and t.typname = 'competition_participation_record'
      and a.attname = 'event_external_id'
      and not a.attisdropped
  ) then
    alter type public.competition_participation_record
      add attribute event_external_id text;
  end if;

  if not exists (
    select 1
    from pg_catalog.pg_attribute a
    join pg_catalog.pg_type t on t.typrelid = a.attrelid
    join pg_catalog.pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public'
      and t.typname = 'competition_participation_record'
      and a.attname = 'competition_external_id'
      and not a.attisdropped
  ) then
    alter type public.competition_participation_record
      add attribute competition_external_id text;
  end if;
end
$$;

create or replace function public.competition_brief(
  p_since date default null,
  p_until date default null,
  p_cohort_id bigint default null,
  p_person_ids bigint[] default null
) returns setof public.competition_participation_record
  language sql stable
as $$
  with params as (
    select
      coalesce(p_since, date_trunc('week', now())::date + 5) as since,
      coalesce(p_until, date_trunc('week', now())::date + 7) as until
  ),
  scoped_people as (
    select distinct p.id, p.name, p.csts_id, p.wdsf_id
    from public.current_tenant_membership tm
    join public.person p on p.id = tm.person_id
    where (p_person_ids is null or p.id = any(p_person_ids))
      and (
        p_cohort_id is null
        or exists (
          select 1
          from public.current_cohort_membership cm
          where cm.person_id = p.id
            and cm.cohort_id = p_cohort_id
        )
      )
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    cross join lateral (
      values
        ('csts'::text, sp.csts_id::bigint),
        ('wdsf'::text, sp.wdsf_id::bigint)
    ) ids(federation, external_id)
    join federated.person fp
      on fp.federation = ids.federation
     and fp.external_id = ids.external_id
  )
  select
    fp.person_id,
    fp.person_name,
    fp.federation,
    fp.federated_person_id,
    c.id as competitor_id,
    c.name as competitor_name,
    c.competitor_type,
    e.id as event_id,
    e.name as event_name,
    coalesce(e.location, e.city) as event_location,
    comp.id as competition_id,
    comp.start_date as competition_date,
    comp.check_in_end,
    cat as category,
    dances.dances,
    comp.participants_total as participants,
    null::integer as ranking,
    null::integer as ranking_to,
    null::numeric(10,3) as point_gain,
    null::boolean as is_final,
    false as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id
  from params
  join federated.competition comp
    on comp.start_date >= params.since
   and comp.start_date < params.until
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  join federated.competition_entry ce
    on ce.competition_id = comp.id
   and not ce.cancelled
  join federated.competitor c on c.id = ce.competitor_id
  join federated.competitor_component cc on cc.competitor_id = c.id
  join federated_people fp on fp.federated_person_id = cc.person_id
  left join lateral (
    select coalesce(array_agg(d.name order by dpd.dance_order), '{}'::text[]) as dances
    from federated.dance_program_dance dpd
    join federated.dance d on d.code = dpd.dance_code
    where dpd.program_id = cat.base_dance_program_id
  ) dances on true
  order by
    comp.start_date,
    comp.check_in_end nulls last,
    fp.person_name,
    cat.discipline,
    cat.class,
    c.name;
$$;

comment on function public.competition_brief(date, date, bigint, bigint[]) is
  '@simpleCollections only';
grant all on function public.competition_brief(date, date, bigint, bigint[]) to anonymous;

create or replace function public.competition_report(
  p_since date default null,
  p_until date default null,
  p_cohort_id bigint default null,
  p_person_ids bigint[] default null
) returns setof public.competition_participation_record
  language sql stable
as $$
  with params as (
    select
      coalesce(p_since, date_trunc('week', now())::date - 2) as since,
      coalesce(p_until, date_trunc('week', now())::date) as until
  ),
  scoped_people as (
    select distinct p.id, p.name, p.csts_id, p.wdsf_id
    from public.current_tenant_membership tm
    join public.person p on p.id = tm.person_id
    where (p_person_ids is null or p.id = any(p_person_ids))
      and (
        p_cohort_id is null
        or exists (
          select 1
          from public.current_cohort_membership cm
          where cm.person_id = p.id
            and cm.cohort_id = p_cohort_id
        )
      )
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    cross join lateral (
      values
        ('csts'::text, sp.csts_id::bigint),
        ('wdsf'::text, sp.wdsf_id::bigint)
    ) ids(federation, external_id)
    join federated.person fp
      on fp.federation = ids.federation
     and fp.external_id = ids.external_id
  )
  select
    fp.person_id,
    fp.person_name,
    fp.federation,
    fp.federated_person_id,
    c.id as competitor_id,
    c.name as competitor_name,
    c.competitor_type,
    e.id as event_id,
    e.name as event_name,
    coalesce(e.location, e.city) as event_location,
    comp.id as competition_id,
    comp.start_date as competition_date,
    comp.check_in_end,
    cat as category,
    dances.dances,
    comp.participants_total as participants,
    cr.ranking,
    cr.ranking_to,
    cr.point_gain,
    cr.is_final,
    true as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id
  from params
  join federated.competition comp
    on comp.start_date >= params.since
   and comp.start_date < params.until
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  join federated.competition_result cr on cr.competition_id = comp.id
  join federated.competitor c on c.id = cr.competitor_id
  join federated.competitor_component cc on cc.competitor_id = c.id
  join federated_people fp on fp.federated_person_id = cc.person_id
  left join lateral (
    select coalesce(array_agg(d.name order by dpd.dance_order), '{}'::text[]) as dances
    from federated.dance_program_dance dpd
    join federated.dance d on d.code = dpd.dance_code
    where dpd.program_id = cat.base_dance_program_id
  ) dances on true
  order by
    comp.start_date,
    fp.person_name,
    cat.discipline,
    cat.class,
    cr.ranking,
    c.name;
$$;

comment on function public.competition_report(date, date, bigint, bigint[]) is
  '@simpleCollections only';
grant all on function public.competition_report(date, date, bigint, bigint[]) to anonymous;

create or replace view public.activity_timeline_item as
select
  null::text as id,
  null::public.activity_timeline_kind as kind,
  null::timestamp with time zone as sort_at,
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

comment on view public.activity_timeline_item is '
@primaryKey id
@interface mode:single type:kind
@type EVENT_ATTENDANCE name:ActivityEventAttendance attributes:event_attendance_id,event_instance_id
@type COMPETITION_BRIEF name:ActivityCompetitionBrief attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,check_in_end,category,dances,participants,competition_type,competition_event_external_id,competition_external_id
@type COMPETITION_RESULT name:ActivityCompetitionResult attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,dances,participants,ranking,ranking_to,point_gain,is_final,competition_type,competition_event_external_id,competition_external_id
@foreignKey (person_id) references person (id)|@fieldName person|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_attendance_id) references event_attendance (id)|@fieldName eventAttendance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_instance_id) references event_instance (id)|@fieldName eventInstance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@behavior -query:resource:list -query:resource:connection -query:resource:single
';

grant select on public.activity_timeline_item to anonymous;

create or replace function public.activity_timeline(
  p_since timestamptz,
  p_until timestamptz,
  p_person_ids bigint[] default null,
  p_cohort_id bigint default null,
  p_kinds public.activity_timeline_kind[] default null,
  p_event_types public.event_type[] default null
) returns setof public.activity_timeline_item
  language plpgsql stable
as $$
declare
  include_event_attendance boolean;
  include_competition_brief boolean;
  include_competition_result boolean;
begin
  if p_since is null or p_until is null or p_until <= p_since then
    return;
  end if;

  if p_person_ids is null and p_cohort_id is null then
    return;
  end if;

  if cardinality(p_kinds) = 0 then
    return;
  end if;

  include_event_attendance =
    (p_kinds is null or 'EVENT_ATTENDANCE'::public.activity_timeline_kind = any(p_kinds))
    and (p_event_types is null or cardinality(p_event_types) > 0);
  include_competition_brief =
    p_kinds is null or 'COMPETITION_BRIEF'::public.activity_timeline_kind = any(p_kinds);
  include_competition_result =
    p_kinds is null or 'COMPETITION_RESULT'::public.activity_timeline_kind = any(p_kinds);

  if include_event_attendance then
    return query
      with scoped_people as (
        select distinct p.id
        from public.person p
        where (p_person_ids is not null and p.id = any(p_person_ids))
           or (
             p_cohort_id is not null
             and exists (
               select 1
               from public.current_cohort_membership cm
               where cm.person_id = p.id
                 and cm.cohort_id = p_cohort_id
             )
           )
      )
      select
        ('event_attendance:' || ea.id)::text as id,
        'EVENT_ATTENDANCE'::public.activity_timeline_kind as kind,
        ei.since as sort_at,
        ei.since::date as activity_date,
        ea.person_id,
        p.name as person_name,
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
      from public.event_attendance ea
      join public.event_instance ei on ei.id = ea.instance_id
      join public.person p on p.id = ea.person_id
      join scoped_people sp on sp.id = ea.person_id
      where ea.status <> 'cancelled'
        and ei.since >= p_since
        and ei.since < p_until
        and (p_event_types is null or ei.type = any(p_event_types));
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
        'COMPETITION_RESULT'::public.activity_timeline_kind as kind,
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
        select *
        from public.competition_report(
          p_since::date,
          p_until::date,
          p_cohort_id,
          p_person_ids
        )
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
          select *
          from public.competition_report(
            p_since::date,
            p_until::date,
            p_cohort_id,
            p_person_ids
          )
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
        'COMPETITION_BRIEF'::public.activity_timeline_kind as kind,
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
        select *
        from public.competition_brief(
          p_since::date,
          p_until::date,
          p_cohort_id,
          p_person_ids
        )
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
end;
$$;

comment on function public.activity_timeline(timestamptz, timestamptz, bigint[], bigint, public.activity_timeline_kind[], public.event_type[]) is '
@behavior +queryField:resource:list -queryField:resource:connection
';
grant all on function public.activity_timeline(timestamptz, timestamptz, bigint[], bigint, public.activity_timeline_kind[], public.event_type[]) to anonymous;
