--! Previous: sha1:2f0a885df1629c54968083fd32e50c0d420b65c5
--! Hash: sha1:372d6fb366be7371586b0fba342bdb88d5216445

--! split: 1-current.sql
comment on table couple is '@behavior +list -connection +query:resource:list -query:resource:connection
@ref federationCompetitors via:public.couple_federation_link;federated.competitor
@ref federationRanklistEntries via:public.couple_federation_link;federated.competitor;federated.ranklist_entry';

comment on table federated.competition_result is '@name competition_result
@behavior -insert -update -delete -list +query:resource:connection -query:resource:list -query:resource:single
@foreignKey (competition_id,last_round) references federated.competition_round (competition_id,round_key)|@fieldName round|@behavior -manyRelation:resource:connection -manyRelation:resource:list';


drop function if exists person_active_couples(person);
drop function if exists person_all_couples(person);
drop function if exists tenant_couples(tenant);
drop function if exists event_remaining_person_spots(event);
drop function if exists event_instance_remaining_person_spots;
drop function if exists event_instance_trainer_lessons_remaining;
drop function if exists people_without_access_or_invitation();
drop function if exists people_without_access_with_existing_account();
drop function if exists people_without_access_with_invitation();
drop function if exists event_trainer_name(event_trainer);
drop function if exists person_has_access(person);
drop function if exists event_instance_trainers;
drop function if exists app_private.event_instance_trainers_at;
drop function if exists app_private.event_trainers_at(event, timestamptz);

do $$
begin
  if to_regtype('public.event_stats') is null then
    create type event_stats as (
      total integer,
      unknown integer,
      attended integer,
      not_excused integer
    );
  end if;

  if exists (select from pg_attribute
    where attrelid = 'event'::regclass and attname = 'stats' and atttypid = 'jsonb'::regtype) then
    alter table event alter column stats drop default;
    alter table event alter column stats type event_stats using row(
      coalesce((stats->>'TOTAL')::integer, 0),
      coalesce((stats->>'UNKNOWN')::integer, 0),
      coalesce((stats->>'ATTENDED')::integer, 0),
      coalesce((stats->>'NOT_EXCUSED')::integer, 0)
    )::event_stats;
  end if;
end $$;

alter table event alter column stats set default row(0, 0, 0, 0)::event_stats;
comment on column event.stats is
  '@behavior -attribute:insert -attribute:update -attribute:filterBy -attribute:orderBy';
comment on column event.manager_person_ids is '@behavior -attribute:insert -attribute:update';

comment on column event_stats.total is '@notNull';
comment on column event_stats.unknown is '@notNull';
comment on column event_stats.attended is '@notNull';
comment on column event_stats.not_excused is '@notNull';

--! Included functions/event_stats.sql
drop trigger if exists _500_eir_refresh_stats_ins on event_registration;
drop trigger if exists _500_eir_refresh_stats_del on event_registration;
drop trigger if exists _500_eir_refresh_stats_upd on event_registration;
drop function if exists app_private.tg_eir__refresh_stats_stmt;
drop function if exists app_private.refresh_event_instance_stats;

create or replace function app_private.refresh_event_stats(p_event_id bigint)
  returns void
  language sql volatile security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select 1 from event where id = p_event_id for no key update;

  update event instance
  set stats = actual.stats
  from (
    select row(
      count(*)::int,
      count(*) filter (where status = 'unknown')::int,
      count(*) filter (where status = 'attended')::int,
      count(*) filter (where status = 'not-excused')::int
    )::event_stats as stats
    from event_registration
    where event_id = p_event_id
      and person_id is not null
      and registration_status = 'active'
  ) actual
  where instance.id = p_event_id and instance.stats is distinct from actual.stats;
$$;

create or replace function app_private.tg_event_registration__refresh_stats_stmt()
  returns trigger
  language plpgsql security definer
  set search_path = pg_catalog, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
begin
  if tg_op = 'INSERT' then
    perform app_private.refresh_event_stats(affected.event_id)
    from (
      select distinct event_id from changed_rows
      where person_id is not null order by event_id
    ) affected;
  elsif tg_op = 'DELETE' then
    perform app_private.refresh_event_stats(affected.event_id)
    from (
      select distinct event_id from deleted_rows
      where person_id is not null order by event_id
    ) affected;
  else
    perform app_private.refresh_event_stats(affected.event_id)
    from (
      select distinct affected.event_id
      from deleted_rows old_row
      join changed_rows new_row using (id)
      cross join lateral (values
        (old_row.event_id, old_row.person_id),
        (new_row.event_id, new_row.person_id)
      ) affected(event_id, person_id)
      where row(old_row.event_id, old_row.person_id, old_row.status, old_row.registration_status)
        is distinct from
        row(new_row.event_id, new_row.person_id, new_row.status, new_row.registration_status)
        and affected.person_id is not null
      order by affected.event_id
    ) affected;
  end if;
  return null;
end;
$$;

create or replace trigger _500_refresh_stats_ins
  after insert on event_registration
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event_registration__refresh_stats_stmt();
create or replace trigger _500_refresh_stats_del
  after delete on event_registration
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_event_registration__refresh_stats_stmt();
create or replace trigger _500_refresh_stats_upd
  after update on event_registration
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event_registration__refresh_stats_stmt();
--! EndIncluded functions/event_stats.sql
--! Included functions/event_registration_info.sql
drop function if exists event_registration_info;
drop type if exists event_registration_info;

create type event_registration_info as (
  registrations integer,
  people integer,
  remaining_capacity integer,
  my boolean,
  can_register boolean
);

create or replace function event_registration_info(e event)
  returns event_registration_info
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  with counts as (
    select
      count(*) filter (where parent_registration_id is null)::integer as registrations,
      count(person_id)::integer as people,
      count(*) filter (where person_id = any(current_person_ids())) > 0 as my
    from event_registration
    where event_id = e.id and registration_status = 'active'
  ), capacity as (
    select counts.*, case
      when e.capacity is null or e.capacity <= 0 then null
      else e.capacity
        - case e.capacity_unit
            when 'people' then people
            when 'registrations' then registrations
          end
    end as remaining_capacity
    from counts
  )
  select row(
    registrations, people, remaining_capacity, my,
    coalesce(
          not coalesce(e.is_locked, false)
      and not e.is_cancelled
      and e.until > now()
      and coalesce(remaining_capacity > 0, true),
      false
    )
  )::event_registration_info
  from capacity;
$$;

grant all on function event_registration_info to anonymous;
--! EndIncluded functions/event_registration_info.sql
--! Included functions/event_facets.sql
drop function if exists event_instances_for_range;
drop function if exists event_facets;
drop type if exists event_instance_range_scope;

create or replace function event_facets(
  start_range timestamptz,
  end_range timestamptz,
  only_types event_type[] default null,
  trainer_ids bigint[] default null,
  participant_ids bigint[] default null,
  parent_id bigint default null,
  location_ids bigint[] default null,
  can_register boolean default null,
  only_mine boolean default false,
  relevant_only boolean default false,
  cohort_id bigint default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with mine as (
    select event_id from event_registration
    where person_id = any((select current_person_ids())::bigint[])
      and registration_status = 'active'
    union all
    select event_id from event_trainer
    where person_id = any((select current_person_ids())::bigint[])
  ), cohort_people as (
    select cohort_id, person_id from current_cohort_membership
    where (not coalesce(only_mine, false)
        or person_id = any((select current_person_ids())::bigint[]))
      and (participant_ids is not null or coalesce(only_mine, false)
        or person_id in (
          select person_id from current_tenant_membership
          union all select person_id from current_tenant_trainer
          union all select person_id from current_tenant_administrator
        ))
  ), candidates as materialized (
    select i.id, i.type, i.location_id,
      array(select person_id from event_trainer where event_id = i.id) as trainers,
      array(select person_id from event_registration
        where event_id = i.id and registration_status = 'active') as participants
    from event i
    where i.tenant_id = current_tenant_id()
      and start_range is not null
      and app_private.tstzrange_overlaps(i.range, tstzrange(start_range, end_range, '[)'))
      and (can_register is null or (event_registration_info(i)).can_register = can_register)
      and ($6 is null or i.parent_id = $6)
      and (not coalesce(only_mine, false) or i.id in (select event_id from mine))
      and (not coalesce(relevant_only, false) or i.parent_id is null
        or i.id in (select event_id from mine)
        or i.parent_id in (select event_id from mine))
  ), matches as (
    select *,
      only_types is null or type = any(only_types) as type_matches,
      trainer_ids is null or trainers && trainer_ids as trainer_matches,
      participant_ids is null or case when $11 is null
        then participants && participant_ids
        else (trainers || participants) && array(
          select person_id from cohort_people c
          where c.cohort_id = $11 and person_id = any(participant_ids)
        ) end as participant_matches,
      $11 is null or (trainers || participants) && array(
        select person_id from cohort_people c where c.cohort_id = $11
      ) as cohort_matches,
      location_ids is null or location_id = any(location_ids) as location_matches
    from candidates
  ), facets as (
    select m.id, 'type' as facet, upper(m.type::text) as value
    from matches m
    where m.trainer_matches and m.participant_matches and m.location_matches and m.cohort_matches
    union all
    select m.id, 'trainer', unnest(m.trainers)::text
    from matches m
    where m.type_matches and m.participant_matches and m.location_matches and m.cohort_matches
    union all
    select m.id, 'participant', person_id::text
    from matches m
    cross join lateral unnest(m.participants) person(person_id)
    where m.type_matches and m.trainer_matches and m.location_matches and m.cohort_matches
      and ($11 is null or person_id in (
        select person_id from cohort_people c where c.cohort_id = $11
      ))
    union all
    select m.id, 'location', m.location_id::text
    from matches m
    where m.type_matches and m.trainer_matches and m.participant_matches and m.cohort_matches
      and m.location_id is not null
    union all
    select m.id, 'cohort', c.cohort_id::text
    from matches m
    cross join lateral unnest(m.trainers || m.participants) person(person_id)
    join cohort_people c using (person_id)
    where m.type_matches and m.trainer_matches and m.location_matches
      and (participant_ids is null or c.person_id = any(participant_ids))
  )
  select facet, value, count(*)::integer
  from (select distinct id, facet, value from facets) events
  group by facet, value;
$$;
comment on function event_facets is '@behavior +list -connection';
grant execute on function event_facets to anonymous;
--! EndIncluded functions/event_facets.sql
drop function if exists public.activity_timeline;
drop view if exists public.activity_timeline_item;
drop type if exists public.activity_timeline_kind;
--! Included functions/person_birthday.sql
create or replace function person_birthday(p person, since date default current_date)
returns date language sql stable strict as $$
  -- Adding a year interval clamps February 29 to February 28 when needed.
  select case
    when (p.birth_date + make_interval(years => extract(year from since)::int - extract(year from p.birth_date)::int))::date >= since
    then (p.birth_date + make_interval(years => extract(year from since)::int - extract(year from p.birth_date)::int))::date
    else (p.birth_date + make_interval(years => extract(year from since)::int - extract(year from p.birth_date)::int + 1))::date
  end;
$$;

comment on function person_birthday is '@behavior -queryField';
grant execute on function person_birthday to anonymous;
--! EndIncluded functions/person_birthday.sql
--! Included functions/set_event_registration.sql
drop function if exists set_event_instance_registration;
drop function if exists set_event_registration;
drop function if exists register_to_event;
drop function if exists app_private.set_event_instance_registration;
drop type if exists event_registration_person_input;

create type event_registration_person_input as (
  first_name text,
  last_name text,
  gender gender_type,
  nationality text,
  email text,
  phone text,
  birth_date date,
  csts_id integer,
  wdsf_id integer
);

create function set_event_registration(
  event_id bigint,
  person_id bigint,
  couple_id bigint,
  is_registered boolean,
  note text default null,
  lesson_trainer_ids bigint[] default null,
  lesson_counts integer[] default null,
  person event_registration_person_input default null
) returns event_registration
language plpgsql security definer
set search_path = pg_catalog, public, pg_temp
as $$
declare
  target_instance event;
  registration event_registration;
  registration_found boolean;
  is_self boolean;
  is_manager boolean;
  registration_source event_registration_source;
  required_capacity integer;
  remaining_capacity integer;
  request record;
begin
  if is_registered is null or num_nonnulls(person_id, couple_id, person) <> 1
    or (person is distinct from null and not is_registered) then
    raise exception 'INVALID_PARTICIPANT' using errcode = '22023';
  end if;

  if num_nonnulls(lesson_trainer_ids, lesson_counts) = 1
    or cardinality(lesson_trainer_ids) <> cardinality(lesson_counts)
    or exists (
      select 1
      from unnest(lesson_trainer_ids, lesson_counts) demand(trainer_id, lesson_count)
      where demand.trainer_id is null
        or demand.lesson_count is null
        or demand.lesson_count < 0
    ) then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;
  if not is_registered and lesson_trainer_ids is not null then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;

  select * into target_instance
  from event
  where id = event_id
    and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;

  is_manager := pg_has_role(current_setting('role'), 'administrator', 'member')
    or (pg_has_role(current_setting('role'), 'trainer', 'member') and app_private.can_trainer_edit_event(event_id));

  if person is distinct from null then
    if nullif(btrim(person.first_name), '') is null
      or nullif(btrim(person.last_name), '') is null
      or person.gender is null
      or nullif(btrim(person.nationality), '') is null
      or person.email is null or person.email !~ '^[^[:space:]@]+@[^[:space:]@]+\.[^[:space:]@]+$'
      or nullif(btrim(person.phone), '') is null
      or person.birth_date > current_date
      or person.csts_id <= 0 or person.wdsf_id <= 0 then
      raise exception 'INVALID_PERSON_DETAILS' using errcode = '22023';
    end if;

    select r.* into registration
    from event_registration r
    join person p on p.id = r.person_id
    where r.event_id = set_event_registration.event_id
      and r.tenant_id = current_tenant_id()
      and r.created_by is not distinct from current_user_id()
      and r.registration_status = 'active'
      and r.parent_registration_id is null
      and r.note is not distinct from set_event_registration.note
      and row(
        p.first_name, p.last_name, p.gender, p.nationality,
        p.email, p.phone, p.birth_date, p.csts_id, p.wdsf_id
      ) is not distinct from row(
        btrim((set_event_registration.person).first_name), btrim((set_event_registration.person).last_name),
        (set_event_registration.person).gender, (set_event_registration.person).nationality,
        btrim((set_event_registration.person).email), btrim((set_event_registration.person).phone),
        (set_event_registration.person).birth_date, (set_event_registration.person).csts_id,
        (set_event_registration.person).wdsf_id
      )
      and coalesce((
        select jsonb_object_agg(d.trainer_id, d.lesson_count)
        from event_lesson_request d where d.registration_id = r.id
      ), '{}'::jsonb) = coalesce((
        select jsonb_object_agg(d.trainer_id, d.lesson_count)
        from unnest(lesson_trainer_ids, lesson_counts) d(trainer_id, lesson_count)
        where d.lesson_count > 0
      ), '{}'::jsonb)
    order by r.id
    limit 1;

    if found then
      return registration;
    end if;

    insert into person (
      first_name, last_name, gender, nationality, email, phone, birth_date, csts_id, wdsf_id
    ) values (
      btrim((set_event_registration.person).first_name), btrim((set_event_registration.person).last_name),
      (set_event_registration.person).gender, (set_event_registration.person).nationality,
      btrim((set_event_registration.person).email), btrim((set_event_registration.person).phone),
      (set_event_registration.person).birth_date, (set_event_registration.person).csts_id,
      (set_event_registration.person).wdsf_id
    )
    returning id into person_id;

    is_self := not is_manager;
  else
    is_self := coalesce(person_id = any(current_person_ids()), false)
      or coalesce(couple_id = any(current_couple_ids()), false);
  end if;

  if not is_self and not is_manager then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;
  if target_instance.is_locked and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  registration_source := case when is_self
    then 'self'::event_registration_source
    else 'manager'::event_registration_source
  end;

  select * into registration
  from event_registration r
  where r.event_id = set_event_registration.event_id
    and r.person_id is not distinct from set_event_registration.person_id
    and r.couple_id is not distinct from set_event_registration.couple_id
  order by (r.registration_status = 'active') desc, (r.parent_registration_id is null) desc, r.id desc
  limit 1;
  registration_found := found;

  if not is_registered then
    if not registration_found then
      raise exception 'REGISTRATION_NOT_FOUND' using errcode = '22023';
    end if;

    update event_registration
    set registration_status = 'cancelled',
        target_cohort_id = null,
        source = registration_source
    where id = registration.id or parent_registration_id = registration.id;

    select * into registration from event_registration where id = registration.id;
    return registration;
  end if;

  if registration_found and registration.registration_status = 'active' then
    if note is not null then
      update event_registration set note = set_event_registration.note where id = registration.id
      returning * into registration;
    end if;
  else
    if not is_manager and (
      target_instance.is_cancelled
        or target_instance.until <= now()
        or not (
          coalesce(target_instance.is_public, false)
          or (
            coalesce(target_instance.is_visible, false)
            and current_tenant_id() = any(my_tenants_array())
          )
        )
    ) then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;

    if not is_manager then
      remaining_capacity := (event_registration_info(target_instance)).remaining_capacity;
      required_capacity := case
        when target_instance.capacity_unit = 'people' and couple_id is not null then 2
        when target_instance.capacity_unit = 'registrations' and exists (
          select 1 from event_registration parent
          where parent.id = registration.parent_registration_id and parent.registration_status = 'active'
        ) then 0
        else 1
      end;
      if couple_id is not null then
        required_capacity := required_capacity - (
          select count(*)::integer from event_registration r
          join couple c on c.id = set_event_registration.couple_id and r.person_id in (c.man_id, c.woman_id)
          where r.event_id = set_event_registration.event_id and r.registration_status = 'active'
            and (target_instance.capacity_unit = 'people' or r.parent_registration_id is null)
        );
      end if;
      if remaining_capacity is not null and remaining_capacity < required_capacity then
        raise exception 'CAPACITY_EXCEEDED' using errcode = '22023';
      end if;
    end if;

    if registration_found then
      update event_registration
      set registration_status = 'active',
          target_cohort_id = null,
          source = registration_source,
          parent_registration_id = (
            select parent.id from event_registration parent
            where parent.id = registration.parent_registration_id and parent.registration_status = 'active'
          )
      where id = registration.id;

      if note is not null then
        update event_registration set note = set_event_registration.note where id = registration.id;
      end if;
    else
      insert into event_registration (
        event_id, person_id, couple_id, source, status, note
      ) values (
        event_id, set_event_registration.person_id, set_event_registration.couple_id, registration_source,
        case when set_event_registration.person_id is not null then 'unknown'::attendance_type end,
        set_event_registration.note
      ) returning * into registration;
    end if;
  end if;

  if couple_id is not null then
    perform app_private.ensure_couple_person_registrations(registration.id);
  end if;

  select * into registration from event_registration where id = registration.id;

  if lesson_trainer_ids is not null then
    delete from event_lesson_request
    where registration_id = registration.id
      and trainer_id <> all(lesson_trainer_ids);

    for request in
      select demand.trainer_id, demand.lesson_count
      from unnest(lesson_trainer_ids, lesson_counts)
        demand(trainer_id, lesson_count)
      order by demand.trainer_id
    loop
      perform app_private.set_lesson_request(registration, request.trainer_id, request.lesson_count);
    end loop;
  end if;

  if person is distinct from null then
    insert into person_invitation (person_id, email, event_id)
    values (registration.person_id, btrim((set_event_registration.person).email), event_id);
  end if;

  return registration;
end;
$$;

grant execute on function set_event_registration to anonymous;

create function set_event_instance_registration(
  p_instance_id bigint,
  p_person_id bigint,
  p_couple_id bigint,
  p_is_registered boolean,
  p_note text default null,
  p_lesson_trainer_ids bigint[] default null,
  p_lesson_counts integer[] default null,
  p_person event_registration_person_input default null
) returns event_registration
language sql
as $$
  select public.set_event_registration($1, $2, $3, $4, $5, $6, $7, $8);
$$;

comment on function set_event_instance_registration is E'@deprecated Use setEventRegistration.\n@resultFieldName eventInstanceRegistration';
grant execute on function set_event_instance_registration to anonymous;
--! EndIncluded functions/set_event_registration.sql
--! Included functions/event_trainer_lessons_remaining.sql
create or replace function event_trainer_lessons_remaining(e event_trainer) returns integer as $$
  select case
    when e.lessons_offered is null then null
    else e.lessons_offered - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_request demand
      join event_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      where demand.trainer_id = e.id
    )
  end;
$$ language sql stable security definer set search_path = pg_catalog, public, pg_temp;

grant all on function event_trainer_lessons_remaining to anonymous;
--! EndIncluded functions/event_trainer_lessons_remaining.sql
--! Included functions/can_trainer_edit_event.sql
create or replace function app_private.can_trainer_edit_event(p_event_id bigint)
  returns boolean
  language sql stable security definer leakproof parallel safe
  set search_path = pg_catalog, public, pg_temp
as $$
  select exists (
    select 1 from event
    where id = p_event_id
      and (cardinality(manager_person_ids) = 0 or manager_person_ids && (select current_person_ids()))
  );
$$;

grant all on function app_private.can_trainer_edit_event to anonymous;
--! EndIncluded functions/can_trainer_edit_event.sql
--! Included functions/tg_event__refresh_manager_person_ids.sql
drop trigger if exists _500_refresh_manager_person_ids_from_parent on event;
drop function if exists app_private.tg_event_instance__refresh_manager_person_ids;
drop function if exists app_private.refresh_event_instance_manager_person_ids;
drop function if exists app_private.event_instance_manager_person_ids;

create or replace function app_private.event_manager_person_ids(p_event_id bigint)
  returns bigint[] language sql stable parallel safe as $$
  with recursive chain as (
    select id, parent_id
    from event where id = p_event_id
    union all
    select parent.id, parent.parent_id
    from event parent
    join chain child on child.parent_id = parent.id
  ), managers as (
    select distinct person_id from event_trainer
    where event_id in (select id from chain)
  )
  select coalesce(array_agg(person_id order by person_id), '{}'::bigint[])
  from managers;
$$;

drop function if exists app_private.refresh_event_manager_person_ids;

create function app_private.refresh_event_manager_person_ids(p_event_id bigint)
  returns void language sql security definer
  set search_path = pg_catalog, public, pg_temp as $$
  with recursive desired as (
    select id, app_private.event_manager_person_ids(id) as person_ids
    from event where id = p_event_id
    union all
    select child.id, array(
      select distinct person_id
      from unnest(parent.person_ids || array(
        select person_id from event_trainer where event_id = child.id
      )) person_id
      order by person_id
    )
    from event child
    join desired parent on child.parent_id = parent.id
  )
  update event instance
  set manager_person_ids = desired.person_ids
  from desired
  where instance.id = desired.id
    and instance.manager_person_ids is distinct from desired.person_ids
$$;

grant execute on function app_private.refresh_event_manager_person_ids to anonymous;

create or replace function app_private.tg_event__refresh_manager_person_ids()
  returns trigger language plpgsql as $$
begin
  perform app_private.refresh_event_manager_person_ids(new.id);
  return null;
end;
$$;

create trigger _500_refresh_manager_person_ids_from_parent
  after insert or update of parent_id on event
  for each row execute function app_private.tg_event__refresh_manager_person_ids();

drop trigger if exists _500_refresh_manager_person_ids on event_trainer;
drop function if exists app_private.tg_event_instance_trainer__refresh_manager_person_ids();

create or replace function app_private.tg_event_trainer__refresh_manager_person_ids()
  returns trigger language plpgsql as $$
begin
  if tg_op = 'DELETE' then
    perform app_private.refresh_event_manager_person_ids(old.event_id);
  elsif tg_op = 'INSERT' then
    perform app_private.refresh_event_manager_person_ids(new.event_id);
  elsif old.event_id is distinct from new.event_id
    or old.person_id is distinct from new.person_id then
    perform app_private.refresh_event_manager_person_ids(old.event_id);
    perform app_private.refresh_event_manager_person_ids(new.event_id);
  end if;
  return null;
end;
$$;

create trigger _500_refresh_manager_person_ids
  after insert or delete or update of event_id, person_id
  on event_trainer
  for each row execute function app_private.tg_event_trainer__refresh_manager_person_ids();
--! EndIncluded functions/tg_event__refresh_manager_person_ids.sql
--! Included functions/trainer_group_attendance_completion.sql
drop function if exists trainer_group_attendance_completion;
drop type if exists trainer_group_attendance_completion;

create type trainer_group_attendance_completion as (
  person_id bigint,
  total_events integer,
  filled_events integer,
  partially_filled_events integer,
  unfilled_events integer,
  filled_ratio double precision,
  total_attendances integer,
  pending_attendances integer
);

comment on type trainer_group_attendance_completion is '@foreignKey (person_id) references person (id)';

create or replace function trainer_group_attendance_completion(
  since timestamp with time zone default null,
  until timestamp with time zone default null
) returns setof trainer_group_attendance_completion
  language sql stable as $_$
  with attendance_stats as (
    select
      trainer.person_id,
      (instance.stats).total as attendance_count,
      (instance.stats).unknown as unknown_count
    from event instance
    join event_trainer trainer on trainer.event_id = instance.id
    where instance.tenant_id = current_tenant_id()
      and not instance.is_cancelled
      and instance.type = 'group'
      and instance.until < coalesce($2, now())
      and ($1 is null or instance.since >= $1)
  ),
  per_trainer as (
    select
      person_id,
      count(*) as total_events,
      count(*) filter (where attendance_count > 0 and unknown_count = 0) as filled_events,
      count(*) filter (
        where attendance_count > 0
          and unknown_count > 0
          and unknown_count < attendance_count
      ) as partially_filled_events,
      count(*) filter (where attendance_count = 0 or unknown_count = attendance_count) as unfilled_events,
      coalesce(sum(attendance_count), 0) as total_attendances,
      coalesce(sum(unknown_count), 0) as pending_attendances
    from attendance_stats
    group by person_id
  )
  select
    person_id,
    total_events,
    filled_events,
    partially_filled_events,
    unfilled_events,
    case
      when total_events > 0 then (filled_events + partially_filled_events)::double precision / total_events
      else null
    end as filled_ratio,
    total_attendances,
    pending_attendances
  from per_trainer
  order by filled_ratio asc nulls last, person_id;
$_$;

comment on function trainer_group_attendance_completion is '@behavior +list -connection';
grant all on function trainer_group_attendance_completion to anonymous;
--! EndIncluded functions/trainer_group_attendance_completion.sql
--! Included functions/event_trainers.sql
create or replace function event_trainers(v_instance event)
  returns setof tenant_trainer
  language sql stable
as $$
-- Compatibility for external clients; internal callers use event assignments.
select tt.*
from event_trainer assignment
join tenant_trainer tt on tt.tenant_id = assignment.tenant_id
  and tt.person_id = assignment.person_id
  and tt.active_range @> v_instance.since
where assignment.event_id = v_instance.id;
$$;

grant all on function event_trainers to anonymous;
comment on function event_trainers is '@behavior +list -connection
@deprecated Use eventTrainersList.';
--! EndIncluded functions/event_trainers.sql
--! Included functions/event_approx_price.sql
drop function if exists event_instance_approx_price;

create or replace function event_approx_price(v_instance event)
  returns table (amount numeric(19,4), currency text)
  language sql stable
as $$
  with stats as (
    select
      (v_instance.stats).total as num_participants,
      extract(epoch from (v_instance.until - v_instance.since)) / 60.0 as duration
    where v_instance.type = 'lesson'
  )
  select
    sum(tt.member_price_45min_amount * s.duration / 45 / s.num_participants) as amount,
    tt.currency as currency
  from stats s
  join event_trainer assignment on assignment.event_id = v_instance.id
  join tenant_trainer tt on tt.tenant_id = assignment.tenant_id
    and tt.person_id = assignment.person_id
    and tt.active_range @> v_instance.since
  where
    s.num_participants > 0
    and s.duration > 0
    and tt.member_price_45min_amount is not null
    and tt.currency is not null
  group by tt.currency;
$$;

grant all on function event_approx_price to anonymous;
comment on function event_approx_price is '@behavior +list -connection';
--! EndIncluded functions/event_approx_price.sql
--! Included functions/create_event_payment.sql
create or replace function create_event_payment(target_event event)
  returns payment
  language plpgsql
as $$
declare
  created_payment payment;
  duration numeric(19, 4);
begin
  if current_tenant_id() <> 2 then
    return null;
  end if;

  -- Serialize initial charging with registration changes and their deferred sync.
  select * into target_event from event
  where id = target_event.id and tenant_id = current_tenant_id() for no key update;
  if not found or target_event.is_cancelled then return null; end if;

  select p.* into created_payment from payment p where p.event_id = target_event.id;

  if found then
    return created_payment;
  end if;

  if target_event.type <> 'lesson' or not exists (
    select 1
    from event_registration registration
    where registration.event_id = target_event.id
      and registration.person_id is not null
      and registration.registration_status = 'active'
  ) then
    return null;
  end if;

  duration := extract(epoch from (target_event.until - target_event.since)) / 60;

  insert into payment (status, event_id, due_at)
  values (
    'tentative', target_event.id, now() + interval '2 weeks'
  )
  on conflict (event_id) where event_id is not null do nothing
  returning * into created_payment;

  if not found then
    return (select p from payment p where p.event_id = target_event.id);
  end if;

  insert into payment_recipient (payment_id, account_id, amount)
  select distinct on (account.id)
    created_payment.id,
    account.id,
    trainer.member_price_45min_amount / 45 * duration
  from event_trainer assignment
  join tenant_trainer trainer on trainer.tenant_id = assignment.tenant_id
    and trainer.person_id = assignment.person_id
    and trainer.active_range @> target_event.since
  cross join lateral person_account(trainer.person_id, trainer.currency) account
  where assignment.event_id = target_event.id
    and trainer.member_price_45min_amount is not null;

  insert into payment_debtor (payment_id, person_id)
  select distinct created_payment.id, registration.person_id
  from event_registration registration
  where registration.event_id = target_event.id
    and registration.person_id is not null
    and registration.registration_status = 'active';

  return created_payment;
end
$$;

comment on function create_event_payment is '@behavior -*';
grant all on function create_event_payment to anonymous;
--! EndIncluded functions/create_event_payment.sql
--! Included functions/create_next_cohort_subscription_payment.sql
create or replace function create_next_cohort_subscription_payment(c cohort_subscription)
    returns setof payment
    language plpgsql
    as $$
declare
  member person;
  v_specific text;
  v_payment payment;
  created_ids bigint[] := array[]::bigint[];
begin
  if not c.active then
    return;
  end if;
  update cohort_subscription set renews_on = renews_on + interval where id = c.id;
  v_specific := '' || (extract(year from c.renews_on) - 2000) || extract(month from c.renews_on) || extract(day from c.renews_on) || c.id;

  for member in select person.* from cohort_membership join person on person.id=person_id where cohort_id=c.cohort_id and c.renews_on <@ cohort_membership.active_range loop
    insert into payment (
      status,
      specific_symbol,
      variable_symbol,
      cohort_subscription_id,
      is_auto_credit_allowed,
      due_at
    ) values (
      'unpaid',
      v_specific,
      regexp_replace(coalesce(nullif(member.tax_identification_number, ''), member.id::text), '[^0-9]', '', 'g'),
      c.id,
      current_tenant_id() <> 1,
      c.renews_on
    ) returning * into v_payment;
    created_ids := created_ids || v_payment.id;

    insert into payment_recipient (payment_id, account_id, amount)
    values (v_payment.id, c.account_id, (c.price).amount);

    insert into payment_debtor (payment_id, person_id)
    values (v_payment.id, member.id);
  end loop;

  return query select * from payment where id = any (created_ids);
end
$$;

comment on function create_next_cohort_subscription_payment is '@behavior -*';
grant all on function create_next_cohort_subscription_payment to anonymous;
--! EndIncluded functions/create_next_cohort_subscription_payment.sql
--! Included functions/create_missing_cohort_subscription_payments.sql
create or replace function create_missing_cohort_subscription_payments(c cohort_subscription)
  returns setof payment language plpgsql as $$
declare
  member person;
  v_specific text;
  v_payment payment;
  created_ids bigint[] := array[]::bigint[];
  v_renews_on timestamptz;
begin
  if not c.active then
    return;
  end if;
  select renews_on - interval into v_renews_on from cohort_subscription where id = c.id;
  v_specific := '' || (extract(year from v_renews_on) - 2000) || extract(month from v_renews_on) || extract(day from v_renews_on) || c.id;

  for member in select person.*
    from cohort_membership
    join person on person.id=person_id
    where cohort_id=c.cohort_id
      and v_renews_on <@ cohort_membership.active_range
      and not exists (
          select 1 from payment join payment_debtor on payment_id=payment.id
          where cohort_subscription_id = c.id and due_at = v_renews_on and person_id=person.id
      ) loop
    insert into payment (
      status,
      specific_symbol,
      variable_symbol,
      cohort_subscription_id,
      is_auto_credit_allowed,
      due_at
    ) values (
      'unpaid',
      v_specific,
      regexp_replace(coalesce(nullif(member.tax_identification_number, ''), member.id::text), '[^0-9]', '', 'g'),
      c.id,
      current_tenant_id() <> 1,
      v_renews_on
    ) returning * into v_payment;
    created_ids := created_ids || v_payment.id;

    insert into payment_recipient (payment_id, account_id, amount)
    values (v_payment.id, c.account_id, (c.price).amount);

    insert into payment_debtor (payment_id, person_id)
    values (v_payment.id, member.id);
  end loop;

  return query select * from payment where id = any (created_ids);
end
$$;

comment on function create_missing_cohort_subscription_payments is '@behavior -*';
grant all on function create_missing_cohort_subscription_payments to anonymous;
--! EndIncluded functions/create_missing_cohort_subscription_payments.sql
--! Included functions/archive_cohort.sql
create or replace function archive_cohort(cohort_id bigint)
  returns cohort
  language sql
as $$
  update cohort_membership
  set until = now(), status = 'expired'
  where cohort_id = $1
    and since < now()
    and (until is null or until > now());

  update cohort set is_visible = false where id = $1 returning *;
$$;

revoke all on function archive_cohort from anonymous;
grant all on function archive_cohort to administrator;
--! EndIncluded functions/archive_cohort.sql
--! Included functions/create_jwt_token.sql
create or replace function app_private.create_jwt_token(u users) returns jwt_token
    language sql stable
as $$
with
  person_ids as (
    select distinct up.person_id from user_proxy up where up.user_id = u.id and up.status = 'active'
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
--! Included functions/tg_tenant_membership__on_status.sql
create or replace function app_private.tg_tenant_membership__on_status()
  returns trigger
  language plpgsql
as $$
begin
  if new.status = 'expired' and not exists (
    select from tenant_membership where tenant_id = new.tenant_id and person_id = new.person_id and active_range @> new.until
    union all
    select from tenant_trainer where tenant_id = new.tenant_id and person_id = new.person_id and active_range @> new.until
    union all
    select from tenant_administrator where tenant_id = new.tenant_id and person_id = new.person_id and active_range @> new.until
  ) then
    update cohort_membership
    set status = 'expired', until = new.until
    where tenant_id = new.tenant_id
      and person_id = new.person_id
      and since < new.until
      and (until is null or until > new.until);
  end if;
  return new;
end;
$$;

select verify_function('app_private.tg_tenant_membership__on_status', 'tenant_membership');
select verify_function('app_private.tg_tenant_membership__on_status', 'tenant_trainer');
select verify_function('app_private.tg_tenant_membership__on_status', 'tenant_administrator');

create or replace trigger _500_on_status
  after update on tenant_membership
  for each row
  when (old.status is distinct from new.status)
  execute function app_private.tg_tenant_membership__on_status();

create or replace trigger _500_on_status
  after update on tenant_trainer
  for each row
  when (old.status is distinct from new.status)
  execute function app_private.tg_tenant_membership__on_status();

create or replace trigger _500_on_status
  after update on tenant_administrator
  for each row
  when (old.status is distinct from new.status)
  execute function app_private.tg_tenant_membership__on_status();
--! EndIncluded functions/tg_tenant_membership__on_status.sql
--! Included functions/security_events.sql
create or replace function app_private.tg_relationship__status()
  returns trigger
  language plpgsql
as $$
begin
  new.status := app_private.relationship_status_next(
    now(), tstzrange(new.since, new.until, '[)'), new.status
  );
  return new;
end;
$$;

create or replace function app_private.tg_security_event__range()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (person_id, kind, method)
      values (old.person_id, tg_argv[1]::security_event_kind, 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (person_id, kind, method, effective_at)
    values (new.person_id, tg_argv[0]::security_event_kind, 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (person_id, kind, method, effective_at)
    values (
      new.person_id,
      (case when new.status = 'active' then tg_argv[0] else tg_argv[1] end)::security_event_kind,
      (case when current_user_id() is null then 'scheduled' else 'manual' end)::security_event_method,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;

create or replace function app_private.tg_security_event__credential()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (person_id, kind, method)
      values (old.person_id, 'access_credential_ended', 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (person_id, kind, method, effective_at)
    values (new.person_id, 'access_credential_issued', 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (person_id, kind, method, effective_at)
    values (
      new.person_id,
      (case when new.status = 'active' then 'access_credential_issued' else 'access_credential_ended' end)::security_event_kind,
      (case when current_user_id() is null then 'scheduled' else 'manual' end)::security_event_method,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;

create or replace function app_private.tg_security_event__user_proxy()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (user_id, person_id, kind, method)
      values (old.user_id, old.person_id, 'person_unlinked', 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (user_id, person_id, kind, method, effective_at)
    values (new.user_id, new.person_id, 'person_linked', 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (user_id, person_id, kind, method, effective_at)
    values (
      new.user_id,
      new.person_id,
      (case when new.status = 'active' then 'person_linked' else 'person_unlinked' end)::security_event_kind,
      (case when current_user_id() is null then 'scheduled' else 'manual' end)::security_event_method,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;

create or replace function app_private.tg_security_event__password()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  insert into security_event (user_id, kind, method)
  values (new.id, 'password_changed', 'manual');
  return new;
end;
$$;

create or replace trigger _050_relationship_status
  before insert or update of since, until on user_proxy
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on couple
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on cohort_membership
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on tenant_membership
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on tenant_trainer
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on tenant_administrator
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on access_credential
  for each row execute function app_private.tg_relationship__status();

create or replace trigger _900_security_event
  after insert or update of status or delete on tenant_membership
  for each row execute function app_private.tg_security_event__range(
    'membership_granted', 'membership_revoked'
  );
create or replace trigger _900_security_event
  after insert or update of status or delete on tenant_trainer
  for each row execute function app_private.tg_security_event__range(
    'trainer_granted', 'trainer_revoked'
  );
create or replace trigger _900_security_event
  after insert or update of status or delete on tenant_administrator
  for each row execute function app_private.tg_security_event__range(
    'administrator_granted', 'administrator_revoked'
  );
create or replace trigger _900_security_event
  after insert or update of status or delete on access_credential
  for each row execute function app_private.tg_security_event__credential();
create or replace trigger _900_security_event
  after insert or update of status or delete on user_proxy
  for each row execute function app_private.tg_security_event__user_proxy();
create or replace trigger _900_security_event
  after update of u_pass on users
  for each row
  when (old.u_pass is distinct from new.u_pass)
  execute function app_private.tg_security_event__password();
--! EndIncluded functions/security_events.sql
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
      i.id as event_id,
      i.since,
      i.until,
      i.type as event_type
    from event i
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
      case when inst.event_type = 'lesson' then 1 else 0 end as lesson_score,
      case when inst.event_type = 'group' then floor((extract(epoch from (inst.until - inst.since)) / 60)::numeric / 45::numeric) else 0 end as group_score,
      case when inst.event_type = 'camp'  then 3 + 2 * ((extract(epoch from (inst.until - inst.since)) > 86400)::int) else 0 end as event_score,
      date_trunc('day', inst.since)::date as day
    from instances inst
    cross join lateral (
      select registration.person_id
      from event_registration registration
      where registration.event_id = inst.event_id
        and registration.person_id is not null
        and registration.registration_status = 'active'
        and (registration.status = 'attended' or inst.event_type = 'lesson')
      -- Keep date-filtered events first, avoiding RLS checks on registration history.
      offset 0
    ) ea
    where ea.person_id = any (select person_id from member_people)
  ),
  per_day as (
    select
      person_id,
      least(sum(lesson_score), 4) as lesson_score,
      sum(group_score) as group_score,
      sum(event_score) as event_score
    from attendance
    group by person_id, day
  ),
  aggregated as (
    select
      person_id,
      coalesce(sum(lesson_score), 0)::bigint as lesson_total_score,
      coalesce(sum(group_score), 0)::bigint as group_total_score,
      coalesce(sum(event_score), 0)::bigint as event_total_score
    from per_day
    group by person_id
  ),
  manual as (
    select
      sma.person_id,
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
    group by sma.person_id
  ),
  totals as (
    select
      person_id,
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
    full join manual m using (person_id)
  )
  select
    person_id,
    scoreboard_entries.cohort_id as cohort_id,
    lesson_total_score,
    group_total_score,
    event_total_score,
    manual_total_score,
    total_score,
    rank() over (order by total_score desc, person_id) as ranking
  from totals
  order by total_score desc, person_id;
$$;

comment on function scoreboard_entries is '@behavior +list -connection';

grant all on function scoreboard_entries to anonymous;
--! EndIncluded functions/scoreboard.sql
--! Included functions/refresh_wdsf_round_dances.sql
create or replace function app_private.refresh_wdsf_round_dances(p_competition_id bigint)
returns integer
language plpgsql
as $$
declare
  preferred_order text[];
  r record;
  dance_codes text[];
  old_orders integer[];
  program_codes text[];
  target_program_code text;
  target_program_id bigint;
  changed_rounds integer := 0;
begin
  select case
    when cat.discipline in ('SMOOTH', 'PD SMOOTH') then array['SW', 'TG', 'SF', 'VW']
    when cat.discipline = 'RHYTHM' then array['CH', 'RU', 'SG', 'BO', 'MA']
    else array['SW', 'TG', 'VW', 'SF', 'QS', 'SA', 'CH', 'RU', 'PD', 'JI']
  end into preferred_order
  from federated.competition c join federated.category cat on cat.id = c.category_id
  where c.id = p_competition_id and c.federation = 'wdsf'
  for no key update of c;
  if not found then return 0; end if;

  for r in
    with dances as (
      select cr.id, dp.code as program_code, rd.dance_code, rd.dance_order,
        exists (
          select 1 from federated.judge_score js
          where (js.round_id, js.dance_order, js.dance_code)
            = (rd.round_id, rd.dance_order, rd.dance_code)
        ) as has_scores
      from federated.competition_round cr
      join federated.dance_program dp on dp.id = cr.dance_program_id
      left join federated.round_dance rd on rd.round_id = cr.id
      where cr.competition_id = p_competition_id
    )
    select id, dances.program_code,
      coalesce(array_agg(dance_code order by dance_order)
        filter (where dance_code is not null), '{}') as current_codes,
      coalesce(array_agg(dance_order order by dance_order)
        filter (where dance_order is not null), '{}') as current_orders,
      coalesce(array_agg(dance_code order by dance_order)
        filter (where has_scores), '{}') as scored_codes,
      coalesce(array_agg(dance_order order by dance_order)
        filter (where has_scores), '{}') as scored_orders
    from dances
    group by id, dances.program_code
    order by id
  loop
    dance_codes := r.scored_codes;
    old_orders := r.scored_orders;
    if dance_codes <@ preferred_order then
      select coalesce(array_agg(code order by array_position(preferred_order, code), old_order), '{}'),
        coalesce(array_agg(old_order order by array_position(preferred_order, code), old_order), '{}')
      into dance_codes, old_orders
      from unnest(dance_codes, old_orders) as d(code, old_order);
    end if;

    select coalesce(array_agg(code order by position), '{}') into program_codes
    from (
      select code, min(position) as position
      from unnest(dance_codes) with ordinality as d(code, position)
      group by code
    ) as unique_dances;
    target_program_code := array_to_string(program_codes, '-');
    if r.program_code is not distinct from target_program_code
      and r.current_codes = dance_codes
      and r.current_orders = array(select generate_series(1, cardinality(dance_codes)))
    then
      continue;
    end if;

    insert into federated.dance_program (code, name)
    values (target_program_code, array_to_string(program_codes, ' '))
    on conflict (code) do nothing
    returning id into target_program_id;
    if target_program_id is null then
      select id into strict target_program_id from federated.dance_program where code = target_program_code;
    end if;
    insert into federated.dance_program_dance (program_id, dance_code, dance_order)
    select target_program_id, code, position::integer
    from unnest(program_codes) with ordinality as d(code, position)
    on conflict (program_id, dance_code) do nothing;

    delete from federated.round_dance rd
    where rd.round_id = r.id
      and not exists (select 1 from federated.judge_score js
                      where (js.round_id, js.dance_order) = (rd.round_id, rd.dance_order));
    update federated.competition_round set dance_program_id = target_program_id
    where id = r.id and dance_program_id <> target_program_id;

    if old_orders <> array(select generate_series(1, cardinality(dance_codes))) then
      -- Swap dance positions without key collisions. Check score references
      -- after both the dance positions and their scores agree again.
      set constraints federated.judge_score_round_id_dance_order_dance_code_fkey deferred;
      update federated.round_dance rd set dance_order = -rd.dance_order
      from unnest(old_orders) with ordinality as d(old_order, position)
      where rd.round_id = r.id and rd.dance_order = d.old_order and d.old_order <> d.position;
      update federated.round_dance rd set dance_order = d.position::integer
      from unnest(old_orders) with ordinality as d(old_order, position)
      where rd.round_id = r.id and rd.dance_order = -d.old_order and d.old_order <> d.position;

      with moved as materialized (
        select js.round_id, d.position::integer as dance_order, js.dance_code,
          js.judge_person_id, js.competitor_id, js.component, js.score, js.raw_score
        from federated.judge_score js
        join unnest(old_orders) with ordinality as d(old_order, position) on d.old_order = js.dance_order
        where js.round_id = r.id and d.old_order <> d.position
      ), removed as (
        delete from federated.judge_score js
        where js.round_id = r.id
          and js.dance_order in (select old_order from unnest(old_orders) with ordinality as d(old_order, position)
                                where old_order <> position)
          and not exists (select 1 from moved m where
            (m.dance_order, m.judge_person_id, m.competitor_id, m.component)
            = (js.dance_order, js.judge_person_id, js.competitor_id, js.component))
      )
      insert into federated.judge_score as target (
        round_id, dance_order, dance_code, judge_person_id, competitor_id, component, score, raw_score
      )
      select * from moved
      on conflict (round_id, dance_order, judge_person_id, competitor_id, component) do update
      set dance_code = excluded.dance_code, score = excluded.score, raw_score = excluded.raw_score
      where (target.dance_code, target.score, target.raw_score)
        is distinct from (excluded.dance_code, excluded.score, excluded.raw_score);
      set constraints federated.judge_score_round_id_dance_order_dance_code_fkey immediate;
    end if;
    changed_rounds := changed_rounds + 1;
  end loop;
  return changed_rounds;
end;
$$;
--! EndIncluded functions/refresh_wdsf_round_dances.sql
--! Included functions/refresh_competition_round_results.sql
create or replace function app_private.refresh_competition_round_results(p_competition_id bigint)
returns void language sql volatile as $$
WITH scoped_rounds AS (
  SELECT id, round_key, round_index, scoring_method
  FROM federated.competition_round
  WHERE competition_id = p_competition_id
), presence AS (
  SELECT
    sr.id AS round_id,
    sr.round_index,
    sr.scoring_method,
    cr.competitor_id,
    competitor.external_id AS competitor_external_id,
    cr.ranking,
    cr.ranking_to,
    last_round.round_index AS last_round_index
  FROM federated.competition_result cr
  JOIN federated.competitor competitor ON competitor.id = cr.competitor_id
  JOIN scoped_rounds last_round ON last_round.round_key = cr.last_round
  JOIN scoped_rounds sr ON sr.round_index <= last_round.round_index
  WHERE cr.competition_id = p_competition_id
), scores AS MATERIALIZED (
  SELECT js.round_id, js.dance_order, js.dance_code, js.competitor_id, sum(js.score)::numeric(10,3) AS score
  FROM federated.judge_score js
  JOIN scoped_rounds sr ON sr.id = js.round_id
  GROUP BY js.round_id, js.dance_order, js.dance_code, js.competitor_id
), dance_scores AS (
  SELECT
    p.round_id,
    p.round_index,
    p.scoring_method,
    p.competitor_id,
    p.ranking,
    p.ranking_to,
    rd.dance_order,
    CASE
      WHEN p.scoring_method = 'skating_marks' THEN coalesce(js.score, 0)::numeric(10,3)
      ELSE js.score
    END AS dance_score
  FROM presence p
  JOIN federated.round_dance rd ON rd.round_id = p.round_id
  LEFT JOIN scores js
    ON js.round_id = p.round_id
   AND js.dance_order = rd.dance_order
   AND js.dance_code = rd.dance_code
   AND js.competitor_id = p.competitor_external_id
), round_scores AS (
  SELECT
    ds.round_id,
    ds.round_index,
    ds.scoring_method,
    ds.competitor_id,
    ds.ranking,
    ds.ranking_to,
    CASE
      WHEN ds.scoring_method = 'skating_marks'
        OR count(ds.dance_score) FILTER (WHERE ds.dance_score IS NOT NULL) > 0
      THEN sum(ds.dance_score)::numeric(10,3)
      ELSE NULL
    END AS overall_score,
    CASE
      WHEN ds.scoring_method = 'skating_marks'
        OR count(ds.dance_score) FILTER (WHERE ds.dance_score IS NOT NULL) > 0
      THEN array_agg(ds.dance_score::real ORDER BY ds.dance_order)
      ELSE NULL
    END AS dance_results
  FROM dance_scores ds
  GROUP BY
    ds.round_id, ds.round_index, ds.scoring_method, ds.competitor_id,
    ds.ranking, ds.ranking_to
), ranked AS (
  SELECT
    round_scores.*,
    CASE
      WHEN scoring_method = 'skating_marks' AND overall_score IS NOT NULL THEN
        rank() OVER (PARTITION BY round_id ORDER BY overall_score DESC NULLS LAST)::int
      WHEN scoring_method = 'skating_places' THEN
        ranking
      ELSE NULL
    END AS overall_ranking,
    CASE
      WHEN scoring_method = 'skating_marks' AND overall_score IS NOT NULL THEN
        count(*) OVER (PARTITION BY round_id, overall_score)::int
      ELSE NULL
    END AS tie_count
  FROM round_scores
), source AS (
  SELECT
    ranked.round_id,
    ranked.competitor_id,
    ranked.overall_ranking,
    CASE
      WHEN ranked.overall_ranking IS NULL THEN NULL
      WHEN ranked.scoring_method = 'skating_places' THEN
        coalesce(ranked.ranking_to, ranked.ranking)
      ELSE ranked.overall_ranking + ranked.tie_count - 1
    END AS overall_ranking_to,
    EXISTS (
      SELECT 1
      FROM presence later
      WHERE later.competitor_id = ranked.competitor_id
        AND later.last_round_index > ranked.round_index
    ) AS qualified_next,
    ranked.overall_score,
    ranked.dance_results
  FROM ranked
), upserted AS (
  INSERT INTO federated.competition_round_result (
    round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score, dance_results
  )
  SELECT
    round_id, competitor_id, overall_ranking, overall_ranking_to,
    qualified_next, overall_score, dance_results
  FROM source
  WHERE NOT EXISTS (
    SELECT 1 FROM federated.competition_round_result r
    WHERE (r.round_id, r.competitor_id) = (source.round_id, source.competitor_id)
      AND (r.overall_ranking, r.overall_ranking_to, r.qualified_next, r.overall_score, r.dance_results)
        IS NOT DISTINCT FROM (source.overall_ranking, source.overall_ranking_to,
          source.qualified_next, source.overall_score, source.dance_results)
  )
  ORDER BY round_id, competitor_id
  ON CONFLICT (round_id, competitor_id) DO UPDATE
    SET overall_ranking = EXCLUDED.overall_ranking,
        overall_ranking_to = EXCLUDED.overall_ranking_to,
        qualified_next = EXCLUDED.qualified_next,
        overall_score = EXCLUDED.overall_score,
        dance_results = EXCLUDED.dance_results
    WHERE federated.competition_round_result.overall_ranking IS DISTINCT FROM EXCLUDED.overall_ranking
       OR federated.competition_round_result.overall_ranking_to IS DISTINCT FROM EXCLUDED.overall_ranking_to
       OR federated.competition_round_result.qualified_next IS DISTINCT FROM EXCLUDED.qualified_next
       OR federated.competition_round_result.overall_score IS DISTINCT FROM EXCLUDED.overall_score
       OR federated.competition_round_result.dance_results IS DISTINCT FROM EXCLUDED.dance_results
  RETURNING 1
), touched AS (
  SELECT count(*) FROM upserted
)
DELETE FROM federated.competition_round_result crr
USING scoped_rounds sr, touched
WHERE crr.round_id = sr.id
  AND NOT EXISTS (
    SELECT 1
    FROM source
    WHERE source.round_id = crr.round_id
      AND source.competitor_id = crr.competitor_id
  );
$$;
--! EndIncluded functions/refresh_competition_round_results.sql

drop function if exists federated.competitor_result_highlights;
drop function if exists federated.person_result_highlights;
drop function if exists federated.person_result_highlights;
drop function if exists app_private.result_highlights;
drop function if exists federated.result_highlights;
drop type if exists federated.result_highlight;

comment on table federated.person is '@name federation_person
@behavior -insert -update -delete -list
@ref people via:public.person_federation_link;public.person behavior:"+list -connection"
@ref competitionResults via:federated.competitor_component;federated.competitor;federated.competition_result behavior:"+list -connection"';

comment on constraint competition_result_competitor_id_fkey on federated.competition_result is null;

comment on table federated.competitor is '@name federation_competitor
@behavior -insert -update -delete -list
@ref people via:federated.competitor_component;federated.person;public.person_federation_link;public.person behavior:"+list -connection"
@ref couples via:public.couple_federation_link;public.couple
@ref competitions via:federated.competition_participation;federated.competition
@ref competitionResults via:federated.competition_result behavior:"+list -connection"';

do $$
begin
  perform app_private.refresh_event_stats(id) from event where type = 'group' order by id;
end
$$;

alter table federated.judge_score
  alter constraint judge_score_round_id_dance_order_dance_code_fkey deferrable initially immediate;

comment on table posting is '@behavior +list +connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';

comment on table payment is '@behavior +list +connection -insert -query:resource:list +query:resource:connection';

do $$
begin
  create type payment_kind as enum ('event', 'cohort-subscription', 'other');
exception when duplicate_object then null;
end
$$;

alter table payment add column if not exists kind payment_kind
  generated always as (
    case
      when event_id is not null then 'event'::payment_kind
      when cohort_subscription_id is not null then 'cohort-subscription'::payment_kind
      else 'other'::payment_kind
    end
  ) stored;

update federated.competition c
set competition_type = 'ranking'
from federated.category cat
where cat.id = c.category_id
  and c.federation = 'wdsf'
  and cat.class = 'GRAND SLAM'
  and c.competition_type is distinct from 'ranking'::federated.competition_type;

insert into crawler.frontier (federation, kind, key)
  select federation, 'competitionEventIndex' as kind, key
  from crawler.frontier
  where federation = 'csts' and kind = 'eventIndex'
  on conflict (federation, kind, key) do nothing;

insert into crawler.rate_limit_rule (host, max_requests, per_interval)
values ('szts.ksis.eu', 1, interval '5 seconds')
on conflict (host) do nothing;

do $$
declare competition_ids bigint[];
declare competition_id bigint;
begin
  with corrected as (
    update federated.competition_result result
    set last_round = reached.name, last_dance = reached.dance
    from crawler.frontier frontier
    join crawler.json_response response on response.id = frontier.last_successful_response_id
    join crawler.json_response_cache cache on cache.content_hash = response.content_hash
    join federated.competition competition
      on competition.federation = 'wdsf' and competition.external_id = cache.content->>'competitionId'
    cross join lateral (
      select round.value->>'name' as name, round.value->'dances'->-1->>'name' as dance
      from jsonb_array_elements(cache.content->'rounds') with ordinality as round(value, position)
      order by case when round.value->>'name' ~* '^f(inal)?$' then 2
        when round.value->>'name' = 'R' then 0 else 1 end desc, position desc
      limit 1
    ) reached
    where frontier.federation = 'wdsf' and frontier.kind = 'participant'
      and result.competition_id = competition.id
      and result.competitor_id = 'wdsf:' || (cache.content->>'coupleId')
      and result.last_round = 'R' and reached.name <> 'R'
    returning result.competition_id
  )
  select array_agg(distinct corrected.competition_id) into competition_ids from corrected;

  foreach competition_id in array coalesce(competition_ids, '{}'::bigint[]) loop
    perform app_private.refresh_competition_round_results(competition_id);
  end loop;
end $$;

alter table federated.person add column if not exists club_id bigint references federated.federation_club(id);

create index if not exists person_club_id_idx on federated.person(club_id);
comment on constraint person_club_id_fkey on federated.person is '@fieldName club
@behavior -manyRelation:resource:connection -manyRelation:resource:list';

alter table federated.competition_entry
  add column if not exists club_id bigint references federated.federation_club(id),
  add column if not exists represents text,
  add column if not exists country text;

create index if not exists competition_entry_club_id_idx on federated.competition_entry(club_id);
comment on constraint competition_entry_club_id_fkey on federated.competition_entry is '@fieldName club
@behavior -manyRelation:resource:connection -manyRelation:resource:list';

alter table federated.competition_result
  add column if not exists club_id bigint references federated.federation_club(id),
  add column if not exists represents text,
  add column if not exists country text;

create index if not exists competition_result_club_id_idx on federated.competition_result(club_id);
comment on constraint competition_result_club_id_fkey on federated.competition_result is '@fieldName club
@behavior -manyRelation:resource:connection -manyRelation:resource:list';

insert into crawler.frontier (federation, kind, key)
select 'csts', 'memberProfile', external_id from federated.person where federation = 'csts'
on conflict (federation, kind, key) do nothing;

alter table federated.event add column if not exists extra_id text;

alter table federated.competitor
  add column if not exists active boolean,
  add column if not exists since timestamptz,
  add column if not exists until timestamptz;

alter table tenant add column if not exists csts_id integer;
grant update (csts_id) on tenant to administrator;

create index if not exists payment_debtor_tenant_person_payment_idx
  on payment_debtor (tenant_id, person_id, payment_id);

--! Included functions/payment_facets.sql
create or replace function app_private.payment_search_ids(search text)
returns setof bigint language sql stable as $$
  with term as not materialized (
    select lower(trim(search)) as value
  )
  select p.id from payment p, term
  where strpos(p.id::text, term.value) > 0
    or strpos(lower(p.variable_symbol), term.value) > 0
    or strpos(lower(p.specific_symbol), term.value) > 0
  union all
  select p.id from payment p
  join event e on e.id = p.event_id, term
  where strpos(lower(e.name), term.value) > 0
  union all
  select p.id from payment p
  join cohort_subscription cs on cs.id = p.cohort_subscription_id
  join cohort c on c.id = cs.cohort_id, term
  where strpos(lower(c.name), term.value) > 0
  union all
  select d.payment_id from payment_debtor d
  join person p on p.id = d.person_id, term
  where strpos(lower(concat_ws(' ', p.first_name, p.last_name)), term.value) > 0;
$$;
grant execute on function app_private.payment_search_ids to anonymous;

create or replace function payment_facets(
  only_status payment_status default null,
  only_kind payment_kind default null,
  person_ids bigint[] default null,
  search text default null
) returns table(facet text, value text, count integer, label text)
language sql stable as $$
  with candidates as materialized (
    select p.id, p.status, p.kind,
      person_ids is null or p.id in (
        select payment_id from payment_debtor where person_id = any(person_ids)
      ) as person_matches
    from payment p
    where nullif(trim(search), '') is null
      or p.id in (select * from app_private.payment_search_ids(search))
  ), summary as (
    select p.status, p.kind, count(*) as count
    from candidates p where p.person_matches
    group by p.status, p.kind
  ), person_counts as (
    select d.person_id, count(distinct d.payment_id)::integer as count
    from payment_debtor d
    -- Keep the index order for distinct counts instead of joining and sorting debtors.
    where (d.payment_id in (
      select p.id from candidates p
      where (only_status is null or p.status = only_status)
        and (only_kind is null or p.kind = only_kind)
    )) is true
    group by d.person_id
  )
  select 'status', coalesce(upper(p.status::text), ''), coalesce(sum(p.count), 0)::integer, null::text
  from summary p
  where only_kind is null or p.kind = only_kind
  group by rollup(p.status)
  union all
  select 'kind', coalesce(upper(replace(p.kind::text, '-', '_')), ''), coalesce(sum(p.count), 0)::integer, null::text
  from summary p
  where only_status is null or p.status = only_status
  group by rollup(p.kind)
  union all
  select 'person', p.id::text, coalesce(c.count, 0), p.name
  from person p left join person_counts c on c.person_id = p.id
  where c.person_id is not null or p.id = any(person_ids);
$$;
comment on function payment_facets is '@behavior +list -connection';
grant execute on function payment_facets to anonymous;
--! EndIncluded functions/payment_facets.sql
--! Included functions/transaction_facets.sql
create or replace function app_private.transaction_search_ids(search text)
returns setof bigint language sql stable as $$
  with term as not materialized (
    select lower(trim(search)) as value
  )
  select t.id from transaction t, term
  where strpos(lower(t.description), term.value) > 0
    or t.payment_id in (
      select p.id from payment p
      where strpos(lower(p.variable_symbol), term.value) > 0
        or strpos(lower(p.specific_symbol), term.value) > 0
    );
$$;
grant execute on function app_private.transaction_search_ids to anonymous;

create or replace function transaction_facets(
  since timestamptz default null,
  until timestamptz default null,
  sources transaction_source[] default null,
  account_ids bigint[] default null,
  search text default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with candidates as materialized (
    select t.id, t.source,
      account_ids is null or t.id in (
        select transaction_id from posting where account_id = any(account_ids)
      ) as account_matches
    from transaction t
    where (since is null or t.effective_date >= since)
      and (until is null or t.effective_date < until)
      and (nullif(trim(search), '') is null
        or t.id in (select * from app_private.transaction_search_ids(search)))
  )
  select 'source', upper(replace(t.source::text, '-', '_')), count(*)::integer
  from candidates t where t.account_matches
  group by t.source
  union all
  select 'account', account_id::text, count(*)::integer
  from (
    select distinct p.account_id, t.id
    from candidates t join posting p on p.transaction_id = t.id
    where sources is null or t.source = any(sources)
  ) accounts
  group by account_id;
$$;
comment on function transaction_facets is '@behavior +list -connection';
grant execute on function transaction_facets to anonymous;
--! EndIncluded functions/transaction_facets.sql
