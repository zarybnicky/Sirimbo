drop function if exists public.quick_create_event_instances;

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

grant execute on function quick_create_event_instances to anonymous;
