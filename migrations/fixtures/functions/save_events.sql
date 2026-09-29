drop function save_events;
drop type event_details_input;
drop type event_input;
drop type event_registration_input;

create type event_registration_input as (
  person_id bigint,
  couple_id bigint,
  target_cohort_id bigint,
  is_cancelled boolean
);

create type event_input as (
  id bigint,
  since timestamptz,
  until timestamptz,
  is_cancelled boolean,
  registrations event_registration_input[]
);

create type event_details_input as (
  parent_id bigint,
  name text,
  type event_type,
  location_id bigint,
  location_text text,
  capacity integer,
  capacity_unit event_capacity_unit,
  is_visible boolean,
  is_public boolean,
  has_public_details boolean,
  is_locked boolean,
  enable_notes boolean
);

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
    select 1 from unnest(events) i
    cross join lateral unnest(i.registrations) registration
    where registration.target_cohort_id is not null and (
      registration.person_id is null or registration.is_cancelled
      or not registration.target_cohort_id = any(coalesce(cohort_ids, '{}'))
    )
  ) then
    raise exception 'a cohort registration requires an active person and a selected cohort' using errcode = '22023';
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

    -- Release person slots before replacing couples with individuals or vice versa.
    with removed as (
      select registration.id
      from event_instance_registration registration
      where registration.instance_id = v_saved_event.id
        and registration.parent_registration_id is null
        and not exists (
          select 1 from unnest(event_to_save.registrations) desired
          where desired.person_id is not distinct from registration.person_id
            and desired.couple_id is not distinct from registration.couple_id
            and not coalesce(desired.is_cancelled, false)
        )
    )
    update event_instance_registration registration
    set registration_status = 'cancelled',
        source = case when registration.parent_registration_id is null and registration.source is distinct from 'cohort'
          then 'manager'::event_registration_source else registration.source end
    from removed
    where registration.registration_status = 'active'
      and (registration.id = removed.id or registration.parent_registration_id = removed.id);

    -- Save the displayed list and explicit removals; reuse rows to preserve attendance.
    insert into event_instance_registration as registration (
      instance_id, person_id, couple_id, target_cohort_id, source, registration_status, status
    )
    select v_saved_event.id, choice.person_id, choice.couple_id, choice.target_cohort_id,
      case when choice.target_cohort_id is null then 'manager'::event_registration_source else 'cohort'::event_registration_source end,
      case when choice.is_cancelled then 'cancelled'::event_instance_registration_status else 'active'::event_instance_registration_status end,
      case when choice.person_id is not null then 'unknown'::attendance_type end
    from (
      select distinct on (choice.person_id, choice.couple_id) choice.*
      from unnest(event_to_save.registrations) with ordinality
        choice(person_id, couple_id, target_cohort_id, is_cancelled, position)
      order by choice.person_id, choice.couple_id, choice.position
    ) choice
    on conflict (instance_id, couple_id, person_id) where parent_registration_id is null
    do update set registration_status = excluded.registration_status,
      source = excluded.source,
      target_cohort_id = excluded.target_cohort_id
    where (registration.registration_status, registration.target_cohort_id)
      is distinct from (excluded.registration_status, excluded.target_cohort_id);

    -- Couple attendance follows the couple's registration state.
    update event_instance_registration child
    set registration_status = parent.registration_status
    from event_instance_registration parent
    where parent.instance_id = v_saved_event.id
      and child.parent_registration_id = parent.id
      and child.registration_status is distinct from parent.registration_status;

    insert into event_instance_registration (instance_id, parent_registration_id, person_id, status)
    select v_saved_event.id, registration.id, person.person_id, 'unknown'
    from event_instance_registration registration
    join couple on couple.id = registration.couple_id
    cross join lateral unnest(array[couple.man_id, couple.woman_id]) person(person_id)
    where registration.instance_id = v_saved_event.id
      and registration.registration_status = 'active'
      and not exists (
        select 1 from event_instance_registration child
        where child.parent_registration_id = registration.id and child.person_id = person.person_id
      );
    insert into event_instance_target_cohort (tenant_id, instance_id, cohort_id)
    select v_tenant_id, v_saved_event.id, id
    from unnest(cohort_ids) cohort(id)
    where id is not null
    on conflict (instance_id, cohort_id) do nothing;

    delete from event_instance_target_cohort target
    where target.instance_id = v_saved_event.id
      and not exists (select 1 from unnest(cohort_ids) cohort(id) where id = target.cohort_id);

  end loop;

  -- Keep the caller's trainer assignment until all edits and replacements are saved.
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

  delete from event_instance_trainer e
  where e.instance_id = any(v_saved_event_ids)
    and not exists (
      select 1
      from unnest(coalesce(trainers, '{}'::event_trainer_input[])) trainer
      where trainer.person_id = e.person_id
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
