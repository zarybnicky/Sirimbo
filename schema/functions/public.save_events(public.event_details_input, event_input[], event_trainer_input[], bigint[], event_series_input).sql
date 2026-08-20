CREATE FUNCTION public.save_events(details public.event_details_input, events public.event_input[], trainers public.event_trainer_input[] DEFAULT '{}'::public.event_trainer_input[], cohort_ids bigint[] DEFAULT '{}'::bigint[], series public.event_series_input DEFAULT NULL::public.event_series_input) RETURNS SETOF public.event_instance
    LANGUAGE plpgsql
    AS $$
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

COMMENT ON FUNCTION public.save_events(details public.event_details_input, events public.event_input[], trainers public.event_trainer_input[], cohort_ids bigint[], series public.event_series_input) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.save_events(details public.event_details_input, events public.event_input[], trainers public.event_trainer_input[], cohort_ids bigint[], series public.event_series_input) TO anonymous;
