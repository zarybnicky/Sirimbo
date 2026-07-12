CREATE FUNCTION public.update_event_instance_details(p_instance_id bigint, p_since timestamp with time zone, p_until timestamp with time zone, p_name text, p_type public.event_type, p_location_id bigint, p_location_text text, p_is_visible boolean, p_is_public boolean, p_is_cancelled boolean, p_trainer_person_ids bigint[] DEFAULT NULL::bigint[], p_registrations public.quick_event_registration_input[] DEFAULT NULL::public.quick_event_registration_input[], p_capacity integer DEFAULT NULL::integer, p_capacity_unit public.event_capacity_unit DEFAULT NULL::public.event_capacity_unit, p_is_locked boolean DEFAULT NULL::boolean, p_trainer_lessons_offered integer[] DEFAULT NULL::integer[], p_cohort_ids bigint[] DEFAULT NULL::bigint[], p_enable_notes boolean DEFAULT NULL::boolean, p_copies public.quick_event_input[] DEFAULT NULL::public.quick_event_input[]) RETURNS public.event_instance
    LANGUAGE plpgsql
    AS $$
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

GRANT ALL ON FUNCTION public.update_event_instance_details(p_instance_id bigint, p_since timestamp with time zone, p_until timestamp with time zone, p_name text, p_type public.event_type, p_location_id bigint, p_location_text text, p_is_visible boolean, p_is_public boolean, p_is_cancelled boolean, p_trainer_person_ids bigint[], p_registrations public.quick_event_registration_input[], p_capacity integer, p_capacity_unit public.event_capacity_unit, p_is_locked boolean, p_trainer_lessons_offered integer[], p_cohort_ids bigint[], p_enable_notes boolean, p_copies public.quick_event_input[]) TO anonymous;
