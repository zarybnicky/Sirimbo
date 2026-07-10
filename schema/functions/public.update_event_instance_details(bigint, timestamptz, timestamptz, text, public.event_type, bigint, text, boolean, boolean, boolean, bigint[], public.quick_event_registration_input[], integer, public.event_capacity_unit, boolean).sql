CREATE FUNCTION public.update_event_instance_details(p_instance_id bigint, p_since timestamp with time zone, p_until timestamp with time zone, p_name text, p_type public.event_type, p_location_id bigint, p_location_text text, p_is_visible boolean, p_is_public boolean, p_is_cancelled boolean, p_trainer_person_ids bigint[] DEFAULT NULL::bigint[], p_registrations public.quick_event_registration_input[] DEFAULT NULL::public.quick_event_registration_input[], p_capacity integer DEFAULT NULL::integer, p_capacity_unit public.event_capacity_unit DEFAULT NULL::public.event_capacity_unit, p_is_locked boolean DEFAULT NULL::boolean) RETURNS public.event_instance
    LANGUAGE plpgsql
    AS $$
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
    capacity = coalesce(p_capacity, capacity),
    capacity_unit = coalesce(p_capacity_unit, capacity_unit),
    is_locked = coalesce(p_is_locked, is_locked),
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

GRANT ALL ON FUNCTION public.update_event_instance_details(p_instance_id bigint, p_since timestamp with time zone, p_until timestamp with time zone, p_name text, p_type public.event_type, p_location_id bigint, p_location_text text, p_is_visible boolean, p_is_public boolean, p_is_cancelled boolean, p_trainer_person_ids bigint[], p_registrations public.quick_event_registration_input[], p_capacity integer, p_capacity_unit public.event_capacity_unit, p_is_locked boolean) TO anonymous;
