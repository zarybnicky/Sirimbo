create or replace function public.set_event_instance_registration(
  p_instance_id bigint,
  p_person_id bigint,
  p_couple_id bigint,
  p_is_registered boolean
) returns public.event_instance_registration
  language plpgsql
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
declare
  target_instance event_instance;
  registration event_instance_registration;
  required_capacity integer;
  remaining_capacity integer;
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

  select * into target_instance
  from event_instance
  where id = p_instance_id
    and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;
  if target_instance.event_id is not null or target_instance.is_locked then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  select * into registration
  from event_instance_registration
  where instance_id = p_instance_id
    and parent_registration_id is null
    and legacy_registration_id is null
    and person_id is not distinct from p_person_id
    and couple_id is not distinct from p_couple_id;

  if not p_is_registered then
    if registration is null then
      raise exception 'REGISTRATION_NOT_FOUND' using errcode = '22023';
    end if;

    update event_instance_registration
    set registration_status = 'cancelled'
    where id = registration.id or parent_registration_id = registration.id;

    select * into registration from event_instance_registration where id = registration.id;
    return registration;
  end if;

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

  if registration is not null and registration.registration_status = 'active' then
    return registration;
  end if;

  remaining_capacity := event_instance_remaining_person_spots(target_instance);
  required_capacity := case
    when target_instance.capacity_unit = 'people' and p_couple_id is not null then 2
    else 1
  end;
  if remaining_capacity is not null and remaining_capacity < required_capacity then
    raise exception 'CAPACITY_EXCEEDED' using errcode = '22023';
  end if;

  if registration is not null then
    update event_instance_registration
    set registration_status = 'active'
    where id = registration.id or parent_registration_id = registration.id;
  else
    insert into event_instance_registration (
      instance_id, person_id, couple_id, status
    ) values (
      p_instance_id, p_person_id, p_couple_id,
      case when p_person_id is not null then 'unknown'::attendance_type end
    ) returning * into registration;

    insert into event_instance_registration (
      instance_id, parent_registration_id, person_id, status
    )
    select p_instance_id, registration.id, person.person_id, 'unknown'
    from couple
    cross join lateral unnest(array[couple.man_id, couple.woman_id]) person(person_id)
    where couple.id = p_couple_id;
  end if;

  select * into registration from event_instance_registration where id = registration.id;
  return registration;
end;
$$;

select verify_function('public.set_event_instance_registration');
grant execute on function public.set_event_instance_registration(bigint, bigint, bigint, boolean)
  to anonymous;
