CREATE FUNCTION public.event_remaining_person_spots(inst public.event) RETURNS integer
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'public', 'pg_catalog', 'pg_temp'
    AS $$
  select case
    when inst.capacity is null or inst.capacity <= 0 then null
    else inst.capacity - case inst.capacity_unit
        when 'people' then (
          select count(*)::integer
          from event_registration registration
          where registration.event_id = inst.id
            and registration.person_id is not null
            and registration.registration_status = 'active'
        )
        when 'registrations' then (
          select count(*)::integer
          from event_registration registration
          where registration.event_id = inst.id
            and registration.parent_registration_id is null
            and registration.registration_status = 'active'
        )
      end
  end;
$$;

GRANT ALL ON FUNCTION public.event_remaining_person_spots(inst public.event) TO anonymous;
