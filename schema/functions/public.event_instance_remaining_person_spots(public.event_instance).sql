CREATE FUNCTION public.event_instance_remaining_person_spots(inst public.event_instance) RETURNS integer
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'public', 'pg_catalog', 'pg_temp'
    AS $$
  select case
    when inst.capacity is null or inst.capacity <= 0 then null
    else inst.capacity - case inst.capacity_unit
        when 'people' then (
          select count(*)::integer
          from event_instance_registration registration
          where registration.instance_id = inst.id
            and registration.person_id is not null
            and registration.registration_status = 'active'
            and registration.status <> 'cancelled'
        )
        when 'registrations' then (
          select count(*)::integer
          from event_instance_registration registration
          where registration.instance_id = inst.id
            and registration.parent_registration_id is null
            and registration.registration_status = 'active'
        )
      end - (
        select count(*)::integer
        from event_external_registration external_registration
        where external_registration.instance_id = inst.id
      )
  end;
$$;

GRANT ALL ON FUNCTION public.event_instance_remaining_person_spots(inst public.event_instance) TO anonymous;
