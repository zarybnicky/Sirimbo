CREATE FUNCTION public.event_instance_remaining_person_spots(inst public.event_instance) RETURNS integer
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'pg_temp'
    AS $$
  select case
    when inst.capacity is null or inst.capacity <= 0 then null
    else inst.capacity - case inst.capacity_unit
        when 'people' then (
          select count(*)::integer
          from public.event_instance_registration person_row
          where person_row.instance_id = inst.id
            and person_row.person_id is not null
            and person_row.registration_status = 'active'
            and person_row.status <> 'cancelled'
        )
        when 'registrations' then (
          select count(*)::integer
          from public.event_instance_registration root
          where root.instance_id = inst.id
            and root.parent_registration_id is null
            and root.registration_status = 'active'
        )
      end - (
      select count(*)::integer
      from public.event_external_registration external_registration
      where external_registration.event_id = inst.event_id
    )
  end;
$$;

GRANT ALL ON FUNCTION public.event_instance_remaining_person_spots(inst public.event_instance) TO anonymous;
