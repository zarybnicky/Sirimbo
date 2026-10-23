CREATE FUNCTION public.event_registration_info(inst public.event) RETURNS public.event_registration_info
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select row(
    count(*) filter (where parent_registration_id is null)::integer,
    count(person_id)::integer,
    case
      when inst.capacity is null or inst.capacity <= 0 then null
      else inst.capacity
        - case inst.capacity_unit
            when 'people' then count(person_id)::integer
            when 'registrations' then count(*) filter (where parent_registration_id is null)::integer
          end
    end,
    count(*) filter (where person_id = any(current_person_ids())) > 0
  )::event_registration_info
  from event_registration
  where event_id = inst.id and registration_status = 'active';
$$;

GRANT ALL ON FUNCTION public.event_registration_info(inst public.event) TO anonymous;
