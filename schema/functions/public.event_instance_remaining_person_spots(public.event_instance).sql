CREATE FUNCTION public.event_instance_remaining_person_spots(inst public.event_instance) RETURNS integer
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  select inst.capacity
    - (select coalesce(count(*), 0) from event_instance_registration eir
        where eir.instance_id = inst.id and eir.person_id is not null and eir.status is distinct from 'cancelled')
    - (select coalesce(count(id), 0) from event_external_registration
        where event_id = inst.event_id);
$$;

GRANT ALL ON FUNCTION public.event_instance_remaining_person_spots(inst public.event_instance) TO anonymous;
