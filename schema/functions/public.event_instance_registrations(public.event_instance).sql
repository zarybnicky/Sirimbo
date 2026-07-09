CREATE FUNCTION public.event_instance_registrations(inst public.event_instance) RETURNS SETOF public.event_registration
    LANGUAGE sql STABLE
    AS $$
  select er.*
  from event_registration er
  join event_instance_registration eir
    on eir.legacy_registration_id = er.id
   and eir.instance_id = inst.id
   and eir.parent_registration_id is null
   and eir.status is distinct from 'cancelled';
$$;

GRANT ALL ON FUNCTION public.event_instance_registrations(inst public.event_instance) TO anonymous;
