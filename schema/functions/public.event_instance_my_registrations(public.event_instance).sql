CREATE FUNCTION public.event_instance_my_registrations(inst public.event_instance) RETURNS SETOF public.event_registration
    LANGUAGE sql STABLE
    AS $$
  select er.*
  from event_registration er
  join event_instance_registration eir
    on eir.legacy_registration_id = er.id
   and eir.instance_id = inst.id
   and eir.parent_registration_id is null
   and eir.status is distinct from 'cancelled'
  where er.person_id = any (current_person_ids())
     or er.couple_id = any (current_couple_ids());
$$;

COMMENT ON FUNCTION public.event_instance_my_registrations(inst public.event_instance) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_instance_my_registrations(inst public.event_instance) TO anonymous;
