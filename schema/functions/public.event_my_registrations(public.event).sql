CREATE FUNCTION public.event_my_registrations(e public.event) RETURNS SETOF public.event_registration
    LANGUAGE sql STABLE
    AS $$
  select * from event_registration
  where event_id = e.id
  and (person_id = any (current_person_ids())
    or couple_id = any (current_couple_ids()));
$$;

COMMENT ON FUNCTION public.event_my_registrations(e public.event) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_my_registrations(e public.event) TO anonymous;
