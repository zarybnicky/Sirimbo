create or replace function event_my_registrations(e event) returns setof event_registration language sql stable as $$
  select * from event_registration
  where event_id = e.id
  and (person_id = any (current_person_ids())
    or couple_id = any (current_couple_ids()));
$$;

COMMENT ON FUNCTION public.event_my_registrations IS '@simpleCollections only';

grant all on function event_my_registrations to anonymous;
