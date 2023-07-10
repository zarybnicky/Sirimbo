CREATE FUNCTION public.event_free_slots(a public.event) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select a.capacity - (select count(*) from attendee_user where event_id = a.id);
$$;

GRANT ALL ON FUNCTION public.event_free_slots(a public.event) TO anonymous;


