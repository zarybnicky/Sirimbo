CREATE FUNCTION public.event_remaining_spots(a public.event) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select a.capacity - (select count(*) from attendee_user where event_id = a.id) - (select count(*) from attendee_external where event_id = a.id);
$$;

GRANT ALL ON FUNCTION public.event_remaining_spots(a public.event) TO anonymous;


