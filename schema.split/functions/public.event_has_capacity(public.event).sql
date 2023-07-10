CREATE FUNCTION public.event_has_capacity(a public.event) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select count(*) < a.capacity from attendee_user where event_id = a.id;
$$;

GRANT ALL ON FUNCTION public.event_has_capacity(a public.event) TO anonymous;


