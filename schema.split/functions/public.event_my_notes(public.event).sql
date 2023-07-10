CREATE FUNCTION public.event_my_notes(a public.event) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select notes from attendee_user where event_id=a.id and user_id=current_user_id();
$$;

GRANT ALL ON FUNCTION public.event_my_notes(a public.event) TO anonymous;


