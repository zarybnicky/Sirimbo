CREATE FUNCTION public.event_signed_up(a public.event) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select exists (select id from attendee_user where event_id=a.id and user_id=current_user_id());
$$;

GRANT ALL ON FUNCTION public.event_signed_up(a public.event) TO anonymous;


