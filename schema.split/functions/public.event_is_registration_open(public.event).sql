CREATE FUNCTION public.event_is_registration_open(e public.event) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select not e.is_locked;
$$;

GRANT ALL ON FUNCTION public.event_is_registration_open(e public.event) TO anonymous;
