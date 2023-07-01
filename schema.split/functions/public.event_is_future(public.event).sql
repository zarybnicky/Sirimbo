CREATE FUNCTION public.event_is_future(event public.event) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  SELECT event.until >= now();
$$;

COMMENT ON FUNCTION public.event_is_future(event public.event) IS '@filterable';

GRANT ALL ON FUNCTION public.event_is_future(event public.event) TO anonymous;


