CREATE FUNCTION public.couple_active(c public.couple) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select now() <@ c.active_range;
$$;

COMMENT ON FUNCTION public.couple_active(c public.couple) IS '@filterable';

GRANT ALL ON FUNCTION public.couple_active(c public.couple) TO anonymous;


