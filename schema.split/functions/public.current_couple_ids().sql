CREATE FUNCTION public.current_couple_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  select my_couple_ids();
$$;

COMMENT ON FUNCTION public.current_couple_ids() IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.current_couple_ids() TO anonymous;


