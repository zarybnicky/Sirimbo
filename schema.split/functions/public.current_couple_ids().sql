CREATE FUNCTION public.current_couple_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  select my_couple_ids();
$$;

GRANT ALL ON FUNCTION public.current_couple_ids() TO anonymous;


