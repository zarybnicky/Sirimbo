CREATE FUNCTION public.current_couple_ids() RETURNS bigint[]
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_couple_ids', true), ''), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION public.current_couple_ids() IS '@omit';

GRANT ALL ON FUNCTION public.current_couple_ids() TO anonymous;
