CREATE FUNCTION public.current_couple_ids() RETURNS bigint[]
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_couple_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION public.current_couple_ids() IS '@behavior -*';

GRANT ALL ON FUNCTION public.current_couple_ids() TO anonymous;
