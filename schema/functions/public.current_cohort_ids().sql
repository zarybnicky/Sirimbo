CREATE FUNCTION public.current_cohort_ids() RETURNS bigint[]
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_cohort_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION public.current_cohort_ids() IS '@behavior -*';

GRANT ALL ON FUNCTION public.current_cohort_ids() TO anonymous;
