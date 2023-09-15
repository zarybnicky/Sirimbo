CREATE FUNCTION public.my_cohorts_array() RETURNS bigint[]
    LANGUAGE sql STABLE
    AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_cohort_ids', true), ''), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION public.my_cohorts_array() IS '@omit';

GRANT ALL ON FUNCTION public.my_cohorts_array() TO anonymous;


