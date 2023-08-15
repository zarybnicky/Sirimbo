CREATE FUNCTION public.person_cohort_ids(p public.person) RETURNS bigint[]
    LANGUAGE sql STABLE
    AS $$
  select array_agg(cohort_id) from cohort_membership where active = true and person_id = p.id;
$$;

GRANT ALL ON FUNCTION public.person_cohort_ids(p public.person) TO anonymous;


