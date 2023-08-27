CREATE FUNCTION public.cohort_membership_active(c public.cohort_membership) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select now() <@ c.active_range;
$$;

COMMENT ON FUNCTION public.cohort_membership_active(c public.cohort_membership) IS '@filterable';

GRANT ALL ON FUNCTION public.cohort_membership_active(c public.cohort_membership) TO anonymous;


