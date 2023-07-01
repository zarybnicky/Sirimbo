CREATE FUNCTION public.users_in_public_cohort(a public.users) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  SELECT s_visible
  FROM skupiny
  inner join users on s_id = u_skupina
  where u_id = a.u_id
$$;

COMMENT ON FUNCTION public.users_in_public_cohort(a public.users) IS '@filterable';

GRANT ALL ON FUNCTION public.users_in_public_cohort(a public.users) TO anonymous;


