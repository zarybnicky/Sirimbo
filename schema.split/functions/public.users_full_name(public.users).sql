CREATE FUNCTION public.users_full_name(u public.users) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select trim(both from COALESCE(u.u_jmeno, '') || ' ' || COALESCE(u.u_prijmeni, ''));
$$;

GRANT ALL ON FUNCTION public.users_full_name(u public.users) TO anonymous;


