CREATE FUNCTION public.person_is_admin(p public.person) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select exists (select 1 from tenant_administrator where now() <@ active_range and person_id = p.id);
$$;

GRANT ALL ON FUNCTION public.person_is_admin(p public.person) TO anonymous;


