CREATE FUNCTION public.person_is_admin(p public.person) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select current_tenant_id() = any (tenant_administrators) from auth_details where person_id=p.id;
$$;

GRANT ALL ON FUNCTION public.person_is_admin(p public.person) TO anonymous;


