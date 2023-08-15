CREATE FUNCTION public.person_tenant_ids(p public.person) RETURNS bigint[]
    LANGUAGE sql STABLE
    AS $$
  select array_agg(tenant_id) from tenant_membership where active = true and person_id = p.id;
$$;

GRANT ALL ON FUNCTION public.person_tenant_ids(p public.person) TO anonymous;


