CREATE FUNCTION public.my_tenant_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  select tenant.id
  from tenant join tenant_membership on tenant.id = tenant_id
  where tenant_membership.active = true and person_id in (select my_person_ids());
$$;

COMMENT ON FUNCTION public.my_tenant_ids() IS '@omit';

GRANT ALL ON FUNCTION public.my_tenant_ids() TO anonymous;


