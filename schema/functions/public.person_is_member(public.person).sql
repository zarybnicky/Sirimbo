CREATE FUNCTION public.person_is_member(p public.person) RETURNS boolean
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  select exists (select 1 from tenant_membership where person_id = p.id and tenant_id = current_tenant_id() and status = 'active');
$$;

GRANT ALL ON FUNCTION public.person_is_member(p public.person) TO anonymous;
