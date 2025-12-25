CREATE FUNCTION public.person_is_admin(p public.person) RETURNS boolean
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  select exists (select 1 from tenant_administrator where person_id = p.id and tenant_id = current_tenant_id() and status = 'active');
$$;

GRANT ALL ON FUNCTION public.person_is_admin(p public.person) TO anonymous;
