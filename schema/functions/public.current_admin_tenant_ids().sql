CREATE FUNCTION public.current_admin_tenant_ids() RETURNS bigint[]
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.admin_tenant_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION public.current_admin_tenant_ids() IS '@behavior -*';

GRANT ALL ON FUNCTION public.current_admin_tenant_ids() TO anonymous;
