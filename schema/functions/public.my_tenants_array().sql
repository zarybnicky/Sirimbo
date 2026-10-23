CREATE FUNCTION public.my_tenants_array() RETURNS bigint[]
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_tenant_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION public.my_tenants_array() IS '@behavior -*';

GRANT ALL ON FUNCTION public.my_tenants_array() TO anonymous;
