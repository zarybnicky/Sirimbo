CREATE FUNCTION public.my_tenants_array() RETURNS bigint[]
    LANGUAGE sql STABLE
    AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_tenant_ids', true), ''), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION public.my_tenants_array() IS '@omit';

GRANT ALL ON FUNCTION public.my_tenants_array() TO anonymous;


