CREATE FUNCTION public.current_tenant_id() RETURNS bigint
    LANGUAGE sql STABLE
    AS $$
  select COALESCE(current_setting('jwt.claims.tenant_id', '1')::bigint, 1);
$$;

GRANT ALL ON FUNCTION public.current_tenant_id() TO anonymous;


