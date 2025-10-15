CREATE FUNCTION public.get_current_tenant() RETURNS public.tenant
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  SELECT * FROM tenant WHERE id = current_tenant_id();
$$;

GRANT ALL ON FUNCTION public.get_current_tenant() TO anonymous;
