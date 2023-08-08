CREATE FUNCTION public.current_tenant_id() RETURNS bigint
    LANGUAGE sql STABLE
    AS $$
  select COALESCE(current_setting('jwt.claims.tenant_id', '1')::bigint, 1);
$$;

SET default_tablespace = '';

SET default_table_access_method = heap;

GRANT ALL ON FUNCTION public.current_tenant_id() TO anonymous;


