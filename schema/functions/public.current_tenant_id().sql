CREATE FUNCTION public.current_tenant_id() RETURNS bigint
    LANGUAGE sql STABLE
    AS $$
  select COALESCE(nullif(current_setting('jwt.claims.tenant_id', true), '')::bigint, 1);
$$;

COMMENT ON FUNCTION public.current_tenant_id() IS '@omit';

SET default_tablespace = '';

SET default_table_access_method = heap;

GRANT ALL ON FUNCTION public.current_tenant_id() TO anonymous;
