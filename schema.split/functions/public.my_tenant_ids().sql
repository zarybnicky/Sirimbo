CREATE FUNCTION public.my_tenant_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_tenant_ids', true), '')::json)::bigint;
$$;

COMMENT ON FUNCTION public.my_tenant_ids() IS '@omit';

GRANT ALL ON FUNCTION public.my_tenant_ids() TO anonymous;


