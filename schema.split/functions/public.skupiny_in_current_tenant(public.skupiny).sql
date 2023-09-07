CREATE FUNCTION public.skupiny_in_current_tenant(s public.skupiny) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select s.tenant_id = current_tenant_id();
$$;

COMMENT ON FUNCTION public.skupiny_in_current_tenant(s public.skupiny) IS '@filterable';

GRANT ALL ON FUNCTION public.skupiny_in_current_tenant(s public.skupiny) TO anonymous;


