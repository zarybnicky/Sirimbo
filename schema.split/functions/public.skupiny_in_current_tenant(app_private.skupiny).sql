CREATE FUNCTION public.skupiny_in_current_tenant(s app_private.skupiny) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select s.tenant_id = current_tenant_id();
$$;

COMMENT ON FUNCTION public.skupiny_in_current_tenant(s app_private.skupiny) IS '@filterable
@deprecated';

GRANT ALL ON FUNCTION public.skupiny_in_current_tenant(s app_private.skupiny) TO anonymous;


