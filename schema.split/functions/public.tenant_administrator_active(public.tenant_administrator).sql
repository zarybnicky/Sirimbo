CREATE FUNCTION public.tenant_administrator_active(c public.tenant_administrator) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select now() <@ c.active_range;
$$;

COMMENT ON FUNCTION public.tenant_administrator_active(c public.tenant_administrator) IS '@filterable';

GRANT ALL ON FUNCTION public.tenant_administrator_active(c public.tenant_administrator) TO anonymous;


