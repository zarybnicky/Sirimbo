CREATE FUNCTION public.tenant_membership_active(c public.tenant_membership) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select now() <@ c.active_range;
$$;

COMMENT ON FUNCTION public.tenant_membership_active(c public.tenant_membership) IS '@filterable';

GRANT ALL ON FUNCTION public.tenant_membership_active(c public.tenant_membership) TO anonymous;


