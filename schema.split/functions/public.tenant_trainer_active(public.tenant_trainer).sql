CREATE FUNCTION public.tenant_trainer_active(c public.tenant_trainer) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select now() <@ c.active_range;
$$;

COMMENT ON FUNCTION public.tenant_trainer_active(c public.tenant_trainer) IS '@filterable';

GRANT ALL ON FUNCTION public.tenant_trainer_active(c public.tenant_trainer) TO anonymous;


