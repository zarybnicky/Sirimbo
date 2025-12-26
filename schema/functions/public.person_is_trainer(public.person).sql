CREATE FUNCTION public.person_is_trainer(p public.person) RETURNS boolean
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  select exists (select 1 from current_tenant_trainer where person_id = p.id);
$$;

GRANT ALL ON FUNCTION public.person_is_trainer(p public.person) TO anonymous;
