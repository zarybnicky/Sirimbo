CREATE FUNCTION public.person_is_admin(p public.person) RETURNS boolean
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  select exists (select 1 from current_tenant_administrator where person_id = p.id);
$$;

GRANT ALL ON FUNCTION public.person_is_admin(p public.person) TO anonymous;
