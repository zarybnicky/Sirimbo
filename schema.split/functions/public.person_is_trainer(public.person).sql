CREATE FUNCTION public.person_is_trainer(p public.person) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select exists (select 1 from tenant_trainer where active = true and person_id = p.id);
$$;

GRANT ALL ON FUNCTION public.person_is_trainer(p public.person) TO anonymous;


