CREATE FUNCTION public.person_is_trainer(p public.person) RETURNS boolean
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  select current_tenant_id() = any (auth_details.tenant_trainers) from app_private.auth_details where person_id=p.id;
$$;

GRANT ALL ON FUNCTION public.person_is_trainer(p public.person) TO anonymous;


