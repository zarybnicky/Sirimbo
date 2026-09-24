CREATE FUNCTION public.access_credential_is_allowed(c public.access_credential) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select c.valid_range @> now() and (
    exists (select 1 from current_tenant_membership r where r.person_id = c.person_id)
    or exists (select 1 from current_tenant_trainer r where r.person_id = c.person_id)
    or exists (select 1 from current_tenant_administrator r where r.person_id = c.person_id)
  );
$$;

GRANT ALL ON FUNCTION public.access_credential_is_allowed(c public.access_credential) TO anonymous;
