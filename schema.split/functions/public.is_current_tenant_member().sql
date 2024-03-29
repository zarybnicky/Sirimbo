CREATE FUNCTION public.is_current_tenant_member() RETURNS boolean
    LANGUAGE sql
    AS $$
  select exists (select * from tenant_membership where tenant_id = current_tenant_id() and person_id = any (current_person_ids()));
$$;

GRANT ALL ON FUNCTION public.is_current_tenant_member() TO anonymous;


