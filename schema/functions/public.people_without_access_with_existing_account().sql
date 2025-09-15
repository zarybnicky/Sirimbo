CREATE FUNCTION public.people_without_access_with_existing_account() RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
  select * from person
  where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and exists (select 1 from users where users.u_email = person.email)
  and (exists (select 1 from tenant_membership where active and person_id = person.id and tenant_id = (select current_tenant_id()))
  or exists (select 1 from tenant_trainer where active and person_id = person.id and tenant_id = (select current_tenant_id()))
  or exists (select 1 from tenant_administrator where active and person_id = person.id and tenant_id = (select current_tenant_id())));
$$;

COMMENT ON FUNCTION public.people_without_access_with_existing_account() IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.people_without_access_with_existing_account() TO anonymous;
