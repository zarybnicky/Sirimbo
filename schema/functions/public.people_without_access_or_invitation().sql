CREATE FUNCTION public.people_without_access_or_invitation() RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
select * from person
where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and not exists (select 1 from person_invitation where email = person.email)
  and not exists (select 1 from users where users.u_email = person.email)
  and (exists (select 1 from current_tenant_membership where person_id = person.id)
    or exists (select 1 from current_tenant_trainer where person_id = person.id)
    or exists (select 1 from current_tenant_administrator where person_id = person.id));
$$;

COMMENT ON FUNCTION public.people_without_access_or_invitation() IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.people_without_access_or_invitation() TO anonymous;
