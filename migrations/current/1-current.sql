create or replace function person_has_access(p person) returns boolean stable language sql as $$
  select exists (select 1 from user_proxy where person_id = p.id);
$$;
grant all on function person_has_access to anonymous;

create or replace function people_without_access_with_existing_account() returns setof person stable language sql as $$
  select * from person
  where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and exists (select 1 from users where users.u_email = person.email)
  and exists (select 1 from tenant_membership where person_id = person.id and tenant_id = (select current_tenant_id()));
$$;
grant all on function people_without_access_with_existing_account to anonymous;

COMMENT ON function people_without_access_with_existing_account IS '@simpleCollections only';

create or replace function people_without_access_or_invitation() returns setof person stable language sql as $$
  select * from person
  where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and not exists (select 1 from users where users.u_email = person.email)
  and exists (select 1 from tenant_membership where person_id = person.id and tenant_id = (select current_tenant_id()));
$$;
grant all on function people_without_access_or_invitation to anonymous;

COMMENT ON function people_without_access_or_invitation IS '@simpleCollections only';

create or replace function people_without_access_with_invitation() returns setof person stable language sql as $$
  select * from person
  where not exists (select 1 from user_proxy where person_id = person.id)
  and exists (select 1 from person_invitation where person_id = person.id)
  and exists (select 1 from tenant_membership where person_id = person.id and tenant_id = (select current_tenant_id()));
$$;
grant all on function people_without_access_with_invitation to anonymous;

COMMENT ON function people_without_access_with_invitation IS '@simpleCollections only';
