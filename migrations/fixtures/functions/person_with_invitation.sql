
create or replace function people_without_access_with_existing_account() returns setof person stable language sql as $$
select * from person
where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and exists (select 1 from users where users.u_email = person.email)
  and (exists (select 1 from current_tenant_membership where person_id = person.id)
  or exists (select 1 from current_tenant_trainer where person_id = person.id)
  or exists (select 1 from current_tenant_administrator where person_id = person.id));
$$;
grant all on function people_without_access_with_existing_account to anonymous;

COMMENT ON function people_without_access_with_existing_account IS '@simpleCollections only';

create or replace function people_without_access_or_invitation() returns setof person stable language sql as $$
select * from person
where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and not exists (select 1 from person_invitation where email = person.email)
  and not exists (select 1 from users where users.u_email = person.email)
  and (exists (select 1 from current_tenant_membership where person_id = person.id)
    or exists (select 1 from current_tenant_trainer where person_id = person.id)
    or exists (select 1 from current_tenant_administrator where person_id = person.id));
$$;
grant all on function people_without_access_or_invitation to anonymous;

COMMENT ON function people_without_access_or_invitation IS '@simpleCollections only';

create or replace function people_without_access_with_invitation() returns setof person stable language sql as $$
select * from person
where not exists (select 1 from user_proxy where person_id = person.id)
  and exists (select 1 from person_invitation where person_id = person.id)
  and (exists (select 1 from current_tenant_membership where person_id = person.id)
  or exists (select 1 from current_tenant_trainer where person_id = person.id)
  or exists (select 1 from current_tenant_administrator where person_id = person.id));
$$;
grant all on function people_without_access_with_invitation to anonymous;

COMMENT ON function people_without_access_with_invitation IS '@simpleCollections only';
