drop policy if exists my_tenant on skupiny;

comment on table person is '@omit create';

create or replace function skupiny_in_current_tenant(s skupiny) returns boolean language sql stable as $$
  select s.tenant_id = current_tenant_id();
$$;
grant all on function skupiny_in_current_tenant to anonymous;
comment on function skupiny_in_current_tenant is '@filterable';

CREATE or replace FUNCTION person_is_admin(p person) RETURNS boolean LANGUAGE sql STABLE security definer AS $$
  select current_tenant_id() = any (auth_details.tenant_administrators) from app_private.auth_details where person_id=p.id;
$$;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;

CREATE or replace FUNCTION person_is_trainer(p person) RETURNS boolean LANGUAGE sql STABLE security definer AS $$
  select current_tenant_id() = any (auth_details.tenant_trainers) from app_private.auth_details where person_id=p.id;
$$;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;

CREATE or replace FUNCTION person_is_member(p person) RETURNS boolean LANGUAGE sql STABLE security definer AS $$
  select current_tenant_id() = any (auth_details.tenant_memberships) from app_private.auth_details where person_id=p.id;
$$;
GRANT ALL ON FUNCTION person_is_member TO anonymous;

comment on table user_proxy is E'@simpleCollections only';
comment on table couple is E'@simpleCollections only';
comment on table cohort_membership is E'@simpleCollections only';
comment on table tenant_membership is E'@simpleCollections only';
comment on table tenant_administrator is E'@simpleCollections only';
comment on table tenant_trainer is E'@simpleCollections only';

drop function if exists person_tenant_ids;
