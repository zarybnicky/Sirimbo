CREATE or replace FUNCTION person_is_admin(p person) RETURNS boolean AS $$
  select exists (select 1 from current_tenant_administrator where person_id = p.id);
$$ language sql stable PARALLEL SAFE LEAKPROOF;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;

CREATE or replace FUNCTION person_is_trainer(p person) RETURNS boolean AS $$
  select exists (select 1 from current_tenant_trainer where person_id = p.id);
$$ language sql stable PARALLEL SAFE LEAKPROOF;
GRANT ALL ON FUNCTION person_is_trainer TO anonymous;

CREATE or replace FUNCTION person_is_member(p person) RETURNS boolean AS $$
  select exists (select 1 from current_tenant_membership where person_id = p.id);
$$ language sql stable PARALLEL SAFE LEAKPROOF;
GRANT ALL ON FUNCTION person_is_member TO anonymous;
