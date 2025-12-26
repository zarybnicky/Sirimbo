create or replace function app_private.visible_person_ids() returns setof bigint as $$
  -- always visible: trainers + admins in current tenant
  select person_id from current_tenant_trainer
  union all
  select person_id from current_tenant_administrator
  union all
  -- members visible only if viewer is in tenant (any role)
  select person_id from current_tenant_membership WHERE (SELECT current_tenant_id() = ANY (my_tenants_array()))
$$ language sql stable leakproof parallel safe;

grant all on function app_private.visible_person_ids() to anonymous;
