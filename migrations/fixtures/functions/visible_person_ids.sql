create or replace function app_private.visible_person_ids() returns setof bigint as $$
  -- always visible: trainers + admins in current tenant
  select tt.person_id from tenant_trainer tt where tt.status = 'active' and tt.tenant_id = (select current_tenant_id())
  union all
  select ta.person_id from tenant_administrator ta where ta.status = 'active' and ta.tenant_id = (select current_tenant_id())
  union all
  -- members visible only if viewer is in tenant (any role)
  select tm.person_id from tenant_membership tm where tm.status = 'active' and tm.tenant_id = (select current_tenant_id())
    and (select current_tenant_id()) = any (my_tenants_array())
$$ language sql stable leakproof parallel safe;

grant all on function app_private.visible_person_ids() to anonymous;

create or replace function app_private.visible_person_ids_array() returns bigint[] as $$
select coalesce(array_agg(x.person_id), '{}'::bigint[])
from (
  -- always visible: trainers + admins in current tenant
  SELECT tt.person_id
  FROM tenant_trainer tt
  WHERE tt.status = 'active'
    AND tt.tenant_id = (SELECT current_tenant_id())
  UNION
  SELECT ta.person_id
  FROM tenant_administrator ta
  WHERE ta.status = 'active'
    AND ta.tenant_id = (SELECT current_tenant_id())
  UNION
  -- members visible only if viewer is in tenant (any role)
  SELECT tm.person_id
  FROM tenant_membership tm
  WHERE tm.status = 'active'
    AND tm.tenant_id = (SELECT current_tenant_id())
    AND (SELECT current_tenant_id()) = ANY (my_tenants_array())
) x;
$$ language sql stable leakproof parallel safe;

grant all on function app_private.visible_person_ids_array() to anonymous;
