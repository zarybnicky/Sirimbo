CREATE FUNCTION app_private.visible_person_ids_array() RETURNS bigint[]
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
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
$$;

GRANT ALL ON FUNCTION app_private.visible_person_ids_array() TO anonymous;
