CREATE FUNCTION app_private.visible_person_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  -- always visible: trainers + admins in current tenant
  select tt.person_id from tenant_trainer tt where tt.status = 'active' and tt.tenant_id = (select current_tenant_id())
  union all
  select ta.person_id from tenant_administrator ta where ta.status = 'active' and ta.tenant_id = (select current_tenant_id())
  union all
  -- members visible only if viewer is in tenant (any role)
  select tm.person_id from tenant_membership tm where tm.status = 'active' and tm.tenant_id = (select current_tenant_id())
    and (select current_tenant_id()) = any (my_tenants_array())
$$;

GRANT ALL ON FUNCTION app_private.visible_person_ids() TO anonymous;
