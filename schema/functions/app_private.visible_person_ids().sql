CREATE FUNCTION app_private.visible_person_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE SECURITY DEFINER LEAKPROOF PARALLEL SAFE
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select unnest(public.current_person_ids()) union all
  -- always visible: trainers + admins in current tenant
  select person_id from public.current_tenant_trainer
  union all
  select person_id from public.current_tenant_administrator
  union all
  -- members visible only if viewer is in tenant (any role)
  select person_id from public.current_tenant_membership
  where (select public.current_tenant_id() = any(public.my_tenants_array()))
  union all
  select visible_person_id from app_private.event_people
  where tenant_id = (select public.current_tenant_id())
    and viewer_person_id = any((select public.current_person_ids())::bigint[]);
$$;

GRANT ALL ON FUNCTION app_private.visible_person_ids() TO anonymous;
