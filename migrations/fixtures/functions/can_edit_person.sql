create or replace function app_private.can_edit_person(p_person_id bigint)
  returns boolean
  language sql stable security definer leakproof parallel safe
  set search_path = pg_catalog, public, pg_temp
as $$
  select exists (
    select 1 from app_private.tenant_relation
    where person_id = p_person_id and has_roles
      and tenant_id = any ((select current_admin_tenant_ids())::bigint[])
  );
$$;

grant all on function app_private.can_edit_person to anonymous;
