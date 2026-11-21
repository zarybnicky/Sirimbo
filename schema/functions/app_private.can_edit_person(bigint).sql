CREATE FUNCTION app_private.can_edit_person(p_person_id bigint) RETURNS boolean
    LANGUAGE sql STABLE SECURITY DEFINER LEAKPROOF PARALLEL SAFE
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select exists (
    select 1 from tenant_membership
    where person_id = p_person_id
      and tenant_id = any ((select current_admin_tenant_ids())::bigint[])
    union all
    select 1 from tenant_trainer
    where person_id = p_person_id
      and tenant_id = any ((select current_admin_tenant_ids())::bigint[])
    union all
    select 1 from tenant_administrator
    where person_id = p_person_id
      and tenant_id = any ((select current_admin_tenant_ids())::bigint[])
  );
$$;

GRANT ALL ON FUNCTION app_private.can_edit_person(p_person_id bigint) TO anonymous;
