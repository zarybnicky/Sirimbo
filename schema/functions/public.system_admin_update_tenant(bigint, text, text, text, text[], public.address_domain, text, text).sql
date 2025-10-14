CREATE FUNCTION public.system_admin_update_tenant(tenant_id bigint, name text DEFAULT NULL::text, description text DEFAULT NULL::text, bank_account text DEFAULT NULL::text, origins text[] DEFAULT NULL::text[], address public.address_domain DEFAULT NULL::public.address_type, cz_ico text DEFAULT NULL::text, cz_dic text DEFAULT NULL::text) RETURNS public.tenant
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
declare
  v_tenant public.tenant;
begin
  if not app_private.is_system_admin(current_user_id()) then
    raise exception 'permission denied for system admin tenant update'
      using errcode = '42501';
  end if;

  update public.tenant t
  set
    name = coalesce(system_admin_update_tenant.name, t.name),
    description = coalesce(system_admin_update_tenant.description, t.description),
    bank_account = coalesce(system_admin_update_tenant.bank_account, t.bank_account),
    origins = coalesce(system_admin_update_tenant.origins, t.origins),
    address = coalesce(system_admin_update_tenant.address, t.address),
    cz_ico = coalesce(system_admin_update_tenant.cz_ico, t.cz_ico),
    cz_dic = coalesce(system_admin_update_tenant.cz_dic, t.cz_dic)
  where t.id = tenant_id
  returning t.* into v_tenant;

  if not found then
    raise exception 'tenant % not found', tenant_id using errcode = 'P0002';
  end if;

  return v_tenant;
end;
$$;

COMMENT ON FUNCTION public.system_admin_update_tenant(tenant_id bigint, name text, description text, bank_account text, origins text[], address public.address_domain, cz_ico text, cz_dic text) IS 'Allows system administrators to update tenant metadata without switching tenant context.';

GRANT ALL ON FUNCTION public.system_admin_update_tenant(tenant_id bigint, name text, description text, bank_account text, origins text[], address public.address_domain, cz_ico text, cz_dic text) TO anonymous;
