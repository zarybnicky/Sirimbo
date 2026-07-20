create or replace function system_admin_update_tenant(
  tenant_id bigint,
  name text default null,
  description text default null,
  bank_account text default null,
  origins text[] default null,
  address address_domain default null,
  cz_ico text default null,
  cz_dic text default null
)
returns tenant
language plpgsql
volatile
security definer
set search_path = public, pg_temp
as $$
declare
  v_tenant tenant;
begin
  if not app_private.is_system_admin(current_user_id()) then
    raise exception 'permission denied for system admin tenant update'
      using errcode = '42501';
  end if;

  update tenant t
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

grant execute on function system_admin_update_tenant to anonymous;
