select app_private.drop_policies('public.tenant_settings');

create policy system_admin_all on tenant_settings to system_admin
  using (true);
create policy admin_own on tenant_settings to administrator
  using (tenant_id = (select current_tenant_id()));
