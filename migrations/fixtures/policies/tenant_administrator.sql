select app_private.drop_policies('public.tenant_administrator');

create policy system_admin_all on tenant_administrator to system_admin
  using (true);
create policy admin_all on tenant_administrator to administrator
  using (tenant_id = (select current_tenant_id()));
create policy public_view on tenant_administrator for select
  using (true);
