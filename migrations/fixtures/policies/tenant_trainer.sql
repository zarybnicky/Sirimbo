select app_private.drop_policies('public.tenant_trainer');

create policy system_admin_all on tenant_trainer to system_admin
  using (true);
create policy admin_all on tenant_trainer to administrator
  using (tenant_id = (select current_tenant_id()));
create policy public_view on tenant_trainer for select
  using (true);
