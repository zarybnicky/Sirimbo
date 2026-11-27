select app_private.drop_policies('public.tenant');

create policy system_admin_all on tenant to system_admin
  using (true);
create policy admin_current on tenant to administrator
  using (id = (select current_tenant_id()));
create policy public_view on tenant for select to anonymous
  using (id = any ((select array[current_tenant_id()] || my_tenants_array())::bigint[]));
