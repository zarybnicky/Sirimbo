select app_private.drop_policies('public.tenant_membership');

create policy system_admin_all on tenant_membership to system_admin
  using (true);
create policy admin_all on tenant_membership to administrator
  using (tenant_id = (select current_tenant_id()));
create policy view_visible_person on tenant_membership for select
  using (true);

