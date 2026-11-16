select app_private.drop_policies('public.tenant_administrator');

create policy system_admin_all on tenant_administrator to system_admin
  using (true);
create policy admin_insert on tenant_administrator for insert to administrator
  with check (tenant_id = any ((select current_admin_tenant_ids())::bigint[])
    and app_private.can_edit_person(person_id));
create policy admin_update on tenant_administrator for update to administrator
  using (tenant_id = (select current_tenant_id()))
  with check (tenant_id = (select current_tenant_id()) and app_private.can_edit_person(person_id));
create policy admin_delete on tenant_administrator for delete to administrator
  using (tenant_id = (select current_tenant_id()));
create policy public_view on tenant_administrator for select
  using (true);
