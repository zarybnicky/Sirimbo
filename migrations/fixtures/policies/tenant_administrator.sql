select app_private.drop_policies('public.tenant_administrator');

create policy system_admin_all on tenant_administrator to system_admin
  using (true);
create policy admin_all on tenant_administrator to anonymous
  using (tenant_id = any ((select current_admin_tenant_ids())::bigint[]))
  with check (tenant_id = any ((select current_admin_tenant_ids())::bigint[]) and app_private.can_edit_person(person_id));
create policy public_view on tenant_administrator for select
  using (true);
