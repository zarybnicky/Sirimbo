select app_private.drop_policies('public.membership_application');
create policy current_tenant on membership_application as restrictive using (tenant_id = (select current_tenant_id()));
create policy manage_admin on membership_application to administrator using (true);
create policy view_my on membership_application for select using (created_by = (select current_user_id()));
create policy insert_my on membership_application for insert
  with check (created_by = (select current_user_id()) and status in ('new', 'sent'));
create policy update_my on membership_application for update
  using (created_by = (select current_user_id()) and status in ('new', 'sent'))
  with check (created_by = (select current_user_id()) and status in ('new', 'sent'));
create policy delete_my on membership_application for delete
  using (created_by = (select current_user_id()) and status in ('new', 'sent'));

