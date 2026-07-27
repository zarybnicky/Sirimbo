select app_private.drop_policies('public.event_instance');

create policy current_tenant on event_instance as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_same_tenant on event_instance to administrator using (true);
create policy trainer_select on event_instance for select to trainer
  using (app_private.can_trainer_edit_instance(id));
create policy trainer_insert on event_instance for insert to trainer
  with check (parent_id is null or app_private.can_trainer_edit_instance(parent_id));
create policy trainer_update on event_instance for update to trainer
  using (app_private.can_trainer_edit_instance(id));
create policy trainer_delete on event_instance for delete to trainer
  using (app_private.can_trainer_edit_instance(id));
create policy member_view on event_instance for select to member
  using (is_visible);
create policy public_view on event_instance for select to anonymous
  using (is_public);
create policy event_share_view on event_instance for select to anonymous
  using (id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[]));
