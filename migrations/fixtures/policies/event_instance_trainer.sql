select app_private.drop_policies('public.event_instance_trainer');

create policy current_tenant on event_instance_trainer as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_instance_trainer to administrator using (true);
create policy trainer_same_tenant on event_instance_trainer to trainer
  using (app_private.can_trainer_edit_instance(instance_id))
  with check (true);
create policy member_view on event_instance_trainer for select to member using (true);
create policy event_share_view on event_instance_trainer for select to anonymous
  using (instance_id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[]));
