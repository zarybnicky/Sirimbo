select app_private.drop_policies('public.event_trainer');

create policy current_tenant on event_trainer as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_trainer to administrator
  using (true);
create policy trainer_same_tenant on event_trainer to trainer
  using (app_private.can_trainer_edit_instance(event_id));
create policy member_view on event_trainer for select to member
  using (true);
create policy public_view on event_trainer for select to anonymous
  using (event_id in (select id from event where is_public));
create policy event_access_view on event_trainer for select to anonymous
  using (event_id in (select app_private.accessible_event_ids()));
