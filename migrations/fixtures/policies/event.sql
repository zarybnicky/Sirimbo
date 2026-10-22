select app_private.drop_policies('public.event');

create policy current_tenant on event as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_same_tenant on event to administrator
  using (true);
create policy trainer_select on event for select to trainer
  using (cardinality(manager_person_ids) = 0 or manager_person_ids && (select current_person_ids()));
create policy trainer_insert on event for insert to trainer
  with check (parent_id is null or app_private.can_trainer_edit_event(parent_id));
create policy trainer_update on event for update to trainer
  using (app_private.can_trainer_edit_event(id));
create policy trainer_delete on event for delete to trainer
  using (app_private.can_trainer_edit_event(id));
create policy member_view on event for select to member
  using (is_visible);
create policy public_view on event for select to anonymous
  using (is_public);
create policy event_access_view on event for select to anonymous
  using (id in (select app_private.accessible_event_ids()));
