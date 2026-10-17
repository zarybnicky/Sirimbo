select app_private.drop_policies('public.event_external_registration');

create policy current_tenant on event_external_registration as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_external_registration to administrator
  using (true);
create policy register_public on event_external_registration for insert to anonymous
  with check ((select i.is_public from event_instance as i where event_external_registration.instance_id = i.id));
create policy trainer_same_tenant on event_external_registration to trainer
  using (app_private.can_trainer_edit_instance(instance_id));
create policy view_visible_instance on event_external_registration for select to member
  using (instance_id = any (select id from event_instance));
create policy admin_my on event_external_registration to member
  using ((select not i.is_locked from event_instance as i where event_external_registration.instance_id = i.id)
     and (created_by = current_user_id()));
