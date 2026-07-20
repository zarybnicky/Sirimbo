select app_private.drop_policies('public.event_instance_target_cohort');

create policy current_tenant on event_instance_target_cohort as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_instance_target_cohort to administrator using (true);
create policy trainer_select on event_instance_target_cohort to trainer
  using (app_private.can_trainer_edit_instance(instance_id));
create policy member_view on event_instance_target_cohort for select to member
  using (instance_id in (select id from event_instance));
