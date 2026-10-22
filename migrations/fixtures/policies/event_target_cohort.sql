select app_private.drop_policies('public.event_target_cohort');

create policy current_tenant on event_target_cohort as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_target_cohort to administrator
  using (true);
create policy trainer_select on event_target_cohort to trainer
  using (app_private.can_trainer_edit_instance(event_id));
create policy member_view on event_target_cohort for select to member
  using (exists (select 1 from event e where e.id = event_target_cohort.event_id));
create policy event_access_view on event_target_cohort for select to anonymous
  using (event_id in (select app_private.accessible_event_ids()));
