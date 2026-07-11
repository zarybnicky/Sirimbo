select app_private.drop_policies('public.event_instance_target_cohort');

create policy current_tenant on public.event_instance_target_cohort as restrictive
  using (tenant_id = (select public.current_tenant_id()));
create policy admin_all on public.event_instance_target_cohort to administrator using (true);
create policy trainer_select on public.event_instance_target_cohort for select to trainer
  using (app_private.can_trainer_edit_instance(instance_id));
create policy trainer_insert on public.event_instance_target_cohort for insert to trainer
  with check (app_private.can_trainer_edit_instance(instance_id));
create policy trainer_update on public.event_instance_target_cohort for update to trainer
  using (app_private.can_trainer_edit_instance(instance_id))
  with check (app_private.can_trainer_edit_instance(instance_id));
create policy trainer_delete on public.event_instance_target_cohort for delete to trainer
  using (app_private.can_trainer_edit_instance(instance_id));
create policy member_view on public.event_instance_target_cohort for select to member
  using (instance_id in (select id from public.event_instance));
