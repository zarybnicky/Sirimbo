create or replace function app_private.can_trainer_edit_event(eid bigint) returns boolean language sql as $$
  select (
    select count(person_id) > 0 from event_trainer where eid = event_id and person_id = any (my_persons_array())
  ) or not exists (
    select 1 from event_trainer where eid = event_id
  );
$$ security definer;
grant all on function app_private.can_trainer_edit_event to anonymous;


select app_private.drop_policies('public.event');
CREATE POLICY my_tenant ON public.event AS RESTRICTIVE USING (tenant_id = current_tenant_id());
CREATE POLICY admin_same_tenant ON public.event to administrator USING (tenant_id = any (my_tenants_array()));
CREATE POLICY trainer_same_tenant ON public.event to trainer
  USING (app_private.can_trainer_edit_event(id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
CREATE POLICY view_public ON public.event FOR SELECT TO anonymous USING (is_public = true or tenant_id = any (my_tenants_array()));

select app_private.drop_policies('public.event_instance');
CREATE POLICY my_tenant ON public.event_instance AS RESTRICTIVE USING (tenant_id = current_tenant_id());
CREATE POLICY admin_same_tenant ON public.event_instance to administrator USING (tenant_id = any (my_tenants_array()));
CREATE POLICY trainer_same_tenant ON public.event_instance to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
CREATE POLICY view_visible_event ON public.event_instance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_instance.event_id = event.id))));

select app_private.drop_policies('public.event_trainer');
CREATE POLICY my_tenant ON public.event_trainer AS RESTRICTIVE USING (tenant_id = current_tenant_id());
create policy admin_all on event_trainer to administrator using (true);
CREATE POLICY trainer_same_tenant ON public.event_trainer to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
create policy view_all on event_trainer for select to member using (true);

select app_private.drop_policies('public.event_target_cohort');
create policy my_tenant on event_target_cohort as restrictive using (tenant_id = current_tenant_id());
create policy admin_all on event_target_cohort to administrator using (true);
CREATE POLICY trainer_same_tenant ON public.event_target_cohort to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
create policy view_tenant on event_target_cohort for select to member using (true);

select app_private.drop_policies('public.event_instance_trainer');
create policy my_tenant on event_instance_trainer as restrictive using (tenant_id = current_tenant_id());
create policy admin_all on event_instance_trainer to administrator using (true);
CREATE POLICY trainer_same_tenant ON public.event_instance_trainer to trainer
  USING (app_private.can_trainer_edit_event((select event_id from event_instance i where i.id = instance_id)))
  WITH CHECK (tenant_id = any (my_tenants_array()));
create policy view_tenant on event_instance_trainer for select to member using (true);

select app_private.drop_policies('public.event_registration');
create policy admin_all on event_registration to administrator using (true);
CREATE POLICY trainer_same_tenant ON public.event_registration to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
create policy update_my on event_registration for update using (
  (select event_is_registration_open(event) from event where event_id = event.id)
  and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()))
);
create policy delete_my on event_registration for delete using (
  (select event_is_registration_open(event) from event where event_id = event.id)
  and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()))
);
create policy view_visible_event on event_registration for select using (
  exists (select 1 from event where event_id = event.id)
);
