drop index if exists event_lesson_demand_registration_id_idx;
CREATE INDEX event_lesson_demand_registration_id_idx ON public.event_lesson_demand USING btree (registration_id);

drop index if exists event_trainer_event_id_idx;
CREATE INDEX event_trainer_event_id_idx ON public.event_trainer USING btree (event_id);

drop index if exists event_trainer_tenant_event_idx;
CREATE INDEX event_trainer_tenant_event_idx ON public.event_trainer USING btree (tenant_id, event_id);

drop index if exists idx_event_tenant_visible;
CREATE INDEX idx_event_tenant_visible ON public.event USING btree (is_visible, tenant_id);


select app_private.drop_policies('public.event');
CREATE POLICY current_tenant ON event AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_same_tenant ON event to administrator USING (true);
CREATE POLICY trainer_same_tenant ON event to trainer USING (app_private.can_trainer_edit_event(id)) WITH CHECK (true);
CREATE POLICY member_view ON event FOR SELECT to member USING (is_visible);
CREATE POLICY public_view ON event FOR SELECT to anonymous USING (is_public);

select app_private.drop_policies('public.event_instance');
CREATE POLICY current_tenant ON event_instance AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_same_tenant ON event_instance to administrator USING (true);
CREATE POLICY trainer_same_tenant ON event_instance to trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
CREATE POLICY view_visible_event ON event_instance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM event
  WHERE (event_instance.event_id = event.id))));

select app_private.drop_policies('public.event_trainer');
CREATE POLICY current_tenant ON event_trainer AS RESTRICTIVE USING (tenant_id = (select current_tenant_id()));
create policy admin_all on event_trainer to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_trainer to trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
create policy member_view on event_trainer for select to member using (true);

select app_private.drop_policies('public.event_target_cohort');
create POLICY current_tenant on event_target_cohort as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_target_cohort to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_target_cohort to trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
create policy member_view on event_target_cohort for select to member using (true);

select app_private.drop_policies('public.event_instance_trainer');
create POLICY current_tenant on event_instance_trainer as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_instance_trainer to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_instance_trainer to trainer USING (app_private.can_trainer_edit_event((select event_id from event_instance i where i.id = instance_id))) WITH CHECK (true);
create policy member_view on event_instance_trainer for select to member using (true);

select app_private.drop_policies('public.event_registration');
create policy admin_all on event_registration to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_registration to trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
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
