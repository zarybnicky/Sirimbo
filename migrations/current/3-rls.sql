select app_private.drop_policies('public.address');
create policy admin_all on address to administrator using (true);
create policy admin_personal on address using (id in (select address_id from person_address where person_id in (select my_person_ids())));
create policy view_visible_person on address for select using (exists (select 1 from person_address where address_id = id));

select app_private.drop_policies('public.couple');
create policy admin_all on couple to administrator using (true);
create policy view_visible_person on couple for select using (exists (select 1 from person where man_id = person.id or woman_id = person.id));

select app_private.drop_policies('public.user_proxy');
create policy admin_all on user_proxy to administrator using (true);
create policy view_personal on user_proxy for select using (user_id = current_user_id());

select app_private.drop_policies('public.person');
create policy admin_all on person to administrator using (true);
create policy view_same_tenant on person for select using (exists (select 1 from tenant_membership where active=true and person_id = id and tenant_id in (select my_tenant_ids())));
create policy view_tenant_admin on person for select using (exists (select 1 from tenant_administrator where active=true and person_id = id));
create policy view_tenant_trainer on person for select using (exists (select 1 from tenant_trainer where active=true and person_id = id));

select app_private.drop_policies('public.person_address');
create policy admin_all on person_address to administrator using (true);
create policy admin_personal on person_address using (person_id in (select my_person_ids()));
create policy view_visible_person on person_address for select using (exists (select 1 from person where person_id = person.id));

select app_private.drop_policies('public.person_email');
create policy admin_all on person_email to administrator using (true);
create policy admin_personal on person_email using (person_id in (select my_person_ids()));
create policy view_visible_person on person_email for select using (exists (select 1 from person where person_id = person.id));

select app_private.drop_policies('public.person_phone');
create policy admin_all on person_phone to administrator using (true);
create policy admin_personal on person_phone using (person_id in (select my_person_ids()));
create policy view_visible_person on person_phone for select using (exists (select 1 from person where person_id = person.id));

alter table cohort_membership add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON cohort_membership (tenant_id);
select app_private.drop_policies('public.cohort_membership');
create policy admin_all on cohort_membership to administrator using (true);
create policy view_visible_person on cohort_membership for select using (exists (select 1 from person where person_id = person.id));

select app_private.drop_policies('public.tenant_membership');
create policy admin_all on tenant_membership to administrator using (true);
create policy view_visible_person on tenant_membership for select using (true);

select app_private.drop_policies('public.tenant_administrator');
create policy admin_all on tenant_administrator to administrator using (true);
create policy public_view on tenant_administrator for select using (true);

select app_private.drop_policies('public.tenant_trainer');
create policy admin_all on tenant_trainer to administrator using (true);
create policy public_view on tenant_trainer for select using (true);

select app_private.drop_policies('public.event_attendance');
create policy admin_all on event_attendance to administrator using (true);
create policy view_visible_event on event_attendance for select using (exists (select 1 from event_instance where instance_id = event_instance.id));

select app_private.drop_policies('public.event_instance');
create policy admin_all on event_instance to administrator using (true);
create policy view_visible_event on event_instance for select using (exists (select 1 from event where event_id = event.id));

select app_private.drop_policies('public.event_instance_trainer');
create policy admin_all on event_instance_trainer to administrator using (true);
create policy view_visible_event on event_instance_trainer for select using (exists (select 1 from event_instance where instance_id = event_instance.id));

select app_private.drop_policies('public.event_lesson_demand');
create policy admin_all on event_lesson_demand to administrator using (true);
create policy view_visible_event on event_lesson_demand for select using (exists (select 1 from event_registration where registration_id = event_registration.id));

select app_private.drop_policies('public.event_registration');
create policy admin_all on event_registration to administrator using (true);
create policy view_visible_event on event_registration for select using (exists (select 1 from event where event_id = event.id));

select app_private.drop_policies('public.event_trainer');
create policy admin_all on event_trainer to administrator using (true);
create policy view_visible_event on event_trainer for select using (exists (select 1 from event where event_id = event.id));

select app_private.drop_policies('public.event_target_cohort');
create policy admin_all on event_target_cohort to administrator using (true);
create policy view_visible_event on event_target_cohort for select using (exists (select 1 from event where event_id = event.id));

select app_private.drop_policies('public.event');
CREATE POLICY admin_all ON event TO administrator USING (true);
create policy view_same_tenant on event for select using (tenant_id in (select my_tenant_ids()));
CREATE POLICY view_public ON event FOR SELECT TO anonymous USING ((is_public = true));
