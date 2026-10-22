select app_private.drop_policies('public.event_registration');

create policy current_tenant on event_registration as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_registration to administrator
  using (true);
create policy trainer_insert on event_registration for insert to trainer
  with check (app_private.can_trainer_edit_instance(event_id));
create policy trainer_update on event_registration for update to trainer
  using (app_private.can_trainer_edit_instance(event_id));
create policy insert_my on event_registration for insert
  with check (person_id = any((select current_person_ids())::bigint[])
    or couple_id = any((select current_couple_ids())::bigint[]));
create policy delete_my on event_registration for delete
  using (person_id = any((select current_person_ids())::bigint[])
    or couple_id = any((select current_couple_ids())::bigint[]));
create policy view_visible_instance on event_registration for select
  using (exists (select 1 from event e where e.id = event_registration.event_id));

grant all on event_registration to anonymous;
