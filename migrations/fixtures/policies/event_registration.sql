select app_private.drop_policies('public.event_registration');
create policy admin_all on event_registration to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_registration to trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
create policy update_my on event_registration for update using (
  (select event_is_registration_open(event) from event where event_id = event.id)
  and (person_id = any ((select current_person_ids())::bigint[]) or couple_id = any ((select current_couple_ids())::bigint[]))
);
create policy delete_my on event_registration for delete using (
  (select event_is_registration_open(event) from event where event_id = event.id)
  and (person_id = any ((select current_person_ids())::bigint[]) or couple_id = any ((select current_couple_ids())::bigint[]))
);
create policy view_visible_event on event_registration for select using (event_id = any (select id from event));
