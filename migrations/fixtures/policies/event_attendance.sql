select app_private.drop_policies('public.event_attendance');
create policy current_tenant on event_attendance as restrictive using (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_all ON event_attendance TO administrator USING (true);
CREATE POLICY admin_trainer ON event_attendance for update TO trainer USING (exists (
  select 1
  from event_instance
  left join event_trainer on event_instance.event_id=event_trainer.event_id
  left join event_instance_trainer on event_instance.id=event_instance_trainer.instance_id
  where event_attendance.instance_id=event_instance.id and (
    event_instance_trainer.person_id = any (current_person_ids())
    or event_trainer.person_id = any (current_person_ids())
  )
));
CREATE POLICY view_visible_event ON event_attendance FOR SELECT USING (EXISTS (SELECT 1
   FROM event_instance
  WHERE event_attendance.instance_id = event_instance.id));
