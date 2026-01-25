select app_private.drop_policies('public.event_attendance');
CREATE POLICY admin_all ON event_attendance TO administrator USING (true);
CREATE POLICY admin_trainer_insert ON event_attendance for insert TO trainer with check (exists (
  select 1
  from event_instance
  left join event_trainer on event_instance.event_id=event_trainer.event_id
  left join event_instance_trainer on event_instance.id=event_instance_trainer.instance_id
  where event_attendance.instance_id=event_instance.id and (
    event_instance_trainer.person_id = any ((select current_person_ids())::bigint[])
    or event_trainer.person_id = any ((select current_person_ids())::bigint[])
  )
));
CREATE POLICY admin_trainer ON event_attendance for update TO trainer USING (exists (
  select 1
  from event_instance
  left join event_trainer on event_instance.event_id=event_trainer.event_id
  left join event_instance_trainer on event_instance.id=event_instance_trainer.instance_id
  where event_attendance.instance_id=event_instance.id and (
    event_instance_trainer.person_id = any ((select current_person_ids())::bigint[])
    or event_trainer.person_id = any ((select current_person_ids())::bigint[])
  )
));
create policy view_visible_event on event_attendance for select using (event_id = any (select id from event));
