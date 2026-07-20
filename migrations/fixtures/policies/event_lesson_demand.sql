select app_private.drop_policies('public.event_lesson_demand');

CREATE POLICY admin_all ON event_lesson_demand TO administrator USING (true);
CREATE POLICY view_visible_instance ON event_lesson_demand FOR SELECT
  USING (registration_id IN (SELECT id FROM event_instance_registration));

GRANT ALL ON event_lesson_demand TO anonymous;
