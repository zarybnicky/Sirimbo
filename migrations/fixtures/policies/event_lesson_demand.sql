select app_private.drop_policies('public.event_lesson_demand');

CREATE POLICY current_tenant ON event_lesson_demand AS RESTRICTIVE
  USING (tenant_id = (SELECT current_tenant_id()));
CREATE POLICY admin_all ON event_lesson_demand TO administrator USING (true);
CREATE POLICY view_visible_instance ON event_lesson_demand FOR SELECT
  USING (EXISTS (SELECT 1 FROM event_instance_registration r WHERE r.id = event_lesson_demand.registration_id));

GRANT ALL ON event_lesson_demand TO anonymous;
