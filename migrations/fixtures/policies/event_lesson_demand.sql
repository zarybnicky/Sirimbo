select app_private.drop_policies('public.event_lesson_demand');
CREATE POLICY admin_all ON public.event_lesson_demand TO administrator USING (true);
CREATE POLICY view_visible_event ON public.event_lesson_demand FOR SELECT USING (event_id = any (SELECT id from event));
