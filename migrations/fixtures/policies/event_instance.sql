select app_private.drop_policies('public.event_instance');
CREATE POLICY admin_same_tenant ON public.event_instance TO administrator USING (true);
CREATE POLICY trainer_same_tenant ON public.event_instance TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
CREATE POLICY view_visible_event ON public.event_instance FOR SELECT USING (event_id = any (select id from event));
