select app_private.drop_policies('public.event_external_registration');

CREATE POLICY admin_all ON public.event_external_registration TO administrator USING (true);
CREATE POLICY admin_my ON public.event_external_registration TO member
  USING ((SELECT public.event_is_registration_open(event.*) FROM event WHERE event_id = event.id) AND (created_by = public.current_user_id()));
CREATE POLICY register_public ON public.event_external_registration FOR INSERT TO anonymous WITH CHECK ((SELECT event.is_public FROM event WHERE event_id = event.id));
CREATE POLICY trainer_same_tenant ON public.event_external_registration TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
CREATE POLICY view_visible_event ON public.event_external_registration FOR SELECT TO member USING (event_id = any (SELECT id from event));
