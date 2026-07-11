select app_private.drop_policies('public.event_external_registration');

CREATE POLICY admin_all ON public.event_external_registration TO administrator USING (true);
CREATE POLICY admin_my ON public.event_external_registration TO member
  USING ((SELECT not instance.is_locked FROM event_instance instance WHERE instance_id = instance.id) AND (created_by = public.current_user_id()));
CREATE POLICY register_public ON public.event_external_registration FOR INSERT TO anonymous WITH CHECK ((SELECT instance.is_public FROM event_instance instance WHERE instance_id = instance.id));
CREATE POLICY trainer_same_tenant ON public.event_external_registration TO trainer USING (app_private.can_trainer_edit_instance(instance_id)) WITH CHECK (true);
CREATE POLICY view_visible_instance ON public.event_external_registration FOR SELECT TO member USING (instance_id = any (SELECT id from event_instance));
