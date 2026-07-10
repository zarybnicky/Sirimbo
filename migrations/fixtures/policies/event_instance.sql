select app_private.drop_policies('public.event_instance');

CREATE POLICY current_tenant ON public.event_instance AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY admin_same_tenant ON public.event_instance TO administrator USING (true);
CREATE POLICY trainer_select ON public.event_instance FOR SELECT TO trainer
  USING (app_private.can_trainer_edit_instance(id));
CREATE POLICY trainer_insert ON public.event_instance FOR INSERT TO trainer
  WITH CHECK (parent_id is null or app_private.can_trainer_edit_instance(parent_id));
CREATE POLICY trainer_update ON public.event_instance FOR UPDATE TO trainer
  USING (app_private.can_trainer_edit_instance(id))
  WITH CHECK (
    app_private.can_trainer_edit_instance(id)
    or (parent_id is not null and app_private.can_trainer_edit_instance(parent_id))
  );
CREATE POLICY trainer_delete ON public.event_instance FOR DELETE TO trainer
  USING (app_private.can_trainer_edit_instance(id));
CREATE POLICY member_view ON public.event_instance FOR SELECT TO member USING (is_visible);
CREATE POLICY public_view ON public.event_instance FOR SELECT TO anonymous USING (is_public);
