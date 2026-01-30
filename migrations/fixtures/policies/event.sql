select app_private.drop_policies('public.event');

CREATE POLICY current_tenant ON public.event AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY admin_same_tenant ON public.event TO administrator USING (true);
CREATE POLICY trainer_same_tenant ON public.event TO trainer USING (app_private.can_trainer_edit_event(id)) WITH CHECK (true);
CREATE POLICY member_view ON public.event FOR SELECT TO member USING (is_visible);
CREATE POLICY public_view ON public.event FOR SELECT TO anonymous USING (is_public);
