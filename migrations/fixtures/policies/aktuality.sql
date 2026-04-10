select app_private.drop_policies('public.aktuality');

CREATE POLICY current_tenant ON public.aktuality AS RESTRICTIVE USING (tenant_id = (SELECT public.current_tenant_id()));
CREATE POLICY admin_all ON public.aktuality TO administrator USING (true);
CREATE POLICY public_view ON public.aktuality FOR SELECT USING (is_visible);
