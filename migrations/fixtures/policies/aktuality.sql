select app_private.drop_policies('public.aktuality');

CREATE POLICY current_tenant ON aktuality AS RESTRICTIVE USING (tenant_id = (SELECT current_tenant_id()));
CREATE POLICY admin_all ON aktuality TO administrator USING (true);
CREATE POLICY public_view ON aktuality FOR SELECT USING (is_visible);
