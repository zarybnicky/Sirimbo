select app_private.drop_policies('public.cohort');

CREATE POLICY current_tenant ON cohort AS RESTRICTIVE USING (tenant_id = (SELECT public.current_tenant_id()));
CREATE POLICY admin_all ON cohort TO administrator USING (true);
CREATE POLICY public_view ON cohort FOR SELECT USING (is_visible);
