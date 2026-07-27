select app_private.drop_policies('public.cohort');

create policy current_tenant on cohort as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on cohort to administrator using (true);
create policy public_view on cohort for select using (is_visible);
create policy event_share_view on cohort for select to anonymous
  using (id = any ((select current_setting('jwt.claims.shared.cohort_ids', true))::bigint[]));
