select app_private.drop_policies('public.cohort');

create policy current_tenant on cohort as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on cohort to administrator
  using (true);
create policy public_view on cohort for select to anonymous
  using (is_visible or app_private.cohort_has_accessible_event(id));
