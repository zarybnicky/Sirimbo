select app_private.drop_policies('public.cohort_group');

create policy current_tenant on cohort_group as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on cohort_group to administrator
  using (true);
create policy public_view on cohort_group for select
  using (true);

