select app_private.drop_policies('public.cohort_subscription');

create policy current_tenant on cohort_subscription as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on cohort_subscription to administrator
  using (true);
create policy member_view on cohort_subscription for select to member
  using (true);

