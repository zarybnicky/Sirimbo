select app_private.drop_policies('public.cohort_membership');

create policy my_tenant on cohort_membership as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on cohort_membership to administrator
  using (true);
create policy view_all on cohort_membership for select
  using (true);

