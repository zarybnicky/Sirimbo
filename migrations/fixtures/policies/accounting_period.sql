select app_private.drop_policies('public.accounting_period');

create policy current_tenant on accounting_period as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on accounting_period to administrator
  using (true);
create policy public_view on accounting_period for select
  using (true);

