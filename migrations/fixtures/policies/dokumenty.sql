select app_private.drop_policies('public.dokumenty');

create policy current_tenant on dokumenty as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on dokumenty to administrator
  using (true);
create policy member_view on dokumenty for select to member
  using (true);

