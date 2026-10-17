select app_private.drop_policies('public.location');

create policy current_tenant on location as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on location to administrator
  using (true);
create policy public_view on location for select
  using (true);

