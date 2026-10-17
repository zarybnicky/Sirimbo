select app_private.drop_policies('public.location_image');

create policy current_tenant on location_image as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on location_image to administrator
  using (true);
create policy public_view on location_image for select
  using (true);

