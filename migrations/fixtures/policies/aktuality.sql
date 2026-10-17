select app_private.drop_policies('public.aktuality');

create policy current_tenant on aktuality as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on aktuality to administrator
  using (true);
create policy public_view on aktuality for select
  using (is_visible);

grant all on table aktuality to anonymous;
