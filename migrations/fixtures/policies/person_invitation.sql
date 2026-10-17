select app_private.drop_policies('public.person_invitation');

create policy current_tenant on person_invitation as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on person_invitation to administrator
  using (true);
