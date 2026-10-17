select app_private.drop_policies('public.form_responses');

create policy current_tenant on form_responses as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on form_responses to administrator
  using (true);
