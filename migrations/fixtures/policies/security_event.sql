select app_private.drop_policies('public.security_event');

create policy current_tenant on security_event as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_view on security_event for select to administrator
  using (true);
create policy self_view on security_event for select to member
  using (user_id = (select current_user_id())
    or person_id = any((select current_person_ids())::bigint[]));

