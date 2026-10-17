select app_private.drop_policies('public.access_event');

create policy current_tenant on access_event as restrictive
  using (tenant_id = current_tenant_id());
create policy admin_view on access_event for select to administrator
  using (true);
create policy my_view on access_event for select
  using (person_id = any ((select public.current_person_ids())::bigint[]));
