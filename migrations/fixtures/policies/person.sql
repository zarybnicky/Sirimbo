select app_private.drop_policies('public.person');

create policy admin_all on person to administrator using (true);
create policy admin_myself on person for update
  using (id = any ((select current_person_ids())::bigint[]));
create policy view_tenant_or_trainer on person for select
  using (id in (select app_private.visible_person_ids()));
create policy event_share_view on person for select to anonymous
  using (id = any ((select current_setting('jwt.claims.shared.person_ids', true))::bigint[]));
