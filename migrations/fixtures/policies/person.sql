select app_private.drop_policies('public.person');

create policy system_admin_all on person to system_admin using (true);
create policy admin_update on person for update to administrator
  using (app_private.can_edit_person(id));
create policy admin_delete on person for delete to administrator
  using (app_private.can_edit_person(id));
create policy admin_myself on person for update
  using (id = any ((select current_person_ids())::bigint[]));
create policy view_tenant_or_trainer on person for select
  using (id in (select app_private.visible_person_ids(true)));

create policy event_share_view on person for select to anonymous
  using (id in (
    select person_id from event_registration
    where registration_status = 'active' and person_id is not null
      and event_id = any ((select app_private.current_shared_event_ids())::bigint[])
    union all
    select person_id from event_trainer
    where event_id = any ((select app_private.current_shared_event_ids())::bigint[])
  ));
