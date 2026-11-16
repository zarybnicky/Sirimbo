select app_private.drop_policies('public.couple');

create policy system_admin_all on couple to system_admin using (true);
create policy admin_insert on couple for insert to administrator
  with check (app_private.can_edit_person(man_id) or app_private.can_edit_person(woman_id));
create policy admin_update on couple for update to administrator
  using (app_private.can_edit_person(man_id) or app_private.can_edit_person(woman_id));
create policy admin_delete on couple for delete to administrator
  using (app_private.can_edit_person(man_id) or app_private.can_edit_person(woman_id));
create policy view_visible_person on couple for select
  using (array[man_id, woman_id] && (
    select array_agg(distinct id) from app_private.visible_person_ids(false) id
  ));
create policy event_share_view on couple for select to anonymous
  using (id in (
    select couple_id from event_registration
    where registration_status = 'active' and parent_registration_id is null and couple_id is not null
      and event_id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[])
  ));
