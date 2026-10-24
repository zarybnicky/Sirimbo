select app_private.drop_policies('public.couple');

create policy admin_all on couple to administrator using (true);
create policy view_visible_person on couple for select
using (man_id in (select app_private.visible_person_ids())
  or woman_id in (select app_private.visible_person_ids()));
create policy event_share_view on couple for select to anonymous
  using (id in (
    select couple_id from event_registration
    where registration_status = 'active' and parent_registration_id is null and couple_id is not null
      and event_id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[])
  ));
