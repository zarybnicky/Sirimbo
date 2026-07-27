select app_private.drop_policies('public.couple');

CREATE POLICY admin_all ON couple TO administrator USING (true);
create policy view_visible_person on couple for select
using (man_id in (select person_id from app_private.visible_person_ids() v(person_id))
  or woman_id in (select person_id from app_private.visible_person_ids() v(person_id)));
create policy event_share_view on couple for select to anonymous
  using (id = any ((select current_setting('jwt.claims.shared.couple_ids', true))::bigint[]));
