select app_private.drop_policies('public.couple');

CREATE POLICY admin_all ON couple TO administrator USING (true);
create policy view_visible_person on couple for select using (
  --array[man_id, woman_id] && (select app_private.visible_person_ids_array())
  man_id in (select person_id from app_private.visible_person_ids() v(person_id))
  or woman_id in (select person_id from app_private.visible_person_ids() v(person_id))
);
