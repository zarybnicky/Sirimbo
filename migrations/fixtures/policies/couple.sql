select app_private.drop_policies('public.couple');

CREATE POLICY admin_all ON couple TO administrator USING (true);
create policy view_visible_person on couple for select using (
  exists (select 1 from person where man_id = person.id)
  or
  exists (select 1 from person where woman_id = person.id)
);
