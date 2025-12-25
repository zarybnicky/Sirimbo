select app_private.drop_policies('public.person');
create policy admin_all on person to administrator using (true);
create policy admin_myself on person for update using (id = any (current_person_ids()));
create policy view_tenant_or_trainer on person for select using (id in (select person_id from app_private.visible_person_ids() v(person_id)));
