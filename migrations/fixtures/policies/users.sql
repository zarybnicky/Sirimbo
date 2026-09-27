select app_private.drop_policies('public.user_proxy');
create policy admin_all on user_proxy to administrator using (true);
create policy view_personal on user_proxy for select
  using (user_id = (select current_user_id())
    or id = any (array(select app_private.visible_user_proxy_ids())));

select app_private.drop_policies('public.users');
create policy admin_all on users to administrator using (true);
create policy manage_own on users using (id = (select current_user_id()));
create policy view_shared_person on users for select
  using (id = any (array(select user_id from user_proxy where status = 'active')));

grant all on users, user_proxy to anonymous;
