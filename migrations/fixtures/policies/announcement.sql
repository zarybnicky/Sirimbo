select app_private.drop_policies('public.announcement');

create policy current_tenant on announcement as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on announcement to administrator using (true);
create policy trainer_manage_own on announcement to trainer
  using (author_id = (select current_user_id()));
create policy member_view on announcement for select to member
  using (id in (select app_private.visible_announcement_ids()));

select app_private.drop_policies('public.announcement_audience');

create policy current_tenant on announcement_audience as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on announcement_audience to administrator using (true);
create policy trainer_manage on announcement_audience to trainer using (true);
create policy member_view on announcement_audience for select to member using (true);

grant all on table announcement, announcement_audience to anonymous;
