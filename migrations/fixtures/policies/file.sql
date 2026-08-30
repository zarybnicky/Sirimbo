select app_private.drop_policies('public.file');
create policy current_tenant on file as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on file to administrator
  using (true);
create policy uploader_manage on file to trainer
  using (uploaded_by = (select current_user_id()));
create policy visible on file for select
  using (id in (select id from app_private.visible_file_ids() visible(id)));

select app_private.drop_policies('public.announcement_attachment');
create policy current_tenant on announcement_attachment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on announcement_attachment to administrator
  using (true);
create policy trainer_manage on announcement_attachment to trainer
  using (announcement_id in (
    select id from announcement where author_id = (select current_user_id())));
create policy member_view on announcement_attachment for select
  using (announcement_id in (select id from announcement));

select app_private.drop_policies('public.article_attachment');
create policy current_tenant on article_attachment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on article_attachment to administrator
  using (true);
create policy public_view on article_attachment for select
  using (aktuality_id in (select id from aktuality));
