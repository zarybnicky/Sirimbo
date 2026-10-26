select app_private.drop_policies('public.file');
create policy admin_all on file to administrator
  using (tenant_id = (select current_tenant_id()));
create policy uploader_manage on file
  using (tenant_id = (select current_tenant_id())
    and uploaded_by = (select current_user_id()));
create policy visible on file for select
  using (
    (tenant_id = (select current_tenant_id())
      and id in (select app_private.visible_file_ids()))
    -- These uncorrelated subqueries use entity RLS, including share and partner access.
    or id in (
      select image_id from person where image_id is not null
      union all
      select image_id from couple where image_id is not null
      union all
      select image_id from cohort where image_id is not null
    )
  );

select app_private.drop_policies('public.announcement_attachment');
create policy current_tenant on announcement_attachment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on announcement_attachment to administrator
  using (true);
create policy trainer_manage on announcement_attachment to trainer
  using (exists (
    select 1 from announcement a
    where a.id = announcement_attachment.announcement_id
      and a.author_id = (select current_user_id())));
create policy member_view on announcement_attachment for select
  using (exists (select 1 from announcement a where a.id = announcement_attachment.announcement_id));

select app_private.drop_policies('public.article_attachment');
create policy current_tenant on article_attachment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on article_attachment to administrator
  using (true);
create policy public_view on article_attachment for select
  using (exists (select 1 from aktuality a where a.id = article_attachment.aktuality_id));
