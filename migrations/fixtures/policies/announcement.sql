select app_private.drop_policies('public.announcement');

create policy current_tenant on announcement as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on announcement to administrator using (true);
create policy trainer_manage_own on announcement to trainer
  using (author_id = (select current_user_id()));
create policy member_view on announcement for select to member
  using (status in ('published', 'archived') and (
    id not in (select announcement_id from announcement_audience)
    or id in (
      select announcement_id from announcement_audience
      where cohort_id in (
        select cohort_id from current_cohort_membership where person_id = any ((select current_person_ids())::bigint[])
      )
      or audience_role = 'member' and exists (
        select from current_tenant_membership where person_id = any ((select current_person_ids())::bigint[])
      )
      or audience_role = 'trainer' and exists (
        select from current_tenant_trainer where person_id = any ((select current_person_ids())::bigint[])
      )
      or audience_role = 'administrator' and exists (
        select from current_tenant_administrator where person_id = any ((select current_person_ids())::bigint[])
      )
    )
  ));

select app_private.drop_policies('public.announcement_audience');

create policy current_tenant on announcement_audience as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on announcement_audience to administrator using (true);
create policy trainer_manage on announcement_audience to trainer using (true);
create policy member_view on announcement_audience for select to member using (true);

grant all on table announcement, announcement_audience to anonymous;
