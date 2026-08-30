create or replace function app_private.visible_announcement_ids()
  returns setof bigint
  language sql stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  with eligible as (
    select announcement.id
    from announcement
    where pg_catalog.pg_has_role(pg_catalog.current_setting('role'), 'member', 'member')
      and announcement.tenant_id = (select current_tenant_id())
      and announcement.status in ('published', 'archived')
  )
  select eligible.id
  from eligible
  where not exists (
    select from announcement_audience where announcement_id = eligible.id
  )
  union all
  select announcement_id
  from announcement_audience join eligible on eligible.id = announcement_id
  where (
    cohort_id in (
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
  );
$$;

grant execute on function app_private.visible_announcement_ids() to anonymous;
