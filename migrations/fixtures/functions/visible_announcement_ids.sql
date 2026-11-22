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
      select unnest(current_cohort_ids) from app_private.tenant_relation
      where tenant_id = (select current_tenant_id())
        and person_id = any ((select current_person_ids())::bigint[])
    )
    or audience_role in (
      select unnest(current_roles) from app_private.current_tenant_relation
      where person_id = any ((select current_person_ids())::bigint[])
    )
  );
$$;

grant execute on function app_private.visible_announcement_ids() to anonymous;
