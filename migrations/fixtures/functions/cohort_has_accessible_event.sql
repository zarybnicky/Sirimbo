create or replace function app_private.cohort_has_accessible_event(p_cohort_id bigint)
  returns boolean
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  -- Most cohorts have a public or member-visible event. Check their indexed
  -- links before enumerating the caller's additional event access.
  select exists (
    select 1
    from event_target_cohort target
    join event e on e.tenant_id = target.tenant_id and e.id = target.event_id
    where target.tenant_id = (select current_tenant_id())
      and target.cohort_id = p_cohort_id
      and (e.is_public or (e.is_visible
        and (select pg_has_role(current_setting('role'), 'member', 'member'))))
  ) or exists (
    with targets as materialized (
      select event_id from event_target_cohort
      where tenant_id = (select current_tenant_id()) and cohort_id = p_cohort_id
    )
    select 1 from targets
    where event_id = any (array(select app_private.accessible_event_ids()))
  );
$$;

grant execute on function app_private.cohort_has_accessible_event(bigint) to anonymous;
