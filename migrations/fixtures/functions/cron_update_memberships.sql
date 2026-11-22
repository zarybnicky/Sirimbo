create or replace function app_private.relationship_status_next(ts timestamptz, range tstzrange, current relationship_status)
  returns relationship_status
  language sql
  immutable
as $$
  select case
    when ts < lower(range) then 'pending'
    when not upper_inf(range) and ts >= upper(range) then 'expired'
    when range @> ts then 'active'
    else current
  end
$$;

create or replace function app_private.cron_update_memberships() returns void language plpgsql
as $$
declare
  announcement_tenant_id bigint;
  original_tenant_id bigint := current_tenant_id();
begin
  UPDATE user_proxy SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE couple SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE cohort_membership SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE tenant_membership SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE tenant_trainer SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE tenant_administrator SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE access_credential SET status = app_private.relationship_status_next(now(), valid_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), valid_range, status);

  for announcement_tenant_id in
    select distinct tenant_id from announcement
    where status in ('scheduled', 'published')
      and status is distinct from app_private.announcement_status_next(now(), scheduled_since, scheduled_until, status)
    order by tenant_id
  loop
    perform set_config('jwt.claims.tenant_id', announcement_tenant_id::text, true);
    update announcement
    set status = app_private.announcement_status_next(now(), scheduled_since, scheduled_until, status)
    where tenant_id = announcement_tenant_id and status in ('scheduled', 'published')
      and status is distinct from app_private.announcement_status_next(now(), scheduled_since, scheduled_until, status);
  end loop;
  perform set_config('jwt.claims.tenant_id', original_tenant_id::text, true);
end;
$$;
