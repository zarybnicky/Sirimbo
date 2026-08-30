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

create or replace function app_private.cron_update_memberships() returns void language sql
as $$
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

  UPDATE announcement
  SET status = app_private.announcement_status_next(now(), scheduled_since, scheduled_until, status)
  WHERE status IN ('scheduled', 'published')
    AND status IS DISTINCT FROM app_private.announcement_status_next(now(), scheduled_since, scheduled_until, status);
$$;
