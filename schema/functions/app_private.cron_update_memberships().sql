CREATE FUNCTION app_private.cron_update_memberships() RETURNS void
    LANGUAGE sql
    AS $$
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
