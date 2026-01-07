CREATE OR REPLACE FUNCTION app_private.relationship_status_next(ts timestamptz, range tstzrange, current public.relationship_status)
  RETURNS public.relationship_status LANGUAGE sql IMMUTABLE
  AS $$
  SELECT CASE
    WHEN ts < lower(range) THEN 'pending'
    WHEN NOT upper_inf(range) AND ts >= upper(range) THEN 'expired'
    WHEN range @> ts THEN 'active'
    ELSE current
  END
$$;

CREATE OR REPLACE FUNCTION app_private.cron_update_memberships() RETURNS void LANGUAGE sql
AS $$
  UPDATE public.user_proxy SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE public.couple SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE public.cohort_membership SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE public.tenant_membership SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE public.tenant_trainer SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE public.tenant_administrator SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);
$$;
