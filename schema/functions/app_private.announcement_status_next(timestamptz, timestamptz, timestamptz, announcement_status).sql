CREATE FUNCTION app_private.announcement_status_next(ts timestamp with time zone, scheduled_since timestamp with time zone, scheduled_until timestamp with time zone, current_status public.announcement_status) RETURNS public.announcement_status
    LANGUAGE sql IMMUTABLE
    AS $$
  select case
    when current_status in ('draft', 'archived') then current_status
    when scheduled_until is not null and ts >= scheduled_until then 'archived'
    when scheduled_since is not null and ts < scheduled_since then 'scheduled'
    else 'published'
  end::announcement_status;
$$;
