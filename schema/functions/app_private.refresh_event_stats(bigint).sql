CREATE FUNCTION app_private.refresh_event_stats(p_event_id bigint) RETURNS void
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select 1 from event where id = p_event_id for no key update;

  update event instance
  set stats = actual.stats
  from (
    select row(
      count(*)::int,
      count(*) filter (where status = 'unknown')::int,
      count(*) filter (where status = 'attended')::int,
      count(*) filter (where status = 'not-excused')::int
    )::event_stats as stats
    from event_registration
    where event_id = p_event_id
      and person_id is not null
      and registration_status = 'active'
  ) actual
  where instance.id = p_event_id and instance.stats is distinct from actual.stats;
$$;
