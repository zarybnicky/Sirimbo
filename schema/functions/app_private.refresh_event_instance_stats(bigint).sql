CREATE FUNCTION app_private.refresh_event_instance_stats(p_instance_id bigint) RETURNS void
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'pg_temp'
    AS $$
  select 1 from public.event_instance where id = p_instance_id for no key update;

  update public.event_instance instance
  set stats = actual.stats
  from (
    select jsonb_build_object(
      'TOTAL', count(*)::int,
      'UNKNOWN', count(*) filter (where status = 'unknown')::int,
      'ATTENDED', count(*) filter (where status = 'attended')::int,
      'NOT_EXCUSED', count(*) filter (where status = 'not-excused')::int
    ) as stats
    from public.event_instance_registration
    where instance_id = p_instance_id
      and person_id is not null
      and registration_status = 'active'
  ) actual
  where instance.id = p_instance_id and instance.stats is distinct from actual.stats;
$$;

REVOKE ALL ON FUNCTION app_private.refresh_event_instance_stats(p_instance_id bigint) FROM PUBLIC;
