CREATE FUNCTION app_private.refresh_event_instance_stats(p_instance_id bigint) RETURNS void
    LANGUAGE sql
    AS $$
  with v as (
    select jsonb_build_object(
      'TOTAL', agg.person_total,
      'UNKNOWN', agg.unknown_count,
      'ATTENDED', agg.attended_count,
      'NOT_EXCUSED', agg.not_excused_count
    ) as stats
    from (
      select
        count(*) filter (where ea.status <> 'cancelled')::int as person_total,
        count(*) filter (where ea.status = 'unknown')::int as unknown_count,
        count(*) filter (where ea.status = 'attended')::int as attended_count,
        count(*) filter (where ea.status = 'not-excused')::int as not_excused_count
      from public.event_attendance ea
      where ea.instance_id = p_instance_id
    ) agg
  )
  update public.event_instance ei set stats = v.stats from v
  where ei.id = p_instance_id and ei.stats is distinct from v.stats;
$$;
