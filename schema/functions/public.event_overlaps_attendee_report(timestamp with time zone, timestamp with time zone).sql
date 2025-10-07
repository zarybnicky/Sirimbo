CREATE FUNCTION public.event_overlaps_attendee_report(p_since timestamp with time zone, p_until timestamp with time zone) RETURNS SETOF public.event_overlaps_conflict
    LANGUAGE sql STABLE
    AS $$
  with target_range as (
    select tstzrange(
      coalesce(p_since, '-infinity'::timestamptz),
      coalesce(p_until, 'infinity'::timestamptz),
      '[]'
    ) as range
  ),
  instances as (
    select
      ea.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      e.id as event_id,
      e.name as event_name,
      ea.status
    from public.event_attendance ea
    join public.event_instance ei on ei.id = ea.instance_id
    join public.event e on e.id = ei.event_id
    join public.person p on p.id = ea.person_id
    join target_range tr on true
    where
      ei.tenant_id = public.current_tenant_id()
      and not ei.is_cancelled
      and ea.status <> 'cancelled'
      and ei.range && tr.range
  )
  select
    i1.person_id,
    i1.person_name,
    i1.instance_id as first_instance_id,
    i1.event_id as first_event_id,
    i1.event_name as first_event_name,
    i1.since as first_since,
    i1.until as first_until,
    i2.instance_id as second_instance_id,
    i2.event_id as second_event_id,
    i2.event_name as second_event_name,
    i2.since as second_since,
    i2.until as second_until,
    tstzrange(
      greatest(i1.since, i2.since),
      least(i1.until, i2.until),
      '[]'
    ) as overlap_range
  from instances i1
  join instances i2 on i1.person_id = i2.person_id
    and i1.instance_id < i2.instance_id
    and i1.range && i2.range
    and greatest(i1.since, i2.since) < least(i1.until, i2.until);
$$;

COMMENT ON FUNCTION public.event_overlaps_attendee_report(p_since timestamp with time zone, p_until timestamp with time zone) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_overlaps_attendee_report(p_since timestamp with time zone, p_until timestamp with time zone) TO anonymous;
