CREATE FUNCTION public.event_overlaps_trainer_report(p_since timestamp with time zone, p_until timestamp with time zone) RETURNS SETOF public.event_conflict
    LANGUAGE sql STABLE
    AS $$
  with trainer_instances as (
    select
      eit.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from event_instance ei
    join event_instance_trainer eit on ei.id = eit.instance_id
    join person p on p.id = eit.person_id
    where not ei.is_cancelled
      and ei.range && tstzrange(coalesce(p_since, '-infinity'::timestamptz), coalesce(p_until, 'infinity'::timestamptz), '[]')
  )
  select
    ti1.person_id,
    ti1.person_name,
    ti1.instance_id as first_instance_id,
    ti1.event_name as first_event_name,
    ti1.since as first_since,
    ti1.until as first_until,
    ti2.instance_id as second_instance_id,
    ti2.event_name as second_event_name,
    ti2.since as second_since,
    ti2.until as second_until,
    tstzrange(greatest(ti1.since, ti2.since), least(ti1.until, ti2.until), '[]') as overlap_range
  from trainer_instances ti1
  join trainer_instances ti2 on ti1.person_id = ti2.person_id
    and ti1.instance_id < ti2.instance_id
    and ti1.range && ti2.range
    and greatest(ti1.since, ti2.since) < least(ti1.until, ti2.until);
$$;

COMMENT ON FUNCTION public.event_overlaps_trainer_report(p_since timestamp with time zone, p_until timestamp with time zone) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_overlaps_trainer_report(p_since timestamp with time zone, p_until timestamp with time zone) TO anonymous;
