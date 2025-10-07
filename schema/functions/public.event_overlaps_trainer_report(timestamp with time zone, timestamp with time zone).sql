CREATE FUNCTION public.event_overlaps_trainer_report(p_since timestamp with time zone, p_until timestamp with time zone) RETURNS SETOF public.event_overlaps_conflict
    LANGUAGE sql STABLE
    AS $$
  with target_range as (
    select tstzrange(
      coalesce(p_since, '-infinity'::timestamptz),
      coalesce(p_until, 'infinity'::timestamptz),
      '[]'
    ) as range
  ),
  trainer_instances as (
    select
      eit.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      e.id as event_id,
      e.name as event_name
    from public.event_instance_trainer eit
    join public.event_instance ei on ei.id = eit.instance_id
    join public.event e on e.id = ei.event_id
    join public.person p on p.id = eit.person_id
    join target_range tr on true
    where
      ei.tenant_id = public.current_tenant_id()
      and not ei.is_cancelled
      and ei.range && tr.range
    union all
    select
      et.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      e.id as event_id,
      e.name as event_name
    from public.event_trainer et
    join public.event_instance ei on ei.event_id = et.event_id
    join public.event e on e.id = ei.event_id
    join public.person p on p.id = et.person_id
    join target_range tr on true
    where
      ei.tenant_id = public.current_tenant_id()
      and not ei.is_cancelled
      and ei.range && tr.range
      and not exists (
        select 1
        from public.event_instance_trainer eit
        where eit.instance_id = ei.id
          and eit.person_id = et.person_id
      )
  )
  select
    ti1.person_id,
    ti1.person_name,
    ti1.instance_id as first_instance_id,
    ti1.event_id as first_event_id,
    ti1.event_name as first_event_name,
    ti1.since as first_since,
    ti1.until as first_until,
    ti2.instance_id as second_instance_id,
    ti2.event_id as second_event_id,
    ti2.event_name as second_event_name,
    ti2.since as second_since,
    ti2.until as second_until,
    tstzrange(
      greatest(ti1.since, ti2.since),
      least(ti1.until, ti2.until),
      '[]'
    ) as overlap_range
  from trainer_instances ti1
  join trainer_instances ti2 on ti1.person_id = ti2.person_id
    and ti1.instance_id < ti2.instance_id
    and ti1.range && ti2.range
    and greatest(ti1.since, ti2.since) < least(ti1.until, ti2.until);
$$;

COMMENT ON FUNCTION public.event_overlaps_trainer_report(p_since timestamp with time zone, p_until timestamp with time zone) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_overlaps_trainer_report(p_since timestamp with time zone, p_until timestamp with time zone) TO anonymous;
