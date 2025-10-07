do $$
begin
  if not exists (
    select 1
    from pg_catalog.pg_type t
    join pg_catalog.pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public'
      and t.typname = 'event_overlaps_conflict'
  ) then
    create type public.event_overlaps_conflict as (
      person_id bigint,
      person_name text,
      first_instance_id bigint,
      first_event_id bigint,
      first_event_name text,
      first_since timestamp with time zone,
      first_until timestamp with time zone,
      second_instance_id bigint,
      second_event_id bigint,
      second_event_name text,
      second_since timestamp with time zone,
      second_until timestamp with time zone,
      overlap_range tstzrange
    );
  end if;
end;
$$;

comment on type public.event_overlaps_conflict is 'Pair of overlapping event instances for a single attendee or trainer.';

create or replace function public.event_overlaps_attendee_report(
  p_since timestamp with time zone,
  p_until timestamp with time zone
)
returns setof public.event_overlaps_conflict
language sql
stable
as $$
  with target_range as (
    select tstzrange(
      coalesce(p_since, '-infinity'::timestamp with time zone),
      coalesce(p_until, 'infinity'::timestamp with time zone),
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

comment on function public.event_overlaps_attendee_report(
  p_since timestamp with time zone,
  p_until timestamp with time zone
) is 'Lists pairs of overlapping event instances for attendees so double bookings can be highlighted.';

grant all on function public.event_overlaps_attendee_report(
  p_since timestamp with time zone,
  p_until timestamp with time zone
) to anonymous;

create or replace function public.event_overlaps_trainer_report(
  p_since timestamp with time zone,
  p_until timestamp with time zone
)
returns setof public.event_overlaps_conflict
language sql
stable
as $$
  with target_range as (
    select tstzrange(
      coalesce(p_since, '-infinity'::timestamp with time zone),
      coalesce(p_until, 'infinity'::timestamp with time zone),
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

comment on function public.event_overlaps_trainer_report(
  p_since timestamp with time zone,
  p_until timestamp with time zone
) is 'Lists pairs of overlapping event instances for trainers to surface teaching conflicts.';

grant all on function public.event_overlaps_trainer_report(
  p_since timestamp with time zone,
  p_until timestamp with time zone
) to anonymous;
