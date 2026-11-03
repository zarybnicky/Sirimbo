CREATE FUNCTION public.trainer_group_attendance_completion(since timestamp with time zone DEFAULT NULL::timestamp with time zone, until timestamp with time zone DEFAULT NULL::timestamp with time zone) RETURNS SETOF public.trainer_group_attendance_completion
    LANGUAGE sql STABLE
    AS $_$
  with attendance_stats as (
    select
      trainer.person_id,
      (instance.stats).total as attendance_count,
      (instance.stats).unknown as unknown_count
    from event instance
    join event_trainer trainer on trainer.event_id = instance.id
    where instance.tenant_id = current_tenant_id()
      and not instance.is_cancelled
      and instance.type = 'group'
      and instance.until < coalesce($2, now())
      and ($1 is null or instance.since >= $1)
  ),
  per_trainer as (
    select
      person_id,
      count(*) as total_events,
      count(*) filter (where attendance_count > 0 and unknown_count = 0) as filled_events,
      count(*) filter (
        where attendance_count > 0
          and unknown_count > 0
          and unknown_count < attendance_count
      ) as partially_filled_events,
      count(*) filter (where attendance_count = 0 or unknown_count = attendance_count) as unfilled_events,
      coalesce(sum(attendance_count), 0) as total_attendances,
      coalesce(sum(unknown_count), 0) as pending_attendances
    from attendance_stats
    group by person_id
  )
  select
    person_id,
    total_events,
    filled_events,
    partially_filled_events,
    unfilled_events,
    case
      when total_events > 0 then (filled_events + partially_filled_events)::double precision / total_events
      else null
    end as filled_ratio,
    total_attendances,
    pending_attendances
  from per_trainer
  order by filled_ratio asc nulls last, person_id;
$_$;

COMMENT ON FUNCTION public.trainer_group_attendance_completion(since timestamp with time zone, until timestamp with time zone) IS '@behavior +list -connection';

GRANT ALL ON FUNCTION public.trainer_group_attendance_completion(since timestamp with time zone, until timestamp with time zone) TO anonymous;
