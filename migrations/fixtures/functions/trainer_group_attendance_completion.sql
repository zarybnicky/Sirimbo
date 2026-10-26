drop function if exists trainer_group_attendance_completion;
drop type if exists trainer_group_attendance_completion;

create type trainer_group_attendance_completion as (
  person_id bigint,
  total_events integer,
  filled_events integer,
  partially_filled_events integer,
  unfilled_events integer,
  filled_ratio double precision,
  total_attendances integer,
  pending_attendances integer
);

comment on type trainer_group_attendance_completion is '@foreignKey (person_id) references person (id)';

create or replace function trainer_group_attendance_completion(
  since timestamp with time zone default null,
  until timestamp with time zone default null
) returns setof trainer_group_attendance_completion
  language sql stable as $_$
  with attendance_stats as (
    select
      trainer.person_id,
      coalesce((instance.stats->>'TOTAL')::integer, 0) as attendance_count,
      coalesce((instance.stats->>'UNKNOWN')::integer, 0) as unknown_count
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

comment on function trainer_group_attendance_completion is '@behavior +list -connection';
grant all on function trainer_group_attendance_completion to anonymous;
