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
  with filtered_instances as (
    select ei.id
    from event ei
    where ei.tenant_id = current_tenant_id()
      and not ei.is_cancelled
      and ei.type = 'group'
      and coalesce(ei.until, ei.since) < coalesce($2, now())
      and ($1 is null or coalesce(ei.since, ei.until) >= $1)
      and ($2 is null or coalesce(ei.until, ei.since) < $2)
  ),
  trainer_instances as (
    select effective_trainer.person_id, fi.id as event_id
    from filtered_instances fi
    join event instance on instance.id = fi.id
    cross join lateral app_private.event_trainers_at(instance, instance.since) effective_trainer
  ),
  attendance_stats as (
    select
      ti.person_id,
      ti.event_id,
      coalesce(stats.attendance_count, 0) as attendance_count,
      coalesce(stats.unknown_count, 0) as unknown_count
    from trainer_instances ti
    left join lateral (
      select
        count(*) as attendance_count,
        count(*) filter (where eir.status = 'unknown') as unknown_count
      from event_registration eir
      where eir.event_id = ti.event_id
        and eir.person_id is not null
        and eir.registration_status = 'active'
    ) stats on true
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
