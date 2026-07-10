create or replace function public.trainer_group_attendance_completion(
  since timestamp with time zone default null,
  until timestamp with time zone default null
) returns setof public.trainer_group_attendance_completion
  language sql stable as $_$
  with filtered_instances as (
    select ei.id, ei.event_id
    from event_instance ei
    join event e on e.id = ei.event_id
    where ei.tenant_id = current_tenant_id()
      and not ei.is_cancelled
      and e.type = 'group'
      and coalesce(ei.until, ei.since) < coalesce($2, now())
      and ($1 is null or coalesce(ei.since, ei.until) >= $1)
      and ($2 is null or coalesce(ei.until, ei.since) < $2)
  ),
  trainer_instances as (
    select distinct trainer.person_id, trainer.instance_id
    from filtered_instances fi
    cross join lateral (
      select eit.person_id, fi.id as instance_id
      from event_instance_trainer eit
      where eit.instance_id = fi.id
      union
      select et.person_id, fi.id as instance_id
      from event_trainer et
      where et.event_id = fi.event_id
      and not exists (select 1 from event_instance_trainer where instance_id=fi.id)
    ) trainer
  ),
  attendance_stats as (
    select
      ti.person_id,
      ti.instance_id,
      coalesce(stats.attendance_count, 0) as attendance_count,
      coalesce(stats.unknown_count, 0) as unknown_count
    from trainer_instances ti
    left join lateral (
      select
        count(*) as attendance_count,
        count(*) filter (where eir.status = 'unknown') as unknown_count
      from event_instance_registration eir
      where eir.instance_id = ti.instance_id
        and eir.person_id is not null
    ) stats on true
  ),
  per_trainer as (
    select
      person_id,
      count(*) as total_instances,
      count(*) filter (where attendance_count > 0 and unknown_count = 0) as filled_instances,
      count(*) filter (
        where attendance_count > 0
          and unknown_count > 0
          and unknown_count < attendance_count
      ) as partially_filled_instances,
      count(*) filter (where attendance_count = 0 or unknown_count = attendance_count) as unfilled_instances,
      coalesce(sum(attendance_count), 0) as total_attendances,
      coalesce(sum(unknown_count), 0) as pending_attendances
    from attendance_stats
    group by person_id
  )
  select
    person_id,
    total_instances,
    filled_instances,
    partially_filled_instances,
    unfilled_instances,
    case
      when total_instances > 0 then (filled_instances + partially_filled_instances)::double precision / total_instances
      else null
    end as filled_ratio,
    total_attendances,
    pending_attendances
  from per_trainer
  order by filled_ratio asc nulls last, person_id;
$_$;

comment on function public.trainer_group_attendance_completion(timestamp with time zone, timestamp with time zone) is '@simpleCollections only';
grant all on function public.trainer_group_attendance_completion(timestamp with time zone, timestamp with time zone) to anonymous;
