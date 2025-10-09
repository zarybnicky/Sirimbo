create or replace function public.trainer_group_attendance_completion()
  returns table (
    trainer_id bigint,
    trainer_name text,
    total_instances bigint,
    filled_instances bigint,
    partially_filled_instances bigint,
    unfilled_instances bigint,
    filled_ratio double precision,
    total_attendances bigint,
    pending_attendances bigint
  )
  language sql
  stable
as $$
  with eligible_instances as (
    select ei.id, ei.event_id
    from event_instance ei
    join event e on e.id = ei.event_id
    where ei.tenant_id = current_tenant_id()
      and not ei.is_cancelled
      and e.type = 'group'
      and coalesce(ei.until, ei.since) < now()
  ),
  trainer_instances as (
    select distinct assignment.trainer_id, assignment.instance_id
    from (
      select eit.person_id as trainer_id, ei.id as instance_id
      from eligible_instances ei
      join event_instance_trainer eit on eit.instance_id = ei.id
      union
      select et.person_id as trainer_id, ei.id as instance_id
      from eligible_instances ei
      join event_trainer et on et.event_id = ei.event_id
    ) assignment
  ),
  attendance_status as (
    select
      ti.trainer_id,
      ti.instance_id,
      count(ea.*) as attendance_count,
      count(ea.*) filter (where ea.status = 'unknown') as unknown_count
    from trainer_instances ti
    left join event_attendance ea on ea.instance_id = ti.instance_id
    group by ti.trainer_id, ti.instance_id
  ),
  per_trainer as (
    select
      trainer_id,
      count(*) as total_instances,
      count(*) filter (where unknown_count = 0 and attendance_count > 0) as filled_instances,
      count(*) filter (
        where unknown_count > 0
          and unknown_count < attendance_count
      ) as partially_filled_instances,
      count(*) filter (
        where attendance_count = 0
          or unknown_count = attendance_count
      ) as unfilled_instances,
      coalesce(sum(attendance_count), 0)::bigint as total_attendances,
      coalesce(sum(unknown_count), 0)::bigint as pending_attendances
    from attendance_status
    group by trainer_id
  )
  select
    pt.trainer_id,
    p.name as trainer_name,
    pt.total_instances,
    pt.filled_instances,
    pt.partially_filled_instances,
    pt.unfilled_instances,
    case
      when pt.total_instances > 0 then (pt.filled_instances + pt.partially_filled_instances)::double precision / pt.total_instances::double precision
      else null
    end as filled_ratio,
    pt.total_attendances,
    pt.pending_attendances
  from per_trainer pt
  join person p on p.id = pt.trainer_id
  order by filled_ratio asc nulls last, trainer_name;
$$;

comment on function public.trainer_group_attendance_completion() is '@simpleCollections only';

grant all on function public.trainer_group_attendance_completion() to anonymous;

select verify_function('trainer_group_attendance_completion');
