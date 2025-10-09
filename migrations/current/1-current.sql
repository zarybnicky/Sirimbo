do $$
begin
  if not exists (
    select 1
    from pg_type t
    join pg_namespace n on n.oid = t.typnamespace
    where t.typname = 'trainer_group_attendance_completion'
      and n.nspname = 'public'
  ) then
    create type public.trainer_group_attendance_completion as (
      person_id bigint,
      total_instances bigint,
      filled_instances bigint,
      partially_filled_instances bigint,
      unfilled_instances bigint,
      filled_ratio double precision,
      total_attendances bigint,
      pending_attendances bigint
    );
  end if;
end;
$$;

comment on type public.trainer_group_attendance_completion is '@foreignKey (person_id) references person (id)';
comment on column public.trainer_group_attendance_completion.person_id is '@foreignKey (person_id) references person (id)';

create or replace function public.trainer_group_attendance_completion(
  since timestamptz default null,
  until timestamptz default null
)
  returns setof trainer_group_attendance_completion
  language sql
  stable
as $$
  with params as (
    select
      since,
      until,
      now() as now
  ),
  eligible_instances as (
    select ei.id, ei.event_id
    from event_instance ei
    join event e on e.id = ei.event_id
    join params p on true
    where ei.tenant_id = current_tenant_id()
      and not ei.is_cancelled
      and e.type = 'group'
      and coalesce(ei.until, ei.since) < coalesce(p.until, p.now)
      and (p.since is null or coalesce(ei.since, ei.until) >= p.since)
      and (p.until is null or coalesce(ei.until, ei.since) < p.until)
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
    pt.trainer_id as person_id,
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
  order by filled_ratio asc nulls last, person_id;
$$;

comment on function public.trainer_group_attendance_completion(timestamptz, timestamptz) is '@simpleCollections only';

grant all on function public.trainer_group_attendance_completion(timestamptz, timestamptz) to anonymous;

select verify_function('trainer_group_attendance_completion');
