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

comment on column public.trainer_group_attendance_completion.person_id is '@foreignKey (person_id) references person (id)';

create or replace function public.trainer_group_attendance_completion(
  since timestamptz default null,
  until timestamptz default null
)
  returns setof trainer_group_attendance_completion
  language sql
  stable
as $$
  with filtered_instances as (
    select ei.id, ei.event_id
    from event_instance ei
    join event e on e.id = ei.event_id
    where ei.tenant_id = current_tenant_id()
      and not ei.is_cancelled
      and e.type = 'group'
      and coalesce(ei.until, ei.since) < coalesce(until, now())
      and (since is null or coalesce(ei.since, ei.until) >= since)
      and (until is null or coalesce(ei.until, ei.since) < until)
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
        count(*) filter (where ea.status = 'unknown') as unknown_count
      from event_attendance ea
      where ea.instance_id = ti.instance_id
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
      coalesce(sum(attendance_count), 0)::bigint as total_attendances,
      coalesce(sum(unknown_count), 0)::bigint as pending_attendances
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
$$;

comment on function public.trainer_group_attendance_completion(timestamptz, timestamptz) is '@simpleCollections only';

grant execute on function public.trainer_group_attendance_completion(timestamptz, timestamptz) to anonymous;

select verify_function('trainer_group_attendance_completion');
