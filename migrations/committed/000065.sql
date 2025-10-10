--! Previous: sha1:9da5f739a5cc82ff294b65f0c3c91525158de755
--! Hash: sha1:c29d6a1b66fade365ef49b36820487d7ad3d9eb1

--! split: 1-current.sql
drop function if exists upsert_event(info event, instances event_instance[], trainers event_trainer[], cohorts event_target_cohort[], registrations event_registration[]);
drop function if exists upsert_event(info event_type_input, instances event_instance_type_input[], trainers event_trainer_type_input[], cohorts event_target_cohort_type_input[], registrations event_registration_type_input[]);

drop type if exists event_type_input;
drop type if exists event_instance_type_input;
drop type if exists event_instance_trainer_type_input;
drop type if exists event_trainer_type_input;
drop type if exists event_target_cohort_type_input;
drop type if exists event_registration_type_input;

CREATE TYPE public.event_type_input AS (
  id bigint,
  name text,
  summary text,
  description text,
  description_member text,
  type public.event_type,
  location_id bigint,
  location_text text,
  capacity integer,
  is_visible boolean,
  is_public boolean,
  is_locked boolean,
  enable_notes boolean,
  payment_type event_payment_type,
  member_price public.price,
  guest_price public.price
);

CREATE TYPE public.event_instance_trainer_type_input AS (
  id bigint,
  person_id bigint
);

CREATE TYPE public.event_instance_type_input AS (
  id bigint,
  since timestamp with time zone,
  until timestamp with time zone,
  is_cancelled boolean,
  trainers event_instance_trainer_type_input[]
);

CREATE TYPE public.event_trainer_type_input AS (
  id bigint,
  person_id bigint,
  lessons_offered integer
);

CREATE TYPE public.event_target_cohort_type_input AS (
  id bigint,
  cohort_id bigint
);

CREATE TYPE public.event_registration_type_input AS (
  id bigint,
  person_id bigint,
  couple_id bigint
);

CREATE OR REPLACE FUNCTION upsert_event(
  info event_type_input,
  instances event_instance_type_input[],
  trainers event_trainer_type_input[],
  cohorts event_target_cohort_type_input[],
  registrations event_registration_type_input[]
) RETURNS event LANGUAGE plpgsql AS $$
declare
  instance event_instance_type_input;
  trainer event_trainer_type_input;
  instance_trainer event_instance_trainer_type_input;
  cohort event_target_cohort_type_input;
  registration event_registration_type_input;
  v_event event;
  v_instance event_instance;
begin
  if info.id is not null then
    update event set
      name=info.name,
      summary=info.summary,
      description=info.description,
      description_member=info.description_member,
      type=info.type,
      location_id=info.location_id,
      location_text=info.location_text,
      capacity=info.capacity,
      is_visible=info.is_visible,
      is_public=info.is_public,
      is_locked=info.is_locked,
      enable_notes=info.enable_notes,
      payment_type=info.payment_type,
      guest_price=info.guest_price,
      member_price=info.member_price
    where id=info.id
    returning * into v_event;
  else
    insert into event (
      name,
      summary,
      description,
      description_member,
      type,
      location_id,
      location_text,
      capacity,
      is_visible,
      is_public,
      is_locked,
      enable_notes,
      payment_type,
      guest_price,
      member_price
    )
    values (
      info.name,
      info.summary,
      info.description,
      info.description_member,
      info.type,
      info.location_id,
      info.location_text,
      info.capacity,
      info.is_visible,
      info.is_public,
      info.is_locked,
      info.enable_notes,
      info.payment_type,
      info.guest_price,
      info.member_price
    )
    returning * into v_event;
  end if;

  foreach instance in array instances loop
    if instance.id is not null then
      if instance.since is null and instance.until is null then
        delete from event_instance where id=instance.id;
        v_instance.id := null;
      else
        update event_instance
        set since=instance.since, until=instance.until, is_cancelled=instance.is_cancelled
        where id=instance.id
        returning * into v_instance;
      end if;
    else
      insert into event_instance (event_id, since, until, is_cancelled)
      values (v_event.id, instance.since, instance.until, instance.is_cancelled)
      returning * into v_instance;
    end if;

    if v_instance.id is not null then
      foreach instance_trainer in array instance.trainers loop
        if instance_trainer.id is not null then
          if instance_trainer.person_id is null then
            delete from event_instance_trainer where id=instance_trainer.id;
          end if;
        else
          insert into event_instance_trainer (instance_id, person_id)
          values (v_instance.id, instance_trainer.person_id);
        end if;
      end loop;
    end if;
  end loop;

  foreach trainer in array trainers loop
    if trainer.id is not null then
      if trainer.person_id is null then
        delete from event_trainer where id=trainer.id;
      else
        update event_trainer set lessons_offered=trainer.lessons_offered where id=trainer.id;
      end if;
    else
      insert into event_trainer (event_id, person_id, lessons_offered)
      values (v_event.id, trainer.person_id, coalesce(trainer.lessons_offered, 0))
      on conflict (event_id, person_id) do update
      set lessons_offered = coalesce(trainer.lessons_offered, 0);
    end if;
  end loop;

  foreach cohort in array cohorts loop
    if cohort.id is not null then
      if cohort.cohort_id is null then
        delete from event_target_cohort where id=cohort.id;
      end if;
    else
      insert into event_target_cohort (event_id, cohort_id)
      values (v_event.id, cohort.cohort_id);
    end if;
  end loop;

  foreach registration in array registrations loop
    if registration.id is not null then
      if registration.person_id is null and registration.couple_id is null then
        delete from event_registration where id=registration.id;
      else
        update event_registration
        set person_id=registration.person_id, couple_id=registration.couple_id
        where id=registration.id;
      end if;
    else
      insert into event_registration (event_id, person_id, couple_id)
      values (v_event.id, registration.person_id, registration.couple_id);
    end if;
  end loop;

  return v_event;
end;
$$;

select verify_function('public.upsert_event');

COMMENT ON FUNCTION public.upsert_event is null;
GRANT ALL ON FUNCTION public.upsert_event TO anonymous;

--! split: 4-attendance-roll-up.sql
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
      person_id integer,
      total_instances integer,
      filled_instances integer,
      partially_filled_instances integer,
      unfilled_instances integer,
      filled_ratio double precision,
      total_attendances integer,
      pending_attendances integer
    );
  end if;
end;
$$;

comment on type public.trainer_group_attendance_completion is '@foreignKey (person_id) references person (id)';

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
$$;

comment on function public.trainer_group_attendance_completion is '@simpleCollections only';

grant execute on function public.trainer_group_attendance_completion to anonymous;
