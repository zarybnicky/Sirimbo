alter table users drop column if exists u_timestamp;

drop trigger if exists _200_refresh_auth_details on cohort_membership;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on cohort_membership
FOR EACH STATEMENT EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

drop trigger if exists _200_refresh_auth_details on couple;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on couple
FOR EACH STATEMENT EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

drop trigger if exists _200_refresh_auth_details on tenant_membership;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on tenant_membership
FOR EACH STATEMENT EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

drop trigger if exists _200_refresh_auth_details on tenant_trainer;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on tenant_trainer
FOR EACH STATEMENT EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

drop trigger if exists _200_refresh_auth_details on tenant_administrator;
CREATE TRIGGER _200_refresh_auth_details
after insert or update or delete on tenant_administrator
FOR EACH STATEMENT EXECUTE PROCEDURE app_private.tg_auth_details__refresh();

--!include functions/my_announcements.sql

create or replace function public.event_overlaps_attendee_report(
  p_since timestamp with time zone,
  p_until timestamp with time zone
)
returns table (
  person_id bigint,
  person_name text,
  first_instance_id bigint,
  first_event_id bigint,
  first_event_name text,
  first_since timestamp with time zone,
  first_until timestamp with time zone,
  first_status public.attendance_type,
  second_instance_id bigint,
  second_event_id bigint,
  second_event_name text,
  second_since timestamp with time zone,
  second_until timestamp with time zone,
  second_status public.attendance_type,
  overlap_range tstzrange
)
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
    i1.status as first_status,
    i2.instance_id as second_instance_id,
    i2.event_id as second_event_id,
    i2.event_name as second_event_name,
    i2.since as second_since,
    i2.until as second_until,
    i2.status as second_status,
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

select verify_function('public.event_overlaps_attendee_report');

create or replace function public.event_overlaps_trainer_report(
  p_since timestamp with time zone,
  p_until timestamp with time zone
)
returns table (
  trainer_id bigint,
  trainer_name text,
  first_instance_id bigint,
  first_event_id bigint,
  first_event_name text,
  first_since timestamp with time zone,
  first_until timestamp with time zone,
  first_assignment_source text,
  second_instance_id bigint,
  second_event_id bigint,
  second_event_name text,
  second_since timestamp with time zone,
  second_until timestamp with time zone,
  second_assignment_source text,
  overlap_range tstzrange
)
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
  instance_assignments as (
    select
      eit.person_id,
      eit.instance_id,
      'instance'::text as assignment_source
    from public.event_instance_trainer eit
  ),
  event_assignments as (
    select
      et.person_id,
      ei.id as instance_id,
      'event'::text as assignment_source
    from public.event_trainer et
    join public.event_instance ei on ei.event_id = et.event_id
    where not exists (
      select 1
      from public.event_instance_trainer eit
      where eit.instance_id = ei.id
    )
  ),
  assignments as (
    select * from instance_assignments
    union all
    select * from event_assignments
  ),
  trainer_instances as (
    select
      a.person_id,
      p.name as trainer_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      e.id as event_id,
      e.name as event_name,
      a.assignment_source
    from assignments a
    join public.event_instance ei on ei.id = a.instance_id
    join public.event e on e.id = ei.event_id
    join public.person p on p.id = a.person_id
    join target_range tr on true
    where
      ei.tenant_id = public.current_tenant_id()
      and not ei.is_cancelled
      and ei.range && tr.range
  )
  select
    ti1.person_id as trainer_id,
    ti1.trainer_name,
    ti1.instance_id as first_instance_id,
    ti1.event_id as first_event_id,
    ti1.event_name as first_event_name,
    ti1.since as first_since,
    ti1.until as first_until,
    ti1.assignment_source as first_assignment_source,
    ti2.instance_id as second_instance_id,
    ti2.event_id as second_event_id,
    ti2.event_name as second_event_name,
    ti2.since as second_since,
    ti2.until as second_until,
    ti2.assignment_source as second_assignment_source,
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

select verify_function('public.event_overlaps_trainer_report');
