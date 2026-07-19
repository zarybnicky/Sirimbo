drop function if exists event_overlaps_attendee_report;
drop function if exists event_overlaps_trainer_report;
drop type if exists event_overlaps_conflict;
drop type if exists event_conflict;

create type event_conflict as (
  person_id bigint,
  person_name text,
  first_instance_id bigint,
  first_event_name text,
  first_since timestamptz,
  first_until timestamptz,
  second_instance_id bigint,
  second_event_name text,
  second_since timestamptz,
  second_until timestamptz,
  overlap_range tstzrange
);

comment on type event_conflict is E'
@foreignKey (person_id) references person (id)
@foreignKey (first_instance_id) references event_instance (id)
@foreignKey (second_instance_id) references event_instance (id)
';
comment on column event_conflict.person_id is '@notNull';
comment on column event_conflict.first_instance_id is '@notNull';
comment on column event_conflict.second_instance_id is '@notNull';
comment on column event_conflict.overlap_range is '@notNull';

create or replace function event_overlaps_attendee_report(p_since timestamptz, p_until timestamptz)
  returns setof event_conflict
  language sql stable
as $$
  with instances as (
    select
      ea.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from event_instance_registration ea
    join event_instance ei on ei.id = ea.instance_id
    join person p on p.id = ea.person_id
    where not ei.is_cancelled
      and ea.person_id is not null
      and ea.registration_status = 'active'
      and ei.range && tstzrange(coalesce(p_since, '-infinity'::timestamptz), coalesce(p_until, 'infinity'::timestamptz), '[]')
  )
  select
    i1.person_id,
    i1.person_name,
    i1.instance_id as first_instance_id,
    i1.event_name as first_event_name,
    i1.since as first_since,
    i1.until as first_until,
    i2.instance_id as second_instance_id,
    i2.event_name as second_event_name,
    i2.since as second_since,
    i2.until as second_until,
    tstzrange(greatest(i1.since, i2.since), least(i1.until, i2.until), '[]') as overlap_range
  from instances i1
  join instances i2 on i1.person_id = i2.person_id
    and i1.instance_id < i2.instance_id
    and i1.range && i2.range
    and greatest(i1.since, i2.since) < least(i1.until, i2.until);
$$;

comment on function event_overlaps_attendee_report is '@simpleCollections only';
grant all on function event_overlaps_attendee_report to anonymous;

create or replace function event_overlaps_trainer_report(p_since timestamptz, p_until timestamptz)
  returns setof event_conflict
  language sql stable
as $$
  with trainer_instances as (
    select
      eit.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from event_instance ei
    join event_instance_trainer eit on ei.id = eit.instance_id
    join person p on p.id = eit.person_id
    where not ei.is_cancelled
      and ei.range && tstzrange(coalesce(p_since, '-infinity'::timestamptz), coalesce(p_until, 'infinity'::timestamptz), '[]')
  )
  select
    ti1.person_id,
    ti1.person_name,
    ti1.instance_id as first_instance_id,
    ti1.event_name as first_event_name,
    ti1.since as first_since,
    ti1.until as first_until,
    ti2.instance_id as second_instance_id,
    ti2.event_name as second_event_name,
    ti2.since as second_since,
    ti2.until as second_until,
    tstzrange(greatest(ti1.since, ti2.since), least(ti1.until, ti2.until), '[]') as overlap_range
  from trainer_instances ti1
  join trainer_instances ti2 on ti1.person_id = ti2.person_id
    and ti1.instance_id < ti2.instance_id
    and ti1.range && ti2.range
    and greatest(ti1.since, ti2.since) < least(ti1.until, ti2.until);
$$;

comment on function event_overlaps_trainer_report is '@simpleCollections only';
grant all on function event_overlaps_trainer_report to anonymous;
