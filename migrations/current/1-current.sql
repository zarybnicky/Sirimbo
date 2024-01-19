-- Write your migration here

alter table event_instance add column if not exists is_cancelled boolean default false;

COMMENT ON TABLE public.event_instance IS E'@omit create,delete
@simpleCollections both';

create or replace view scoreboard as
  with members as (
    select person.id
    from person
    inner join cohort_membership on cohort_membership.person_id=person.id
    where now() <@ cohort_membership.active_range and tenant_id=current_tenant_id()
  ), attendances as (
    select
      event_attendance.person_id,
      case when event.type = 'lesson' then 1 else 0 end as lesson_score,
      case when event.type = 'group' then floor(extract(epoch from i.until - i.since) / 60 / 45) else 0 end as group_score,
      case when event.type = 'camp' then 3 + 2 * (extract(epoch from i.until - i.since) > 86400)::int else 0 end as event_score,
      i.since
    from event_attendance
    inner join event_registration on event_registration.id=event_attendance.registration_id
    inner join event on event.id=event_registration.event_id
    inner join event_instance i on event_attendance.instance_id=i.id
    where (event_attendance.status = 'attended' or event.type = 'lesson')
    and event.type <> 'reservation'
    and not i.is_cancelled
    and i.since > '2023-09-01T00:00:00.0000Z'
    and i.until < date_trunc('day', now())
    and event_attendance.person_id in (select id from members)
  ), per_day as (
    select
      person_id,
      least(SUM(lesson_score), 4) AS lesson_score,
      SUM(group_score) AS group_score,
      SUM(event_score) AS event_score,
      least(SUM(lesson_score), 4) + sum(group_score) + sum(event_score) AS total_score,
      since
    from attendances
    group by person_id, since
  )
  select
    person_id,
    SUM(lesson_score)::bigint AS lesson_total_score,
    SUM(group_score)::bigint AS group_total_score,
    SUM(event_score)::bigint AS event_total_score,
    SUM(lesson_score + group_score + event_score)::bigint AS total_score,
    rank() OVER (ORDER BY SUM(lesson_score + group_score) DESC) AS ranking
  from per_day
  group by person_id
  ORDER BY total_score DESC;
comment on view scoreboard is E'@foreignKey (person_id) references person (id)
@simpleCollections only';
grant all on scoreboard to anonymous;


select app_private.drop_policies('public.tenant_location');
CREATE POLICY admin_all ON public.tenant_location TO administrator USING (true);
CREATE POLICY my_tenant ON public.tenant_location AS RESTRICTIVE USING (tenant_id = public.current_tenant_id());
CREATE POLICY public_view ON public.tenant_location FOR SELECT TO anonymous using (true);

create or replace function person_weekly_attendance(p person) returns table (
  week date,
  event_count int
) language sql as $$
  select date_trunc('week', since) as week, count(*) as count
  from event_attendance
  join event_instance on instance_id=event_instance.id
  where person_id = p.id
  group by date_trunc('week', since)
$$ stable;
grant all on function person_weekly_attendance to anonymous;
comment on function person_weekly_attendance is '@simpleCollections only';
