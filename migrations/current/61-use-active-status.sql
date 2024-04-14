drop function if exists app_private.tg__person_email_primary;
drop function if exists app_private.tg__person_address_primary;
drop function if exists app_private.tg__person_phone_primary;
drop function if exists public.users_in_public_cohort;
drop function if exists users_date_of_oldest_payment;
drop function if exists users_date_of_newest_payment;
drop function if exists on_update_current_timestamp_rozpis;
drop function if exists on_update_current_timestamp_nabidka;
drop function if exists person_couple_ids;

comment on function skupiny_in_current_tenant is E'@filterable
@deprecated';

alter table event_registration
    alter is_confirmed set default false,
    alter confirmed_at set default null;
drop function if exists is_current_tenant_member;

create or replace view scoreboard as
  with members as (
    select person.id
    from person
    inner join cohort_membership on cohort_membership.person_id=person.id
    where cohort_membership.active and tenant_id=current_tenant_id()
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

create or replace function tenant_couples(t tenant) returns setof couple stable
begin atomic
  select distinct couple.*
  from couple
  join tenant_membership on man_id=person_id or woman_id=person_id
  where couple.active and tenant_membership.active and tenant_id=t.id
  order by couple.active_range asc;
end;
grant all on function tenant_couples to anonymous;
comment on function tenant_couples is '@simpleCollections only';

CREATE or replace FUNCTION public.person_active_couples(p public.person) RETURNS SETOF public.couple STABLE
begin atomic
  select * from couple where (man_id = p.id or woman_id = p.id) and active order by active_range;
end;
CREATE or replace FUNCTION public.person_all_couples(p public.person) RETURNS SETOF public.couple STABLE
begin atomic
  select * from couple where (man_id = p.id or woman_id = p.id) order by active_range;
end;
