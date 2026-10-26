drop view if exists scoreboard;
drop function if exists scoreboard_entries;

create or replace function scoreboard_entries(
  since date,
  until date,
  cohort_id bigint default null
)
returns setof scoreboard_record
language sql
stable
as $$
  with membership as (
    select cm.person_id, cm.cohort_id
    from current_cohort_membership cm
    where (cm.cohort_id = scoreboard_entries.cohort_id or scoreboard_entries.cohort_id is null)
  ),
  instances as (
    select
      i.id as event_id,
      i.since,
      i.until,
      i.type as event_type
    from event i
    where not i.is_cancelled
      and i.since >= scoreboard_entries.since::timestamptz
      and i.until  < scoreboard_entries.until::timestamptz
      and i.type <> 'reservation'
  ),
  member_people as (
    select distinct person_id
    from membership
  ),
  attendance as materialized (
    select
      ea.person_id,
      case when inst.event_type = 'lesson' then 1 else 0 end as lesson_score,
      case when inst.event_type = 'group' then floor((extract(epoch from (inst.until - inst.since)) / 60)::numeric / 45::numeric) else 0 end as group_score,
      case when inst.event_type = 'camp'  then 3 + 2 * ((extract(epoch from (inst.until - inst.since)) > 86400)::int) else 0 end as event_score,
      date_trunc('day', inst.since)::date as day
    from instances inst
    cross join lateral (
      select registration.person_id
      from event_registration registration
      where registration.event_id = inst.event_id
        and registration.person_id is not null
        and registration.registration_status = 'active'
        and (registration.status = 'attended' or inst.event_type = 'lesson')
      -- Keep date-filtered events first, avoiding RLS checks on registration history.
      offset 0
    ) ea
    where ea.person_id = any (select person_id from member_people)
  ),
  per_day as (
    select
      person_id,
      least(sum(lesson_score), 4) as lesson_score,
      sum(group_score) as group_score,
      sum(event_score) as event_score
    from attendance
    group by person_id, day
  ),
  aggregated as (
    select
      person_id,
      coalesce(sum(lesson_score), 0)::bigint as lesson_total_score,
      coalesce(sum(group_score), 0)::bigint as group_total_score,
      coalesce(sum(event_score), 0)::bigint as event_total_score
    from per_day
    group by person_id
  ),
  manual as (
    select
      sma.person_id,
      sum(sma.points)::bigint as manual_total_score
    from scoreboard_manual_adjustment sma
    where sma.tenant_id = current_tenant_id()
      and sma.awarded_at >= scoreboard_entries.since
      and sma.awarded_at < scoreboard_entries.until
      and (
        scoreboard_entries.cohort_id is null or
        sma.cohort_id is null or
        sma.cohort_id = scoreboard_entries.cohort_id
      )
      and exists (
        select 1
        from membership mem
        where mem.person_id = sma.person_id
          and (
            (scoreboard_entries.cohort_id is null and sma.cohort_id is null)
            or mem.cohort_id is not distinct from coalesce(sma.cohort_id, scoreboard_entries.cohort_id)
          )
      )
    group by sma.person_id
  ),
  totals as (
    select
      person_id,
      coalesce(a.lesson_total_score, 0)::bigint as lesson_total_score,
      coalesce(a.group_total_score, 0)::bigint as group_total_score,
      coalesce(a.event_total_score, 0)::bigint as event_total_score,
      coalesce(m.manual_total_score, 0)::bigint as manual_total_score,
      (
        coalesce(a.lesson_total_score, 0)::bigint +
        coalesce(a.group_total_score, 0)::bigint +
        coalesce(a.event_total_score, 0)::bigint +
        coalesce(m.manual_total_score, 0)::bigint
      ) as total_score
    from aggregated a
    full join manual m using (person_id)
  )
  select
    person_id,
    scoreboard_entries.cohort_id as cohort_id,
    lesson_total_score,
    group_total_score,
    event_total_score,
    manual_total_score,
    total_score,
    rank() over (order by total_score desc, person_id) as ranking
  from totals
  order by total_score desc, person_id;
$$;

comment on function scoreboard_entries is '@behavior +list -connection';

grant all on function scoreboard_entries to anonymous;
