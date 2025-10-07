drop view if exists scoreboard;
drop function if exists scoreboard_entries;
create or replace function scoreboard_entries(
  cohort_id bigint default null,
  since date default null,
  until date default null
)
returns setof scoreboard_record
language sql
stable
as $$
  with params as (
    select
      cohort_id as cohort_id,
      coalesce(
        since,
        make_date(
          (date_part('year', now())::int - case when date_part('month', now())::int < 9 then 1 else 0 end),
          9,
          1
        )
      ) as since,
      coalesce(until, (date_trunc('day', now()) + interval '1 day')::date) as until
  ),
  membership as (
    select
      cm.person_id,
      cm.cohort_id
    from cohort_membership cm
    join params p on true
    where cm.tenant_id = current_tenant_id()
      and cm.active
      and (p.cohort_id is null or cm.cohort_id = p.cohort_id)
  ),
  attendance as (
    select
      ea.person_id,
      case when p.cohort_id is null then null else matched_membership.cohort_id end as cohort_id,
      case when e.type = 'lesson' then 1 else 0 end as lesson_score,
      case when e.type = 'group' then floor((extract(epoch from (i.until - i.since)) / 60)::numeric / 45::numeric) else 0 end as group_score,
      case when e.type = 'camp' then 3 + 2 * ((extract(epoch from (i.until - i.since)) > 86400)::int) else 0 end as event_score,
      date_trunc('day', i.since)::date as day
    from event_attendance ea
    join event_registration er on er.id = ea.registration_id
    join event e on e.id = er.event_id
    join event_instance i on i.id = ea.instance_id
    join params p on true
    left join event_target_cohort tc on tc.id = er.target_cohort_id
    join lateral (
      select m.cohort_id
      from membership m
      where m.person_id = ea.person_id
        and (p.cohort_id is null or m.cohort_id = p.cohort_id)
        and (tc.cohort_id is null or tc.cohort_id = m.cohort_id)
      limit 1
    ) matched_membership on true
    where (ea.status = 'attended' or e.type = 'lesson')
      and e.type <> 'reservation'
      and not i.is_cancelled
      and i.since >= p.since::timestamptz
      and i.until < p.until::timestamptz
  ),
  per_day as (
    select
      person_id,
      cohort_id,
      least(sum(lesson_score), 4) as lesson_score,
      sum(group_score) as group_score,
      sum(event_score) as event_score
    from attendance
    group by person_id, cohort_id, day
  ),
  aggregated as (
    select
      person_id,
      cohort_id,
      coalesce(sum(lesson_score), 0)::bigint as lesson_total_score,
      coalesce(sum(group_score), 0)::bigint as group_total_score,
      coalesce(sum(event_score), 0)::bigint as event_total_score
    from per_day
    group by person_id, cohort_id
  ),
  manual as (
    select
      sma.person_id,
      case
        when p.cohort_id is null then null
        else coalesce(sma.cohort_id, p.cohort_id)
      end as cohort_id,
      sum(sma.points)::bigint as manual_total_score
    from scoreboard_manual_adjustment sma
    join params p on true
    where sma.tenant_id = current_tenant_id()
      and sma.awarded_at >= p.since
      and sma.awarded_at < p.until
      and (
        p.cohort_id is null or
        sma.cohort_id is null or
        sma.cohort_id = p.cohort_id
      )
      and exists (
        select 1
        from membership mem
        where mem.person_id = sma.person_id
          and (
            (p.cohort_id is null and sma.cohort_id is null)
            or mem.cohort_id is not distinct from coalesce(sma.cohort_id, p.cohort_id)
          )
      )
    group by 1, 2
  ),
  totals as (
    select
      coalesce(a.person_id, m.person_id) as person_id,
      coalesce(a.cohort_id, m.cohort_id) as cohort_id,
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
    full join manual m using (person_id, cohort_id)
  )
  select
    person_id,
    cohort_id,
    lesson_total_score,
    group_total_score,
    event_total_score,
    manual_total_score,
    total_score,
    rank() over (order by total_score desc, person_id) as ranking
  from totals
  order by total_score desc, person_id;
$$;

comment on function scoreboard_entries is '@simpleCollections only';

grant all on function scoreboard_entries to anonymous;

drop view if exists scoreboard;
create or replace view scoreboard as
select
  person_id,
  cohort_id,
  lesson_total_score,
  group_total_score,
  event_total_score,
  manual_total_score,
  total_score,
  ranking
from scoreboard_entries();

comment on view scoreboard is '@foreignKey (person_id) references person (id)
@foreignKey (cohort_id) references cohort (id)
@simpleCollections only';

grant all on table scoreboard to anonymous;
