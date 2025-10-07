-- Add manual scoreboard adjustment table so points can be granted outside the
-- automatic attendance rollups. These adjustments stay tenant-scoped and are
-- read by the scoreboard aggregations below.
create table if not exists scoreboard_manual_adjustment (
  id bigserial primary key,
  tenant_id bigint not null default current_tenant_id(),
  person_id bigint not null references person(id) on update cascade on delete cascade,
  cohort_id bigint references cohort(id),
  points integer not null,
  reason text,
  awarded_at date not null default current_date,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

comment on table scoreboard_manual_adjustment is '@simpleCollections only';

grant all on table scoreboard_manual_adjustment to anonymous;

alter table scoreboard_manual_adjustment enable row level security;

drop policy if exists admin_manage on scoreboard_manual_adjustment;
create policy admin_manage on scoreboard_manual_adjustment
  for all to administrator
  using (tenant_id = current_tenant_id())
  with check (tenant_id = current_tenant_id());

drop policy if exists member_read on scoreboard_manual_adjustment;
create policy member_read on scoreboard_manual_adjustment
  for select to member
  using (tenant_id = current_tenant_id());

drop trigger if exists _100_timestamps on scoreboard_manual_adjustment;
create trigger _100_timestamps
  before insert or update on scoreboard_manual_adjustment
  for each row
  execute function app_private.tg__timestamps();

create index if not exists scoreboard_manual_adjustment_tenant_id_idx
  on scoreboard_manual_adjustment (tenant_id);
create index if not exists scoreboard_manual_adjustment_person_id_idx
  on scoreboard_manual_adjustment (person_id);
create index if not exists scoreboard_manual_adjustment_cohort_id_idx
  on scoreboard_manual_adjustment (cohort_id);

-- Allow manual adjustments to affect the scoreboard by rewriting the
-- aggregation to include them and exposing a parameterised helper.
create or replace function scoreboard_entries(
  cohort_id bigint default null,
  since date default null,
  until date default null
)
returns table (
  person_id bigint,
  cohort_id bigint,
  lesson_total_score bigint,
  group_total_score bigint,
  event_total_score bigint,
  manual_total_score bigint,
  total_score bigint,
  ranking bigint
)
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
      coalesce(until, date_trunc('day', now())::date) as until
  ),
  membership as (
    select
      cm.person_id,
      cm.cohort_id,
      cm.active_range
    from cohort_membership cm
    join params p on true
    where cm.tenant_id = current_tenant_id()
      and cm.active
      and cm.active_range && tstzrange(p.since::timestamptz, (p.until + 1)::timestamptz, '[)')
      and (p.cohort_id is null or cm.cohort_id = p.cohort_id)
  ),
  attendance as (
    select
      ea.person_id,
      mem.cohort_id,
      case when e.type = 'lesson' then 1 else 0 end as lesson_score,
      case when e.type = 'group' then floor((extract(epoch from (i.until - i.since)) / 60)::numeric / 45::numeric) else 0 end as group_score,
      case when e.type = 'camp' then 3 + 2 * ((extract(epoch from (i.until - i.since)) > 86400)::int) else 0 end as event_score,
      date_trunc('day', i.since)::date as day
    from event_attendance ea
    join event_registration er on er.id = ea.registration_id
    join event e on e.id = er.event_id
    join event_instance i on i.id = ea.instance_id
    join params p on true
    join lateral (
      select
        case when p.cohort_id is null then null else m.cohort_id end as cohort_id
      from membership m
      where m.person_id = ea.person_id
        and m.active_range @> i.since
        and (p.cohort_id is null or m.cohort_id = p.cohort_id)
      order by m.cohort_id
      limit 1
    ) mem on true
    left join event_target_cohort tc on tc.id = er.target_cohort_id
    where (ea.status = 'attended' or e.type = 'lesson')
      and e.type <> 'reservation'
      and not i.is_cancelled
      and i.since >= p.since::timestamptz
      and i.until < (p.until + 1)::timestamptz
      and (p.cohort_id is null or tc.cohort_id is null or tc.cohort_id = mem.cohort_id)
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
  manual_base as (
    select
      sma.person_id,
      case
        when p.cohort_id is null then null
        else coalesce(sma.cohort_id, p.cohort_id)
      end as cohort_id,
      sma.points
    from scoreboard_manual_adjustment sma
    join params p on true
    where sma.tenant_id = current_tenant_id()
      and sma.awarded_at >= p.since
      and sma.awarded_at < (p.until + 1)
      and (
        p.cohort_id is null or
        sma.cohort_id is null or
        sma.cohort_id = p.cohort_id
      )
      and exists (
        select 1
        from membership m
        where m.person_id = sma.person_id
          and (
            (p.cohort_id is null and sma.cohort_id is null)
            or m.cohort_id is not distinct from coalesce(sma.cohort_id, p.cohort_id)
          )
      )
  ),
  manual as (
    select
      person_id,
      cohort_id,
      sum(points)::bigint as manual_total_score
    from manual_base
    group by person_id, cohort_id
  ),
  totals as (
    select
      person_id,
      cohort_id,
      lesson_total_score,
      group_total_score,
      event_total_score,
      0::bigint as manual_total_score
    from aggregated
    union all
    select
      person_id,
      cohort_id,
      0::bigint,
      0::bigint,
      0::bigint,
      manual_total_score
    from manual
  ),
  rolled as (
    select
      person_id,
      cohort_id,
      sum(lesson_total_score)::bigint as lesson_total_score,
      sum(group_total_score)::bigint as group_total_score,
      sum(event_total_score)::bigint as event_total_score,
      sum(manual_total_score)::bigint as manual_total_score
    from totals
    group by person_id, cohort_id
  )
  select
    person_id,
    cohort_id,
    lesson_total_score,
    group_total_score,
    event_total_score,
    manual_total_score,
    (lesson_total_score + group_total_score + event_total_score + manual_total_score) as total_score,
    rank() over (order by (lesson_total_score + group_total_score + event_total_score + manual_total_score) desc, person_id) as ranking
  from rolled
  order by total_score desc, person_id;
$$;

comment on function scoreboard_entries(bigint, date, date) is '@simpleCollections only
@foreignKey (person_id) references person (id)
@foreignKey (cohort_id) references cohort (id)';

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
