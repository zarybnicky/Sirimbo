--! Previous: sha1:c334ad738be675ccb5a665326dee02ca41d3a138
--! Hash: sha1:e4a023b172235a1d2056bff61769c77eed5f0422

--! split: 1-current.sql
CREATE INDEX IF NOT EXISTS json_response_frontier_fetched_desc_idx
  ON crawler.json_response (frontier_id, fetched_at DESC);

CREATE INDEX IF NOT EXISTS html_response_frontier_fetched_desc_idx
  ON crawler.html_response (frontier_id, fetched_at DESC);

CREATE INDEX IF NOT EXISTS frontier_process_pending_ok_gone_pick_idx
  ON crawler.frontier (last_fetched_at, discovered_at, id)
  WHERE process_status = 'pending'
    AND fetch_status IN ('ok','gone');

alter table user_proxy alter column active_range set expression as (tstzrange(since, until, '[)'::text));
alter table tenant_administrator alter column active_range set expression as (tstzrange(since, until, '[)'::text));
alter table tenant_trainer alter column active_range set expression as (tstzrange(since, until, '[)'::text));
alter table tenant_membership alter column active_range set expression as (tstzrange(since, until, '[)'::text));
alter table cohort_membership alter column active_range set expression as (tstzrange(since, until, '[)'::text));
alter table couple alter column active_range set expression as (tstzrange(since, until, '[)'::text));
alter table accounting_period alter column range set expression as (tstzrange(since, until, '[)'::text));

alter table public.user_proxy
  drop constraint if exists user_proxy_no_overlap,
  add constraint user_proxy_no_overlap exclude using gist (user_id with =, person_id with =, active_range with &&);

alter table public.tenant_administrator
  drop constraint if exists tenant_administrator_no_overlap,
  add constraint tenant_administrator_no_overlap exclude using gist (tenant_id with =, person_id with =, active_range with &&);

alter table public.tenant_trainer
  drop constraint if exists tenant_trainer_no_overlap,
  add constraint tenant_trainer_no_overlap exclude using gist (tenant_id with =, person_id with =, active_range with &&);

alter table public.tenant_membership
  drop constraint if exists tenant_membership_no_overlap,
  add constraint tenant_membership_no_overlap exclude using gist (tenant_id with =, person_id with =, active_range with &&);

alter table public.cohort_membership
  drop constraint if exists cohort_membership_no_overlap,
  add constraint cohort_membership_no_overlap exclude using gist (cohort_id with =, person_id with =, active_range with &&);

alter table public.couple
  drop constraint if exists couple_no_overlap,
  add constraint couple_no_overlap exclude using gist (man_id with =, woman_id with =, active_range with &&);

alter table public.accounting_period
  drop constraint if exists accounting_period_no_overlap,
  add constraint accounting_period_no_overlap exclude using gist (tenant_id with =, range with &&);

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
    select
      cm.person_id,
      cm.cohort_id
    from cohort_membership cm
    where cm.tenant_id = current_tenant_id()
      and cm.status = 'active'
      and (cm.cohort_id = scoreboard_entries.cohort_id or scoreboard_entries.cohort_id is null)
  ),
  attendance as (
    select
      ea.person_id,
      case when scoreboard_entries.cohort_id is null then null else matched_membership.cohort_id end as cohort_id,
      case when e.type = 'lesson' then 1 else 0 end as lesson_score,
      case when e.type = 'group' then floor((extract(epoch from (i.until - i.since)) / 60)::numeric / 45::numeric) else 0 end as group_score,
      case when e.type = 'camp' then 3 + 2 * ((extract(epoch from (i.until - i.since)) > 86400)::int) else 0 end as event_score,
      date_trunc('day', i.since)::date as day
    from event_attendance ea
    join event_registration er on er.id = ea.registration_id
    join event e on e.id = er.event_id
    join event_instance i on i.id = ea.instance_id
    left join event_target_cohort tc on tc.id = er.target_cohort_id
    join lateral (
      select m.cohort_id
      from membership m
      where m.person_id = ea.person_id
        and (scoreboard_entries.cohort_id is null or m.cohort_id = scoreboard_entries.cohort_id)
        and (tc.cohort_id is null or tc.cohort_id = m.cohort_id)
      limit 1
    ) matched_membership on true
    where (ea.status = 'attended' or e.type = 'lesson')
      and e.type <> 'reservation'
      and not i.is_cancelled
      and i.since >= scoreboard_entries.since::timestamptz
      and i.until < scoreboard_entries.until::timestamptz
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
        when scoreboard_entries.cohort_id is null then null
        else coalesce(sma.cohort_id, scoreboard_entries.cohort_id)
      end as cohort_id,
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

CREATE or replace FUNCTION person_active_couples(p person) RETURNS SETOF couple as $$
  select *
  from couple
  where (man_id = p.id or woman_id = p.id) and status = 'active'
  order by active_range;
$$ language sql stable;


CREATE or replace FUNCTION person_all_couples(p person) RETURNS SETOF couple as $$
  select *
  from couple
  where (man_id = p.id or woman_id = p.id)
  order by active_range;
$$ language sql stable;

grant all on function person_active_couples to anonymous;
grant all on function person_all_couples to anonymous;

create or replace function tenant_couples(t tenant) returns setof couple as $$
  select distinct couple.*
  from couple
  join tenant_membership on man_id = person_id or woman_id = person_id
  where couple.status = 'active' and tenant_membership.status = 'active' and tenant_id = t.id
  order by couple.active_range asc;
$$ language sql stable;
grant all on function tenant_couples to anonymous;
comment on function tenant_couples is '@simpleCollections only';

create or replace function public.system_admin_tenants()
returns table (
  id bigint,
  name text,
  description text,
  bank_account text,
  origins text[],
  cz_ico text,
  cz_dic text,
  address public.address_domain,
  membership_count bigint,
  trainer_count bigint,
  administrator_count bigint,
  session_count_last_30_days bigint,
  session_count_per_trainer_last_30_days double precision
)
language plpgsql
stable
security definer
set search_path = public, pg_temp
as $$
begin
  if not app_private.is_system_admin(current_user_id()) then
    raise exception 'permission denied for system admin tenant overview'
      using errcode = '42501';
  end if;

  return query
  select
    t.id,
    t.name,
    t.description,
    t.bank_account,
    t.origins,
    t.cz_ico,
    t.cz_dic,
    t.address,
    membership_counts.membership_count,
    staffing.trainer_count,
    administrators.administrator_count,
    load.session_count_last_30_days,
    load.session_count_per_trainer_last_30_days
  from public.tenant t
  cross join lateral (
    select
      count(*) filter (where tm.status = 'active') as membership_count
    from public.tenant_membership tm
    where tm.tenant_id = t.id
  ) as membership_counts
  cross join lateral (
    select count(*) filter (where tt.status = 'active') as trainer_count
    from public.tenant_trainer tt
    where tt.tenant_id = t.id
  ) as staffing
  cross join lateral (
    select count(*) filter (where ta.status = 'active') as administrator_count
    from public.tenant_administrator ta
    where ta.tenant_id = t.id
  ) as administrators
  cross join lateral (
    select
      count(*) as session_count_last_30_days,
      case
        when coalesce(staffing.trainer_count, 0) > 0 then count(*)::double precision / staffing.trainer_count::double precision
        else 0::double precision
      end as session_count_per_trainer_last_30_days
    from public.event_instance ei
    where ei.tenant_id = t.id
      and coalesce(ei.is_cancelled, false) = false
      and ei.since >= now() - interval '30 days'
  ) as load
  order by t.name;
end;
$$;

comment on function public.system_admin_tenants() is 'Lists tenants with aggregate membership, staffing, and recent session statistics for system administrators.';

grant execute on function public.system_admin_tenants() to anonymous;

select verify_function('public.system_admin_tenants');


alter table user_proxy drop column if exists active;
alter table tenant_administrator drop column if exists active;
alter table tenant_trainer drop column if exists active;
alter table tenant_membership drop column if exists active;
alter table cohort_membership drop column if exists active;
alter table couple drop column if exists active;
