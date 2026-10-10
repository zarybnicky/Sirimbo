\set ON_ERROR_STOP on
\if :{?apply}
\else
\set apply false
\endif

begin isolation level repeatable read;
\if :apply
set local lock_timeout = '5s';
lock table crawler.frontier, crawler.json_response in share row exclusive mode;
\endif

create temp table response_runs on commit drop as
with previous as (
  select r.*,
    row(url, http_status, error, content_hash) is distinct from
      lag(row(url, http_status, error, content_hash)) over (
        partition by frontier_id order by fetched_at, id
      ) as changed
  from crawler.json_response r
), numbered as (
  select *, sum(changed::int) over (
    partition by frontier_id order by fetched_at, id
  ) as run
  from previous
), runs as (
  select frontier_id, run,
    array_agg(id order by fetched_at desc, id desc) as ids,
    min(fetched_at) as fetched_at,
    max(coalesce(last_observed_at, fetched_at)) as last_observed_at,
    sum(observation_count)::bigint as observation_count
  from numbered
  group by frontier_id, run
)
select *, tstzrange(fetched_at,
  lead(fetched_at) over (partition by frontier_id order by run), '[)') as sys_period
from runs;

-- Keep the newest row, including any already-open period at the legacy boundary.
create temp table response_merges on commit drop as
select id, ids[1] as retained_id
from response_runs cross join lateral unnest(ids) as member(id)
where id <> ids[1];
create unique index on response_merges (id);
analyze response_merges;

select (select count(*) from response_merges) as redundant_rows,
  count(*) as retained_rows, sum(observation_count) as observations
from response_runs;

-- Equal change timestamps or overlapping observations cannot form valid periods.
do $$
begin
  if exists (select 1 from response_runs where not (sys_period @> last_observed_at)) then
    raise exception 'Response observations overlap the next response period';
  end if;
  if exists (
    select 1 from response_runs r
    join crawler.frontier f on f.id = r.frontier_id
    where upper_inf(r.sys_period) and f.last_response_id is distinct from r.ids[1]
  ) then
    raise exception 'A current response pointer does not match its latest observation';
  end if;
end $$;

\if :apply
update crawler.frontier f
set last_successful_response_id = m.retained_id
from response_merges m
where f.last_successful_response_id = m.id;

delete from crawler.json_response r using response_merges m where r.id = m.id;

update crawler.json_response r
set fetched_at = run.fetched_at,
    sys_period = run.sys_period,
    last_observed_at = run.last_observed_at,
    observation_count = run.observation_count
from response_runs run
where r.id = run.ids[1]
  and row(r.fetched_at, r.sys_period, r.last_observed_at, r.observation_count)
    is distinct from row(run.fetched_at, run.sys_period, run.last_observed_at, run.observation_count);

commit;
\else
rollback;
\endif
