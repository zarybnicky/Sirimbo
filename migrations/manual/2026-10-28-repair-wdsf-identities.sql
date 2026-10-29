-- Apply the migration and deploy the worker first. Pause crawler workers while this script runs.
-- Run with psql -X -v ON_ERROR_STOP=1 -f this-file.sql DATABASE.
-- Cached responses establish the mappings. Each competition commits separately.
-- The script can resume after an interruption. Conflicting source scores stop it.
\set ON_ERROR_STOP on
\pset pager off
set lock_timeout = '5s';
set statement_timeout = '120s';
set work_mem = '64MB';
set jit = off;

do $$ begin
  if to_regprocedure('app_private.refresh_competition_round_results(bigint)') is null then
    raise exception 'Apply the migration containing refresh_competition_round_results before this repair.';
  end if;
end $$;

-- These are session-local helpers and tables, not application schema changes.
create function pg_temp.wdsf_identity(p jsonb)
returns table(source_id text, competitor_id text, competitor_type federated.competitor_type, name text)
language sql immutable as $$
  select source_id,
    case when source_id = 'person-0' then 'participant-' || (p->>'id') else source_id end,
    case kind when 'couple' then 'couple' when 'team' then 'team' else 'solo' end::federated.competitor_type,
    coalesce(nullif(p->>'Team', ''), nullif(p->>'PerformanceName', ''), p->>'name', '')
  from (
    select case
      when rel like '%/couple' then substring(href from '/couple/((?:rls|wdsf)-[0-9]+)$')
      when rel like '%/team' then 'team-' || substring(href from '/team/([0-9]+)$')
      else 'person-' || substring(href from '/person/([0-9]+)$') end source_id,
      case when rel like '%/couple' then 'couple' when rel like '%/team' then 'team' else 'solo' end kind
    from jsonb_to_recordset(p->'link') as l(rel text, href text)
    where rel in ('http://services.worlddancesport.org/rel/participant/couple',
      'http://services.worlddancesport.org/rel/participant/team',
      'http://services.worlddancesport.org/rel/participant/athlete')
    order by case when rel like '%/couple' then 1 when rel like '%/team' then 2 else 3 end
    limit 1
  ) s where source_id is not null
$$;

create temp table participant_source as
select f.id frontier_id, f.key participant_id, c.id competition_id,
  cache.content #>> '{rounds,-1,dances,-1,name}' last_dance,
  i.*
from crawler.frontier f
join crawler.json_response response on response.id = f.last_successful_response_id
join crawler.json_response_cache cache using (content_hash)
join federated.competition c on c.federation = 'wdsf' and c.external_id = cache.content->>'competitionId'
cross join lateral pg_temp.wdsf_identity(cache.content) i
where f.federation = 'wdsf' and f.kind = 'participant';
create unique index on participant_source (participant_id);
create index on participant_source (competition_id, competitor_id);
analyze participant_source;

create temp table index_source as
select f.id frontier_id, c.id competition_id, p->>'id' participant_id,
  i.source_id, coalesce(detail.competitor_id, i.competitor_id) competitor_id,
  coalesce(detail.competitor_type, i.competitor_type) competitor_type,
  coalesce(nullif(detail.name, ''), i.name) name
from crawler.frontier f
join crawler.json_response response on response.id = f.last_successful_response_id
join crawler.json_response_cache cache using (content_hash)
join federated.competition c on c.federation = 'wdsf' and c.external_id = f.key
cross join lateral jsonb_array_elements(cache.content) p
cross join lateral pg_temp.wdsf_identity(p) i
left join participant_source detail on detail.participant_id = p->>'id' and detail.competition_id = c.id
where f.federation = 'wdsf' and f.kind = 'participantIndex';

create temp table identity_map as
select distinct competition_id, old_id, competitor_id new_id, competitor_type, name
from (
  select competition_id, source_id, competitor_id, competitor_type, name from participant_source
  union all
  select competition_id, source_id, competitor_id, competitor_type, name from index_source
) source
cross join lateral (values (source_id), (regexp_replace(source_id, '^(rls-|wdsf-|team-|person-)', ''))) old(old_id)
where old_id <> competitor_id and source_id <> 'person-0';

do $$ begin
  if exists (select 1 from identity_map group by competition_id, old_id having count(distinct new_id) > 1) then
    raise exception 'Ambiguous competitor mapping. No competition data was changed.';
  end if;
end $$;
-- Names can vary between responses for one source ID; they do not define identity.
delete from identity_map a using identity_map b
where a.ctid > b.ctid and (a.competition_id, a.old_id) = (b.competition_id, b.old_id);
create unique index on identity_map (competition_id, old_id);
create index on identity_map (new_id);
analyze identity_map;

-- A participant can have a corrected MIN in newer responses. Recover its old
-- identifier from that competition's index history, using the same participant ID.
create temp table unmapped_reference as
with refs as (
  select competition_id, substring(competitor_id from 6) old_id from federated.competition_result
  where competitor_id ~ '^wdsf:[1-9][0-9]*$'
  union
  select competition_id, substring(competitor_id from 6) old_id from federated.competition_entry
  where competitor_id ~ '^wdsf:[1-9][0-9]*$'
)
select * from refs r where not exists (
  select 1 from identity_map m where (m.competition_id, m.old_id) = (r.competition_id, r.old_id)
);
do $$ begin
  if exists (select 1 from unmapped_reference) then
    create temp table historical_map as
    select distinct u.competition_id, old.old_id, current.competitor_id new_id,
      current.competitor_type, current.name
    from unmapped_reference u
    join federated.competition c on c.id = u.competition_id and c.federation = 'wdsf'
    join crawler.frontier idx on idx.federation = 'wdsf' and idx.kind = 'participantIndex' and idx.key = c.external_id
    join crawler.json_response response on response.frontier_id = idx.id and response.http_status = 200 and response.error is null
    join crawler.json_response_cache cache using (content_hash)
    cross join lateral jsonb_array_elements(case when jsonb_typeof(cache.content) = 'array'
      then cache.content else '[]'::jsonb end) p
    join participant_source current on current.competition_id = u.competition_id
      and current.participant_id = p->>'id' and current.source_id <> 'person-0'
    cross join lateral pg_temp.wdsf_identity(p) previous
    cross join lateral (values (previous.source_id),
      (regexp_replace(previous.source_id, '^(rls-|wdsf-|team-|person-)', ''))) old(old_id)
    where regexp_replace(previous.source_id, '^(rls-|wdsf-|team-|person-)', '') = u.old_id
      and old.old_id <> current.competitor_id;
    if exists (select 1 from historical_map group by competition_id, old_id having count(distinct new_id) > 1) then
      raise exception 'Ambiguous historical WDSF identity. No competition data was changed.';
    end if;
    insert into identity_map select distinct on (competition_id, old_id) * from historical_map
    order by competition_id, old_id, name on conflict do nothing;
  end if;
  if exists (select 1 from unmapped_reference u where not exists (
    select 1 from identity_map m where (m.competition_id, m.old_id) = (u.competition_id, u.old_id)
  )) then raise exception 'Legacy references lack a unique cached source identity. No competition data was changed.'; end if;
end $$;

create temp table zero_person_competition as
select competition_id from federated.competition_result where competitor_id in ('wdsf:0', 'wdsf:person-0')
union select competition_id from federated.competition_entry where competitor_id in ('wdsf:0', 'wdsf:person-0');

create temp table affected_competition as
select distinct c.id competition_id
from federated.competition c
where c.federation = 'wdsf' and (
  exists (select 1 from federated.competition_result r join identity_map m
    on m.competition_id = r.competition_id and r.competitor_id = 'wdsf:' || m.old_id
    where r.competition_id = c.id)
  or exists (select 1 from federated.competition_entry e join identity_map m
    on m.competition_id = e.competition_id and e.competitor_id = 'wdsf:' || m.old_id
    where e.competition_id = c.id)
  or exists (select 1 from participant_source p where p.competition_id = c.id and p.source_id = 'person-0')
  or c.id in (select competition_id from zero_person_competition)
);

-- Only canonical identities supported by cached WDSF links are created.
insert into federated.competitor (federation, external_id, competitor_type, name)
select distinct on (m.new_id) 'wdsf', m.new_id, m.competitor_type, m.name
from identity_map m join affected_competition a using (competition_id)
order by m.new_id, m.name desc
on conflict (id) do nothing;
insert into federated.person (federation, external_id, canonical_name, gender)
select distinct on (m.new_id) 'wdsf', substring(m.new_id from '^person-([0-9]+)$')::bigint, m.name, 'unknown'
from identity_map m join affected_competition a using (competition_id)
where m.competitor_type = 'solo' and m.new_id ~ '^person-[1-9][0-9]*$'
order by m.new_id, m.name desc
on conflict (id) do nothing;
insert into federated.competitor_component (competitor_id, person_id, role)
select distinct 'wdsf:' || m.new_id, 'wdsf:' || substring(m.new_id from '^person-([0-9]+)$'), 'member'::federated.competitor_role
from identity_map m join affected_competition a using (competition_id)
where m.competitor_type = 'solo' and m.new_id ~ '^person-[1-9][0-9]*$'
on conflict do nothing;

create function pg_temp.repair_wdsf_competition(comp_id bigint) returns void
language plpgsql as $$
begin
  perform 1 from federated.competition where id = comp_id and federation = 'wdsf' for no key update;
  if not found then raise exception 'Not a WDSF competition: %', comp_id; end if;

  -- Source scores are never summed or arbitrarily selected to resolve duplicates.
  if exists (
    select 1 from federated.judge_score s
    join federated.competition_round r on r.id = s.round_id
    left join identity_map m on m.competition_id = comp_id and m.old_id = s.competitor_id
    where r.competition_id = comp_id
    group by s.round_id, s.dance_order, s.judge_person_id, coalesce(m.new_id, s.competitor_id), s.component
    having count(distinct (s.dance_code, s.score, s.raw_score)) > 1
  ) then raise exception 'Conflicting scores in competition %. Replay its source participants before repair.', comp_id; end if;
  if exists (
    with mapped as (
      select coalesce('wdsf:' || m.new_id, r.competitor_id) target_id, to_jsonb(r) - 'competitor_id' value
      from federated.competition_result r
      left join identity_map m on m.competition_id = comp_id and r.competitor_id = 'wdsf:' || m.old_id
      where r.competition_id = comp_id
    )
    select 1 from mapped group by target_id
    having count(distinct (value - 'last_dance')) > 1 or (
      count(distinct value) > 1 and not exists (
        -- Older rows can carry a different last array item. Keep the canonical
        -- value only when the current cached participant responses agree with it.
        select 1 from participant_source p
        join federated.competition_result canonical
          on canonical.competition_id = p.competition_id and canonical.competitor_id = 'wdsf:' || p.competitor_id
        where p.competition_id = comp_id and canonical.competitor_id = mapped.target_id
          and p.last_dance is not distinct from canonical.last_dance
          and not exists (select 1 from participant_source other
            where other.competition_id = p.competition_id and other.competitor_id = p.competitor_id
              and other.last_dance is distinct from p.last_dance)
      )
    )
  ) then raise exception 'Conflicting results in competition %.', comp_id; end if;
  if exists (
    select 1 from federated.competition_entry e
    left join identity_map m on m.competition_id = comp_id and e.competitor_id = 'wdsf:' || m.old_id
    where e.competition_id = comp_id
    group by coalesce('wdsf:' || m.new_id, e.competitor_id)
    having count(distinct e.cancelled) > 1
  ) then raise exception 'Conflicting registrations in competition %.', comp_id; end if;

  -- Remove verified duplicates, then change only the IDs of remaining rows.
  delete from federated.judge_score s using federated.competition_round r, identity_map m
  where r.id = s.round_id and r.competition_id = comp_id and m.competition_id = comp_id and s.competitor_id = m.old_id
    and exists (select 1 from federated.judge_score canonical
      where (canonical.round_id, canonical.dance_order, canonical.judge_person_id, canonical.competitor_id, canonical.component)
        = (s.round_id, s.dance_order, s.judge_person_id, m.new_id, s.component));
  update federated.judge_score s set competitor_id = m.new_id
  from federated.competition_round r, identity_map m
  where r.id = s.round_id and r.competition_id = comp_id and m.competition_id = comp_id and s.competitor_id = m.old_id;

  delete from federated.competition_result r using identity_map m
  where r.competition_id = comp_id and m.competition_id = comp_id and r.competitor_id = 'wdsf:' || m.old_id
    and exists (select 1 from federated.competition_result canonical
      where canonical.competition_id = r.competition_id and canonical.competitor_id = 'wdsf:' || m.new_id);
  update federated.competition_result r set competitor_id = 'wdsf:' || m.new_id
  from identity_map m
  where r.competition_id = comp_id and m.competition_id = comp_id and r.competitor_id = 'wdsf:' || m.old_id;
  delete from federated.competition_entry e using identity_map m
  where e.competition_id = comp_id and m.competition_id = comp_id and e.competitor_id = 'wdsf:' || m.old_id
    and exists (select 1 from federated.competition_entry canonical
      where canonical.competition_id = e.competition_id and canonical.competitor_id = 'wdsf:' || m.new_id);
  update federated.competition_entry e set competitor_id = 'wdsf:' || m.new_id
  from identity_map m
  where e.competition_id = comp_id and m.competition_id = comp_id and e.competitor_id = 'wdsf:' || m.old_id;

  -- Recalculate from stored scores without replaying participant responses.
  perform app_private.refresh_wdsf_round_dances(comp_id);
  perform app_private.refresh_competition_round_results(comp_id);
end $$;

\o /dev/null
select format('select pg_temp.repair_wdsf_competition(%s);', competition_id)
from affected_competition order by competition_id
\gexec
\o

begin;
-- Zero is WDSF's missing-person placeholder, not a person or competitor identity.
delete from federated.judge_score s using federated.competition_round r, zero_person_competition z
where s.round_id = r.id and r.competition_id = z.competition_id and s.competitor_id in ('0', 'person-0');
delete from federated.competition_round_result where competitor_id in ('wdsf:0', 'wdsf:person-0');
delete from federated.competition_result where competitor_id in ('wdsf:0', 'wdsf:person-0');
delete from federated.competition_entry where competitor_id in ('wdsf:0', 'wdsf:person-0');
delete from federated.competition_official where person_id = 'wdsf:0';
select app_private.refresh_wdsf_round_dances(z.competition_id) from zero_person_competition z;
select app_private.refresh_competition_round_results(z.competition_id) from zero_person_competition z;

-- Only missing-person entries and dance-name corrections need response replay.
update crawler.frontier f set process_status = 'pending', last_process_error = null, last_process_error_at = null
where f.id in (
  select p.frontier_id from participant_source p
  join federated.competition c on c.id = p.competition_id
  join federated.category cat on cat.id = c.category_id
  where p.source_id = 'person-0' or p.competition_id in (select competition_id from zero_person_competition)
    or (cat.discipline in
    ('SMOOTH', 'PD SMOOTH', 'RHYTHM', 'SALSA ONE', 'SALSA TWO', 'SALSA SHINE', 'BACHATA', 'BACHATA SHINE')
    and exists (select 1 from federated.competition_round r where r.competition_id = c.id))
);
update crawler.frontier f set process_status = 'pending', last_process_error = null, last_process_error_at = null
where f.id in (select frontier_id from index_source where source_id = 'person-0'
  or competition_id in (select competition_id from zero_person_competition));
-- These stale indexes contain a different athlete link for the same participant.
-- Refetch the endpoint instead of altering cached source bodies or loader logic.
update crawler.frontier f set fetch_status = 'pending', process_status = 'pending', next_fetch_at = now()
where f.id in (select frontier_id from index_source where source_id <> competitor_id and source_id <> 'person-0');

delete from federated.competitor c
where c.federation = 'wdsf' and (c.external_id ~ '^[0-9]+$' or c.external_id = 'person-0')
  and not exists (select 1 from federated.competition_result r where r.competitor_id = c.id)
  and not exists (select 1 from federated.competition_entry e where e.competitor_id = c.id)
  and not exists (select 1 from federated.competition_round_result r where r.competitor_id = c.id)
  and not exists (select 1 from federated.competitor_category_progress p where p.competitor_id = c.id)
  and not exists (select 1 from federated.competitor_club_affiliation a where a.competitor_id = c.id)
  and not exists (select 1 from federated.ranklist_entry r where r.competitor_id = c.id);
delete from federated.person p where p.id = 'wdsf:0';
do $$ begin
  if exists (
    with score_competitors as materialized (
      select distinct s.competitor_id from federated.judge_score s
      join federated.competition_round r on r.id = s.round_id
      join federated.competition c on c.id = r.competition_id and c.federation = 'wdsf'
    )
    select 1 from score_competitors s
    left join federated.competitor competitor on competitor.id = 'wdsf:' || s.competitor_id
    where s.competitor_id ~ '^[0-9]+$' or competitor.id is null
  ) then raise exception 'Unmapped WDSF score IDs remain. No legacy identity cleanup was committed.'; end if;
end $$;
commit;

-- Preserve valid couple payloads stored under obsolete numeric frontier keys.
-- Their original request URLs and fetch times remain in the response history.
create temp table obsolete_frontier as
select id from crawler.frontier where federation = 'wdsf' and kind = 'couple' and key ~ '^[0-9]+$';
insert into crawler.frontier (federation, kind, key)
select distinct 'wdsf', 'couple', m.new_id from identity_map m
join affected_competition a using (competition_id)
where m.competitor_type = 'couple' on conflict do nothing;
insert into crawler.frontier (federation, kind, key)
select distinct 'wdsf', 'couple', cache.content->>'id'
from obsolete_frontier old join crawler.json_response r on r.frontier_id = old.id
join crawler.json_response_cache cache using (content_hash)
where cache.content->>'id' ~ '^(rls|wdsf)-[0-9]+$' on conflict do nothing;
begin;
update crawler.json_response r set frontier_id = canonical.id,
  sys_period = case when r.sys_period is not null and not upper_inf(r.sys_period) then r.sys_period
    else tstzrange(r.fetched_at, greatest(clock_timestamp(),
      coalesce(r.last_observed_at, r.fetched_at) + interval '1 microsecond'), '[)') end,
  last_observed_at = coalesce(r.last_observed_at, r.fetched_at)
from obsolete_frontier old, crawler.json_response_cache cache, crawler.frontier canonical
where r.frontier_id = old.id and cache.content_hash = r.content_hash
  and canonical.federation = 'wdsf' and canonical.kind = 'couple' and canonical.key = cache.content->>'id'
  and canonical.key ~ '^(rls|wdsf)-[0-9]+$';
with cached as materialized (
  select distinct on (r.frontier_id) r.frontier_id, r.id, r.fetched_at
  from crawler.json_response r join crawler.frontier f on f.id = r.frontier_id
  where f.federation = 'wdsf' and f.kind = 'couple'
    and f.last_successful_response_id is null and f.last_response_id is null
    and r.http_status = 200 and r.error is null
  order by r.frontier_id, r.fetched_at desc, r.id desc
), reopened as (
  update crawler.json_response r set sys_period = tstzrange(r.fetched_at, null, '[)')
  where r.id in (select id from cached) returning r.id
)
update crawler.frontier f set last_response_id = coalesce(f.last_response_id, cached.id),
  last_successful_response_id = cached.id, last_fetched_at = cached.fetched_at,
  fetch_status = 'ok', process_status = 'pending'
from cached join reopened on reopened.id = cached.id where f.id = cached.frontier_id;
select graphile_worker.complete_jobs(array_agg(j.job_id))
from crawler.frontier_fetch_job j join obsolete_frontier old on j.frontier_id = old.id;
delete from crawler.frontier f using obsolete_frontier old where f.id = old.id;
commit;

select competitor_type, count(*) remaining_legacy_competitors
from federated.competitor where federation = 'wdsf' and external_id ~ '^[0-9]+$'
group by competitor_type;
do $$ begin
  if exists (select 1 from federated.competitor
    where federation = 'wdsf' and (external_id ~ '^[0-9]+$' or external_id = 'person-0')) then
    raise exception 'Legacy WDSF competitors still have references. Inspect them before resuming workers.';
  end if;
end $$;
select graphile_worker.add_job('frontier_schedule', '{}'::json, job_key => 'frontier_schedule');
