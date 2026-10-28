-- Run after deploying WDSF round postprocessing.
-- Reprocess one cached participant per affected competition, without refetching.
begin;

with ballroom as (
  select array['SW', 'TG', 'VW', 'SF', 'QS', 'SA', 'CH', 'RU', 'PD', 'JI'] as codes
), dances as materialized (
  select c.id as competition_id, c.external_id, cr.id as round_id, dp.code as program_code,
    rd.dance_code, rd.dance_order,
    exists (
      select 1 from federated.judge_score js
      where (js.round_id, js.dance_order, js.dance_code)
        = (rd.round_id, rd.dance_order, rd.dance_code)
    ) as has_scores
  from federated.competition c
  join federated.competition_round cr on cr.competition_id = c.id
  join federated.dance_program dp on dp.id = cr.dance_program_id
  left join federated.round_dance rd on rd.round_id = cr.id
  where c.federation = 'wdsf'
), rounds as (
  select competition_id, external_id, round_id, program_code,
    coalesce(array_agg(dance_code order by dance_order)
      filter (where dance_code is not null), '{}') as current_codes,
    coalesce(array_agg(dance_order order by dance_order)
      filter (where dance_order is not null), '{}') as current_orders,
    coalesce(array_agg(dance_code order by dance_order)
      filter (where has_scores), '{}') as scored_codes
  from dances
  group by competition_id, external_id, round_id, program_code
), expected as (
  select r.*, case when scored_codes <@ b.codes then
    array(select code from unnest(scored_codes) with ordinality as d(code, position)
          order by array_position(b.codes, code), position)
    else scored_codes end as codes
  from rounds r cross join ballroom b
), affected_rounds as materialized (
  select * from expected
  where current_codes is distinct from codes
    or current_orders is distinct from array(select generate_series(1, cardinality(codes)))
    or program_code is distinct from array_to_string(array(
      select code from unnest(codes) with ordinality as d(code, position)
      group by code order by min(position)
    ), '-')
), selected as materialized (
  select c.competition_id, c.external_id, participant.id as frontier_id
  from (select distinct competition_id, external_id from affected_rounds) c
  left join lateral (
    select f.id
    from crawler.frontier idx
    join crawler.json_response index_response on index_response.id = idx.last_successful_response_id
    join crawler.json_response_cache index_cache on index_cache.content_hash = index_response.content_hash
    cross join lateral jsonb_array_elements(index_cache.content) entry
    join crawler.frontier f
      on f.federation = 'wdsf' and f.kind = 'participant' and f.key = entry->>'id'
    join crawler.json_response response on response.id = f.last_successful_response_id
    join crawler.json_response_cache cache on cache.content_hash = response.content_hash
    where idx.federation = 'wdsf' and idx.kind = 'participantIndex' and idx.key = c.external_id
      and f.fetch_status = 'ok' and f.process_status in ('ok', 'pending')
      and cache.content->>'competitionId' = c.external_id
      and cache.content @? '$.link[*] ? (
        @.rel == "http://services.worlddancesport.org/rel/participant/couple" ||
        @.rel == "http://services.worlddancesport.org/rel/participant/team" ||
        @.rel == "http://services.worlddancesport.org/rel/participant/athlete")'
      and (
        cache.content @? '$.rounds[*].dances[*].scores[*] ? (
          @.kind == "mark" || @.kind == "final" || @.kind == "onScale" ||
          @.kind == "onScale2" || @.kind == "onScale3" || @.kind == "onScaleSkating")'
        or cache.content @? '$.rounds[*] ? (
          !exists(@.dances[*].scores[*]) && exists(@.dances[*] ? (
            @.name == "WALTZ" || @.name == "TANGO" || @.name == "VIENNESE WALTZ" ||
            @.name == "SLOW FOXTROT" || @.name == "QUICKSTEP" || @.name == "SAMBA" ||
            @.name == "CHA CHA CHA" || @.name == "RUMBA" || @.name == "PASO DOBLE" || @.name == "JIVE")))'
      )
    limit 1
  ) participant on true
), marked as (
  update crawler.frontier f set process_status = 'pending'
  from selected s
  where f.id = s.frontier_id and f.process_status <> 'pending'
  returning f.id
)
select
  (select count(*) from affected_rounds) as affected_rounds,
  count(*) as affected_competitions,
  count(frontier_id) as selected_participants,
  (select count(*) from marked) as marked_pending,
  count(*) filter (where frontier_id is null) as competitions_without_eligible_participant,
  coalesce(array_agg(external_id order by external_id) filter (where frontier_id is null), '{}')
    as competitions_without_eligible_participant_ids
from selected;

commit;
