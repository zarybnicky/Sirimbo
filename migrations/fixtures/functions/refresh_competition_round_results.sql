create or replace function app_private.refresh_competition_round_results(p_competition_id bigint)
returns void language sql volatile as $$
WITH scoped_rounds AS (
  SELECT id, round_key, round_index, scoring_method
  FROM federated.competition_round
  WHERE competition_id = p_competition_id
), presence AS (
  SELECT
    sr.id AS round_id,
    sr.round_index,
    sr.scoring_method,
    cr.competitor_id,
    competitor.external_id AS competitor_external_id,
    cr.ranking,
    cr.ranking_to,
    last_round.round_index AS last_round_index
  FROM federated.competition_result cr
  JOIN federated.competitor competitor ON competitor.id = cr.competitor_id
  JOIN scoped_rounds last_round ON last_round.round_key = cr.last_round
  JOIN scoped_rounds sr ON sr.round_index <= last_round.round_index
  WHERE cr.competition_id = p_competition_id
), scores AS MATERIALIZED (
  SELECT js.round_id, js.dance_order, js.dance_code, js.competitor_id, sum(js.score)::numeric(10,3) AS score
  FROM federated.judge_score js
  JOIN scoped_rounds sr ON sr.id = js.round_id
  GROUP BY js.round_id, js.dance_order, js.dance_code, js.competitor_id
), dance_scores AS (
  SELECT
    p.round_id,
    p.round_index,
    p.scoring_method,
    p.competitor_id,
    p.ranking,
    p.ranking_to,
    rd.dance_order,
    CASE
      WHEN p.scoring_method = 'skating_marks' THEN coalesce(js.score, 0)::numeric(10,3)
      ELSE js.score
    END AS dance_score
  FROM presence p
  JOIN federated.round_dance rd ON rd.round_id = p.round_id
  LEFT JOIN scores js
    ON js.round_id = p.round_id
   AND js.dance_order = rd.dance_order
   AND js.dance_code = rd.dance_code
   AND js.competitor_id = p.competitor_external_id
), round_scores AS (
  SELECT
    ds.round_id,
    ds.round_index,
    ds.scoring_method,
    ds.competitor_id,
    ds.ranking,
    ds.ranking_to,
    CASE
      WHEN ds.scoring_method = 'skating_marks'
        OR count(ds.dance_score) FILTER (WHERE ds.dance_score IS NOT NULL) > 0
      THEN sum(ds.dance_score)::numeric(10,3)
      ELSE NULL
    END AS overall_score,
    CASE
      WHEN ds.scoring_method = 'skating_marks'
        OR count(ds.dance_score) FILTER (WHERE ds.dance_score IS NOT NULL) > 0
      THEN array_agg(ds.dance_score::real ORDER BY ds.dance_order)
      ELSE NULL
    END AS dance_results
  FROM dance_scores ds
  GROUP BY
    ds.round_id, ds.round_index, ds.scoring_method, ds.competitor_id,
    ds.ranking, ds.ranking_to
), ranked AS (
  SELECT
    round_scores.*,
    CASE
      WHEN scoring_method = 'skating_marks' AND overall_score IS NOT NULL THEN
        rank() OVER (PARTITION BY round_id ORDER BY overall_score DESC NULLS LAST)::int
      WHEN scoring_method = 'skating_places' THEN
        ranking
      ELSE NULL
    END AS overall_ranking,
    CASE
      WHEN scoring_method = 'skating_marks' AND overall_score IS NOT NULL THEN
        count(*) OVER (PARTITION BY round_id, overall_score)::int
      ELSE NULL
    END AS tie_count
  FROM round_scores
), source AS (
  SELECT
    ranked.round_id,
    ranked.competitor_id,
    ranked.overall_ranking,
    CASE
      WHEN ranked.overall_ranking IS NULL THEN NULL
      WHEN ranked.scoring_method = 'skating_places' THEN
        coalesce(ranked.ranking_to, ranked.ranking)
      ELSE ranked.overall_ranking + ranked.tie_count - 1
    END AS overall_ranking_to,
    EXISTS (
      SELECT 1
      FROM presence later
      WHERE later.competitor_id = ranked.competitor_id
        AND later.last_round_index > ranked.round_index
    ) AS qualified_next,
    ranked.overall_score,
    ranked.dance_results
  FROM ranked
), upserted AS (
  INSERT INTO federated.competition_round_result (
    round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score, dance_results
  )
  SELECT
    round_id, competitor_id, overall_ranking, overall_ranking_to,
    qualified_next, overall_score, dance_results
  FROM source
  WHERE NOT EXISTS (
    SELECT 1 FROM federated.competition_round_result r
    WHERE (r.round_id, r.competitor_id) = (source.round_id, source.competitor_id)
      AND (r.overall_ranking, r.overall_ranking_to, r.qualified_next, r.overall_score, r.dance_results)
        IS NOT DISTINCT FROM (source.overall_ranking, source.overall_ranking_to,
          source.qualified_next, source.overall_score, source.dance_results)
  )
  ORDER BY round_id, competitor_id
  ON CONFLICT (round_id, competitor_id) DO UPDATE
    SET overall_ranking = EXCLUDED.overall_ranking,
        overall_ranking_to = EXCLUDED.overall_ranking_to,
        qualified_next = EXCLUDED.qualified_next,
        overall_score = EXCLUDED.overall_score,
        dance_results = EXCLUDED.dance_results
    WHERE federated.competition_round_result.overall_ranking IS DISTINCT FROM EXCLUDED.overall_ranking
       OR federated.competition_round_result.overall_ranking_to IS DISTINCT FROM EXCLUDED.overall_ranking_to
       OR federated.competition_round_result.qualified_next IS DISTINCT FROM EXCLUDED.qualified_next
       OR federated.competition_round_result.overall_score IS DISTINCT FROM EXCLUDED.overall_score
       OR federated.competition_round_result.dance_results IS DISTINCT FROM EXCLUDED.dance_results
  RETURNING 1
), touched AS (
  SELECT count(*) FROM upserted
)
DELETE FROM federated.competition_round_result crr
USING scoped_rounds sr, touched
WHERE crr.round_id = sr.id
  AND NOT EXISTS (
    SELECT 1
    FROM source
    WHERE source.round_id = crr.round_id
      AND source.competitor_id = crr.competitor_id
  );
$$;
