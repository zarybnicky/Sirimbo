/* @name GetCompetitionWithCategory */
SELECT
  c.id AS competition_id,
  c.event_id,
  c.category_id,
  c.start_date::text AS start_date,
  cat.series,
  cat.discipline,
  cat.age_group,
  cat.gender_group,
  cat.class,
  cat.competitor_type::text AS competitor_type
FROM federated.competition c
JOIN federated.category cat ON cat.id = c.category_id
WHERE c.federation = :federation AND c.external_id = :competitionExternalId;

/* @name UpsertFederationAthlete */
SELECT federated.upsert_athlete(
  in_federation     => :federation,
  in_external_id    => :externalId,
  in_canonical_name => :canonicalName,
  in_gender         => :gender::federated.gender
) AS athlete_id;

/* @name UpsertCategory */
SELECT federated.upsert_category(
  in_series       => :series,
  in_discipline   => :discipline,
  in_age_group    => :ageGroup,
  in_gender_group => :genderGroup,
  in_class        => :class,
  in_competitor_type => :competitorType::federated.competitor_type
) as id;

/* @name UpsertCompetitor */
SELECT federated.upsert_competitor(
  in_federation => :federation,
  in_external_id => :federationCompetitorId,
  in_type => :type::federated.competitor_type,
  in_label => :label,
  in_components => ARRAY(
    SELECT (u.athlete_id, u.role)::federated.competitor_component_input
    FROM unnest(
      :component_athlete_ids::bigint[],
      :component_roles::federated.competitor_role[]
    ) AS u(athlete_id, role)
  )
) as competitor_id;

/* @name UpsertManyCompetitors */
SELECT
  input.external_id AS federation_id,
  federated.upsert_competitor(
    in_federation  => input.federation,
    in_external_id => input.external_id,
    in_type        => input.competitor_type,
    in_label       => input.label,
    in_components  => '{}'::federated.competitor_component_input[]
  ) AS federated_id
FROM unnest(
  :federations::text[],
  :external_ids::text[],
  :types::federated.competitor_type[],
  :labels::text[]
) AS input(federation, external_id, competitor_type, label);

/* @name UpsertCompetitorProgress */
SELECT federated.upsert_competitor_category_progress(
  in_federation      => :federation,
  in_competitor_id   => :competitorId,
  in_category_id     => :categoryId,
  in_points          => :points::numeric(10,3),
  in_domestic_finale => :domesticFinale::int,
  in_foreign_finale  => :foreignFinale::int
);

/* @name UpsertRanklistSnapshot */
SELECT federated.upsert_ranklist_snapshot(
  in_federation     => :federation,
  in_category_id    => :categoryId,
  in_ranklist_name  => :ranklistName,
  in_as_of_date     => :asOfDate::date,
  in_kind           => COALESCE(:kind, 'default'),
  in_entries        => :entries::text::jsonb
) AS snapshot_id;

/* @name UpsertCompetitionEntries */
WITH ins AS (
  INSERT INTO federated.competition_entry (competition_id, competitor_id, cancelled)
  SELECT
    :competitionId::bigint,
    competitor_id,
    cancelled
  FROM unnest(
    :competitorId::bigint[],
    :cancelled::boolean[]
  ) AS t(competitor_id, cancelled)
  ON CONFLICT (competition_id, competitor_id)
    DO UPDATE SET cancelled = EXCLUDED.cancelled
  RETURNING 1
)
SELECT count(*)::int FROM ins;

/* @name UpsertCompetitonResults */
WITH ins AS (
  INSERT INTO federated.competition_result (competition_id, competitor_id, start_number, ranking, ranking_to)
  SELECT
    :competition_id::bigint,
    competitor_id,
    nullif(start_number, ''),
    ranking,
    ranking_to
  FROM unnest(
    :competitor_id::bigint[],
    :start_number:text[],
    :ranking::integer[],
    :ranking_to::integer[]
  ) AS t(competitor_id, start_number, ranking, ranking_to)
  ON CONFLICT (competition_id, competitor_id)
    DO UPDATE SET
      start_number = EXCLUDED.start_number,
      ranking      = EXCLUDED.ranking,
      ranking_to   = EXCLUDED.ranking_to
  RETURNING 1
)
SELECT count(*)::int FROM ins;

/* @name UpsertCompetitionRoundResults */
INSERT INTO federated.competition_round_result
  (round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score)
SELECT
  s.round_id,
  :competitorId::bigint,
  s.overall_ranking,
  s.overall_ranking_to,
  s.qualified_next,
  s.overall_score
FROM unnest(
  :roundIds::bigint[],
  :overallRanking::integer[],
  :overallRankingTo::integer[],
  :qualifiedNext::boolean[],
  :overallScores::numeric[]
) AS s(round_id, overall_ranking, overall_ranking_to, qualified_next, overall_score)
ON CONFLICT (round_id, competitor_id)
  DO UPDATE SET
    overall_ranking     = EXCLUDED.overall_ranking,
    overall_ranking_to  = EXCLUDED.overall_ranking_to,
    qualified_next      = EXCLUDED.qualified_next,
    overall_score       = EXCLUDED.overall_score;

/* @name UpsertJudgeScores */
WITH ins AS (
  INSERT INTO federated.judge_score
    (federation, event_date, event_id, competition_id, category_id, round_id, dance_code, judge_id, competitor_id, component, score, raw_score)
  SELECT
    :federation,
    :eventDate,
    :eventId,
    :competitionId,
    :categoryId,
    round_id,
    dance_code,
    judge_id,
    :competitorId,
    component,
    score,
    nullif(raw_score, '')
  FROM unnest(
    :roundId::bigint[],
    :danceCode::text[],
    :judgeId::bigint[],
    :component::federated.score_component[],
    :score::numeric[],
    :rawScore::text[]
  ) AS t(round_id, dance_code, judge_id, component, score, raw_score)
  ON CONFLICT (round_id, dance_code, judge_id, competitor_id, component)
    DO UPDATE SET
      score     = EXCLUDED.score,
      raw_score = EXCLUDED.raw_score
  RETURNING 1
)
SELECT count(*)::int FROM ins;

/* @name UpsertRoundsAndRoundDances */
WITH upserted AS (
  INSERT INTO federated.competition_round (
    competition_id, round_key, round_label, round_index, dance_program_id, scoring_method
  )
  SELECT
    :competitionId::bigint,
    round_key,
    nullif(round_label, ''),
    round_index,
    dance_program_id,
    scoring_method
  FROM unnest(
    :roundKeys::text[],
    :roundLabels::text[],
    :roundIndexes::int[],
    :danceProgramIds::bigint[],
    :scoringMethods::federated.scoring_method[]
  ) AS rk(round_key, round_label, round_index, dance_program_id, scoring_method)
  ON CONFLICT (competition_id, round_key)
    DO UPDATE SET
      round_label      = EXCLUDED.round_label,
      round_index      = EXCLUDED.round_index,
      dance_program_id = EXCLUDED.dance_program_id,
      scoring_method   = EXCLUDED.scoring_method
  RETURNING id, round_key
),
ins_round_dance AS (
  INSERT INTO federated.round_dance (round_id, dance_code)
  SELECT u.id, p.dance_code
  FROM unnest(
    :roundKeys::text[],
    :danceCodes::text[]
  ) AS p(round_key, dance_code)
  JOIN upserted u USING (round_key)
  ON CONFLICT (round_id, dance_code) DO NOTHING
  RETURNING 1
)
SELECT id, round_key FROM upserted;
