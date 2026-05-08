/* @name EnsurePeople */
INSERT INTO federated.person (federation, external_id, canonical_name, gender)
SELECT federation, external_id, canonical_name, gender
FROM unnest(
  :federation::text[],
  :externalId::text[],
  :canonicalName::text[],
  :gender::federated.gender[]
) AS i (federation, external_id, canonical_name, gender)
ON CONFLICT (id) DO NOTHING;

/* @name UpsertPeopleDetailed */
INSERT INTO federated.person (
  federation, external_id, canonical_name, first_name, last_name, gender, nationality, age_group, medical_checkup_expiration
)
SELECT
  federation,
  external_id,
  nullif(canonical_name, ''),
  nullif(first_name, ''),
  nullif(last_name, ''),
  gender,
  nullif(nationality, ''),
  nullif(age_group, ''),
  CAST(NULLIF(medical_checkup_expiration, '') AS date)
FROM unnest(
  :federation::text[],
  :externalId::text[],
  :canonicalName::text[],
  :firstName::text[],
  :lastName::text[],
  :gender::federated.gender[],
  :nationality::text[],
  :ageGroup::text[],
  :medicalCheckupExpiration::text[]
) AS input(
  federation, external_id, canonical_name, first_name, last_name, gender, nationality, age_group, medical_checkup_expiration)
ON CONFLICT (id)
  DO UPDATE SET
    canonical_name = COALESCE(EXCLUDED.canonical_name, federated.person.canonical_name),
    first_name = EXCLUDED.first_name,
    last_name = EXCLUDED.last_name,
    gender = CASE
      WHEN EXCLUDED.gender IS NOT NULL AND EXCLUDED.gender <> 'unknown'
      THEN EXCLUDED.gender
      ELSE federated.person.gender
    END,
    nationality = EXCLUDED.nationality,
    age_group = EXCLUDED.age_group,
    medical_checkup_expiration = EXCLUDED.medical_checkup_expiration;

/* @name GetAllCategories */
SELECT id, series, discipline, age_group AS "ageGroup",
       gender_group AS "genderGroup", class, competitor_type AS "competitorType"
FROM federated.category;

/* @name UpsertCategories */
WITH input AS (
  SELECT series, discipline, age_group, gender_group, class, competitor_type
  FROM unnest(
    :series::text[],
    :discipline::text[],
    :ageGroup::text[],
    :genderGroup::text[],
    :class::text[],
    :competitorType::federated.competitor_type[]
  ) AS input(series, discipline, age_group, gender_group, class, competitor_type)
), inserted AS (
  INSERT INTO federated.category (series, discipline, age_group, gender_group, class, competitor_type, name)
  SELECT *, concat_ws(' ', series, age_group, nullif(competitor_type, 'couple'), nullif(class, ''), discipline)
  FROM input
  ON CONFLICT (series, discipline, age_group, gender_group, class, competitor_type) DO NOTHING
  RETURNING *
), result AS (
  SELECT * FROM inserted
  UNION
  SELECT c.* FROM federated.category c
  JOIN input i USING (series, discipline, age_group, gender_group, class, competitor_type)
)
SELECT
  id AS "id!",
  class AS "class!",
  series AS "series!",
  discipline AS "discipline!",
  age_group AS "ageGroup!",
  gender_group AS "genderGroup!",
  competitor_type AS "competitorType!"
FROM result;

/* @name EnsureCompetitors */
INSERT INTO federated.competitor (federation, external_id, competitor_type, name)
SELECT input.federation, input.external_id, input.competitor_type, input.name
FROM unnest(
  :federation::text[],
  :externalId::text[],
  :type::federated.competitor_type[],
  :label::text[]
) AS input(federation, external_id, competitor_type, name)
ON CONFLICT (id) DO NOTHING;

/* @name EnsureCompetitorsWithComponents */
WITH competitor_input AS (
  SELECT federation, external_id, competitor_type, name
  FROM unnest(
    :federation::text[],
    :externalId::text[],
    :type::federated.competitor_type[],
    :label::text[]
  ) AS input(federation, external_id, competitor_type, name)
), inserted_competitor AS (
  INSERT INTO federated.competitor (federation, external_id, competitor_type, name)
  SELECT federation, external_id, competitor_type, name
  FROM competitor_input
  ON CONFLICT (id) DO NOTHING
  RETURNING id
), existing_competitor AS (
  SELECT c.id
  FROM federated.competitor c
  JOIN competitor_input
    ON c.federation = competitor_input.federation
   AND c.external_id = competitor_input.external_id
), candidate_competitor AS (
  SELECT id FROM inserted_competitor
  UNION
  SELECT id FROM existing_competitor
), component_input AS (
  SELECT
    component_competitor_id, person_id, person_federation, person_external_id,
    person_canonical_name, person_gender, component_role
  FROM unnest(
    :componentCompetitorId::text[],
    :personId::text[],
    :personFederation::text[],
    :personExternalId::text[],
    :personCanonicalName::text[],
    :personGender::federated.gender[],
    :componentRole::federated.competitor_role[]
  ) AS input(
    component_competitor_id, person_id, person_federation, person_external_id,
    person_canonical_name, person_gender, component_role
  )
), target_component AS (
  SELECT component_input.*
  FROM component_input
  JOIN candidate_competitor ON candidate_competitor.id = component_input.component_competitor_id
  WHERE NOT EXISTS (
    SELECT 1
    FROM federated.competitor_component component
    WHERE component.competitor_id = component_input.component_competitor_id
  )
), inserted_person AS (
  INSERT INTO federated.person (federation, external_id, canonical_name, gender)
  SELECT DISTINCT person_federation, person_external_id, person_canonical_name, person_gender
  FROM target_component
  ON CONFLICT (id) DO NOTHING
  RETURNING id
)
INSERT INTO federated.competitor_component (competitor_id, person_id, role)
SELECT component_competitor_id, person_id, component_role
FROM target_component
ON CONFLICT (competitor_id, person_id) DO NOTHING;

/* @name UpsertCompetitorsDetailed */
INSERT INTO federated.competitor (federation, external_id, competitor_type, name)
SELECT input.federation, input.external_id, input.competitor_type, input.name
FROM unnest(
  :federation::text[],
  :externalId::text[],
  :type::federated.competitor_type[],
  :label::text[]
) AS input(federation, external_id, competitor_type, name)
ON CONFLICT (id) DO UPDATE
  SET competitor_type = EXCLUDED.competitor_type,
      name = CASE
        WHEN EXCLUDED.name <> '' THEN EXCLUDED.name
        ELSE federated.competitor.name
      END;

/* @name MergeCompetitorComponents */
MERGE INTO federated.competitor_component AS t
USING (
  SELECT competitor_id, person_id, role
  FROM unnest(
    :competitorId::text[],
    :personId::text[],
    :role::federated.competitor_role[]
  ) AS input(competitor_id, person_id, role)
) AS s
ON t.competitor_id = s.competitor_id AND t.person_id = s.person_id
WHEN MATCHED AND t.role IS DISTINCT FROM s.role THEN
  UPDATE SET role = s.role
WHEN NOT MATCHED THEN
  INSERT (competitor_id, person_id, role)
  VALUES (s.competitor_id, s.person_id, s.role)
WHEN NOT MATCHED BY SOURCE
  AND t.competitor_id IN (SELECT unnest(:competitorId::text[])) THEN
  DELETE;

/* @name MergeCompetitorProgress */
MERGE INTO federated.competitor_category_progress AS t
USING (
  SELECT
    entry.competitor_id, entry.category_id, entry.points, entry.domestic_finals, entry.foreign_finals
  FROM unnest(
    :competitorId::text[],
    :categoryId::bigint[],
    :points::numeric(10,3)[],
    :domesticFinals::int[],
    :foreignFinals::int[]
  ) entry (competitor_id, category_id, points, domestic_finals, foreign_finals)
) AS s
ON t.competitor_id = s.competitor_id AND t.category_id = s.category_id
WHEN MATCHED THEN
  UPDATE SET points = s.points, domestic_finals = s.domestic_finals, foreign_finals = s.foreign_finals
WHEN NOT MATCHED THEN
  INSERT (competitor_id, category_id, points, domestic_finals, foreign_finals)
  VALUES (s.competitor_id, s.category_id, s.points, s.domestic_finals, s.foreign_finals)
WHEN NOT MATCHED BY SOURCE
  AND t.competitor_id IN (SELECT unnest(:competitorId::text[])) THEN
  DELETE;

/* @name UpsertRanklistSnapshot */
WITH upsert_ranklist AS (
  INSERT INTO federated.ranklist (federation, category_id, name)
  VALUES (:federation, :categoryId, :ranklistName)
  ON CONFLICT (federation, category_id)
    DO UPDATE SET name = EXCLUDED.name
  RETURNING id
),
upsert_snapshot AS (
  INSERT INTO federated.ranklist_snapshot (ranklist_id, as_of_date, kind)
  SELECT id, :asOfDate, COALESCE(:kind, 'default')
  FROM upsert_ranklist
  ON CONFLICT (ranklist_id, as_of_date, kind)
    DO UPDATE SET kind = EXCLUDED.kind
  RETURNING id
)
MERGE INTO federated.ranklist_entry t USING (
  SELECT s.id AS snapshot_id, e.competitor_id, e.ranking, e.ranking_to, e.points
  FROM upsert_snapshot s
  CROSS JOIN unnest(
    :entryCompetitorId::text[],
    :entryRanking::int[],
    :entryRankingTo::int[],
    :entryPoints::numeric(10,3)[]
  ) AS e(competitor_id, ranking, ranking_to, points)
) s
ON t.snapshot_id = s.snapshot_id AND t.competitor_id = s.competitor_id
WHEN MATCHED THEN
  UPDATE SET ranking = s.ranking, ranking_to = s.ranking_to, points = s.points
WHEN NOT MATCHED BY TARGET THEN
  INSERT (snapshot_id, competitor_id, ranking, ranking_to, points)
  VALUES (s.snapshot_id, s.competitor_id, s.ranking, s.ranking_to, s.points)
WHEN NOT MATCHED BY SOURCE
  AND t.snapshot_id = (SELECT id FROM upsert_snapshot) THEN
  DELETE;

/* @name UpsertFederationClubs */
INSERT INTO federated.federation_club (federation, external_id, name, city, country)
SELECT federation, external_id, name, nullif(city, ''), nullif(country, '')
FROM unnest(
  :federation::text[],
  :externalId::text[],
  :name::text[],
  :city::text[],
  :country::text[]
) AS input(federation, external_id, name, city, country)
ON CONFLICT (federation, external_id) DO UPDATE
  SET name = EXCLUDED.name,
      city = EXCLUDED.city,
      country = EXCLUDED.country
  WHERE federated.federation_club.name IS DISTINCT FROM EXCLUDED.name
     OR federated.federation_club.city IS DISTINCT FROM EXCLUDED.city
     OR federated.federation_club.country IS DISTINCT FROM EXCLUDED.country;

/* @name UpsertEvents */
INSERT INTO federated.event (
  federation, external_id, name, start_date, end_date, location, city, country, organizing_club_id
)
SELECT
  federation,
  external_id,
  nullif(name, ''),
  start_date,
  end_date,
  nullif(location, ''),
  nullif(city, ''),
  nullif(country, ''),
  CAST(NULLIF(organizing_club_id, '') AS bigint)
FROM unnest(
  :federation::text[],
  :externalId::text[],
  :name::text[],
  :startDate::date[],
  :endDate::date[],
  :location::text[],
  :city::text[],
  :country::text[],
  :organizingClubId::text[]
) AS input(
  federation, external_id, name, start_date, end_date, location, city, country, organizing_club_id
)
ON CONFLICT (federation, external_id) DO UPDATE
  SET name = COALESCE(EXCLUDED.name, federated.event.name),
      start_date = EXCLUDED.start_date,
      end_date = COALESCE(EXCLUDED.end_date, federated.event.end_date),
      location = COALESCE(EXCLUDED.location, federated.event.location),
      city = COALESCE(EXCLUDED.city, federated.event.city),
      country = COALESCE(EXCLUDED.country, federated.event.country),
      organizing_club_id = COALESCE(EXCLUDED.organizing_club_id, federated.event.organizing_club_id);

/* @name UpsertEventsDetailed */
INSERT INTO federated.event (
  federation, external_id, name, start_date, end_date, location, city, country, street_address, postal_code,
  address_note, geo_reference, floor_size, contact_name, contact_phone, contact_email, website_url, organizing_club_id
)
SELECT
  federation,
  external_id,
  nullif(name, ''),
  start_date,
  end_date,
  nullif(location, ''),
  nullif(city, ''),
  nullif(country, ''),
  nullif(street_address, ''),
  nullif(postal_code, ''),
  nullif(address_note, ''),
  nullif(geo_reference, ''),
  nullif(floor_size, ''),
  nullif(contact_name, ''),
  nullif(contact_phone, ''),
  nullif(contact_email, ''),
  nullif(website_url, ''),
  CAST(NULLIF(organizing_club_id, '') AS bigint)
FROM unnest(
  :federation::text[],
  :externalId::text[],
  :name::text[],
  :startDate::date[],
  :endDate::date[],
  :location::text[],
  :city::text[],
  :country::text[],
  :streetAddress::text[],
  :postalCode::text[],
  :addressNote::text[],
  :geoReference::text[],
  :floorSize::text[],
  :contactName::text[],
  :contactPhone::text[],
  :contactEmail::text[],
  :websiteUrl::text[],
  :organizingClubId::text[]
) AS input(
  federation, external_id, name, start_date, end_date, location, city, country, street_address, postal_code,
  address_note, geo_reference, floor_size, contact_name, contact_phone, contact_email, website_url, organizing_club_id
)
ON CONFLICT (federation, external_id) DO UPDATE
  SET name = COALESCE(EXCLUDED.name, federated.event.name),
      start_date = EXCLUDED.start_date,
      end_date = COALESCE(EXCLUDED.end_date, federated.event.end_date),
      location = COALESCE(EXCLUDED.location, federated.event.location),
      city = COALESCE(EXCLUDED.city, federated.event.city),
      country = COALESCE(EXCLUDED.country, federated.event.country),
      street_address = EXCLUDED.street_address,
      postal_code = EXCLUDED.postal_code,
      address_note = EXCLUDED.address_note,
      geo_reference = EXCLUDED.geo_reference,
      floor_size = EXCLUDED.floor_size,
      contact_name = EXCLUDED.contact_name,
      contact_phone = EXCLUDED.contact_phone,
      contact_email = EXCLUDED.contact_email,
      website_url = EXCLUDED.website_url,
      organizing_club_id = COALESCE(EXCLUDED.organizing_club_id, federated.event.organizing_club_id);

/* @name GetAllDancePrograms */
SELECT id, code FROM federated.dance_program;

/* @name UpsertDancePrograms */
WITH input_program AS (
  SELECT code, name, nullif(discipline, '') AS discipline
  FROM unnest(
    :code::text[],
    :name::text[],
    :discipline::text[]
  ) AS input(code, name, discipline)
), programs AS (
  INSERT INTO federated.dance_program (code, name, discipline)
  SELECT code, name, discipline
  FROM input_program
  ON CONFLICT (code) DO UPDATE
    SET name = EXCLUDED.name,
        discipline = EXCLUDED.discipline
  RETURNING id, code
), input_dance AS (
  SELECT program_code, dance_code, dance_order
  FROM unnest(
    :programCode::text[],
    :danceCode::text[],
    :danceOrder::int[]
  ) AS input(program_code, dance_code, dance_order)
), deleted AS (
  DELETE FROM federated.dance_program_dance dpd
  USING programs p
  WHERE dpd.program_id = p.id
    AND NOT EXISTS (
      SELECT 1
      FROM input_dance keep
      WHERE keep.program_code = p.code
        AND keep.dance_code = dpd.dance_code
    )
), upserted AS (
  INSERT INTO federated.dance_program_dance (program_id, dance_code, dance_order)
  SELECT p.id, input_dance.dance_code, input_dance.dance_order
  FROM input_dance
  JOIN programs p ON p.code = input_dance.program_code
  ON CONFLICT (program_id, dance_code) DO UPDATE
    SET dance_order = EXCLUDED.dance_order
)
SELECT p.id AS "id!", p.code AS "code!"
FROM programs p;

/* @name UpsertCompetitions */
WITH event AS (
  SELECT id
  FROM federated.event
  WHERE federation = :federation AND external_id = :eventExternalId
), input AS (
  SELECT *
  FROM unnest(
    :externalId::text[],
    :categoryId::bigint[],
    :startDate::date[],
    :endDate::date[],
    :participantsTotal::int[],
    :checkInEnd::text[],
    :completedAt::text[],
    :registrationFee::text[],
    :excusedTotal::int[]
  ) AS input(
    external_id, category_id, start_date, end_date, participants_total,
    check_in_end, completed_at, registration_fee, excused_total
  )
)
INSERT INTO federated.competition (
  federation, external_id, event_id, category_id, start_date, end_date, participants_total,
  check_in_end, completed_at, registration_fee, excused_total
)
SELECT
  :federation,
  input.external_id,
  event.id,
  input.category_id,
  input.start_date,
  input.end_date,
  input.participants_total,
  nullif(input.check_in_end, '')::time,
  nullif(input.completed_at, '')::timestamptz,
  nullif(input.registration_fee, '')::numeric(10,3),
  input.excused_total
FROM input
CROSS JOIN event
ON CONFLICT (federation, external_id) DO UPDATE
  SET event_id = EXCLUDED.event_id,
      category_id = EXCLUDED.category_id,
      start_date = EXCLUDED.start_date,
      end_date = EXCLUDED.end_date,
      participants_total = EXCLUDED.participants_total,
      check_in_end = EXCLUDED.check_in_end,
      completed_at = EXCLUDED.completed_at,
      registration_fee = EXCLUDED.registration_fee,
      excused_total = EXCLUDED.excused_total;

/* @name MergeEventOfficials */
WITH event AS (
  SELECT id
  FROM federated.event
  WHERE federation = :federation
    AND external_id = :eventExternalId
), source AS (
  SELECT
    event.id AS event_id,
    person_id,
    role,
    nullif(discipline, '') AS discipline,
    nullif(grade, '') AS grade
  FROM event
  CROSS JOIN unnest(
    :personId::text[],
    :role::federated.official_role[],
    :discipline::text[],
    :grade::text[]
  ) AS input(person_id, role, discipline, grade)
)
MERGE INTO federated.event_official AS t
USING source AS s
ON t.event_id = s.event_id
  AND t.person_id = s.person_id
  AND t.role = s.role
  AND t.discipline = coalesce(s.discipline, '')
WHEN MATCHED AND t.grade IS DISTINCT FROM s.grade THEN
  UPDATE SET grade = s.grade
WHEN NOT MATCHED THEN
  INSERT (event_id, person_id, role, discipline, grade)
  VALUES (s.event_id, s.person_id, s.role, coalesce(s.discipline, ''), s.grade)
WHEN NOT MATCHED BY SOURCE
  AND t.event_id IN (SELECT id FROM event) THEN
  DELETE;

/* @name MergeCompetitionOfficials */
WITH input AS (
  SELECT competition_external_id, person_id, role
  FROM unnest(
    :competitionExternalId::text[],
    :personId::text[],
    :role::federated.official_role[]
  ) AS input(competition_external_id, person_id, role)
), competitions AS (
  SELECT id, external_id
  FROM federated.competition
  WHERE federation = :federation
    AND external_id IN (SELECT unnest(:scopeCompetitionExternalId::text[]))
), source AS (
  SELECT competitions.id AS competition_id, input.person_id, input.role
  FROM input
  JOIN competitions ON competitions.external_id = input.competition_external_id
)
MERGE INTO federated.competition_official AS t
USING source AS s
ON t.competition_id = s.competition_id
  AND t.person_id = s.person_id
  AND t.role = s.role
WHEN NOT MATCHED THEN
  INSERT (competition_id, person_id, role)
  VALUES (s.competition_id, s.person_id, s.role)
WHEN NOT MATCHED BY SOURCE
  AND t.competition_id IN (SELECT id FROM competitions) THEN
  DELETE;

/* @name GetCompetitionContext */
SELECT
  c.id,
  c.event_id AS "eventId",
  c.category_id AS "categoryId",
  c.start_date AS "startDate",
  cat.competitor_type AS "competitorType"
FROM federated.competition c
JOIN federated.category cat ON cat.id = c.category_id
WHERE c.federation = :federation
  AND c.external_id = :externalId;

/* @name MergeCompetitionEntriesByEventId */
WITH input AS (
  SELECT competition_external_id, competitor_id, cancelled
  FROM unnest(
    :competitionExternalId::text[],
    :competitorId::text[],
    :cancelled::boolean[]
  ) AS input(competition_external_id, competitor_id, cancelled)
), competitions AS (
  SELECT c.id, c.external_id
  FROM federated.competition c
  JOIN federated.event e ON e.id = c.event_id
  WHERE c.federation = :federation AND e.external_id = :eventId
), source AS (
  SELECT competitions.id AS competition_id, input.competitor_id, input.cancelled
  FROM input
  JOIN competitions ON competitions.external_id = input.competition_external_id
)
MERGE INTO federated.competition_entry AS t
USING source AS s
ON t.competition_id = s.competition_id AND t.competitor_id = s.competitor_id
WHEN MATCHED AND t.cancelled IS DISTINCT FROM s.cancelled THEN
  UPDATE SET cancelled = s.cancelled
WHEN NOT MATCHED THEN
  INSERT (competition_id, competitor_id, cancelled)
  VALUES (s.competition_id, s.competitor_id, s.cancelled)
WHEN NOT MATCHED BY SOURCE
  AND t.competition_id IN (SELECT id FROM competitions) THEN
  DELETE;

/* @name MergeCompetitionResults */
MERGE INTO federated.competition_result AS t
USING (
  SELECT
    competitor_id,
    nullif(start_number, '') AS start_number,
    ranking,
    ranking_to,
    nullif(point_gain, '')::numeric(10,3) AS point_gain,
    nullif(final_gain, '')::numeric(10,3) AS final_gain,
    is_final,
    nullif(completion_status, '') AS completion_status,
    nullif(last_round, '') AS last_round,
    nullif(last_dance, '') AS last_dance
  FROM unnest(
    :competitorId::text[],
    :startNumber::text[],
    :ranking::int[],
    :rankingTo::int[],
    :pointGain::text[],
    :finalGain::text[],
    :isFinal::boolean[],
    :completionStatus::text[],
    :lastRound::text[],
    :lastDance::text[]
  ) AS input(
    competitor_id, start_number, ranking, ranking_to, point_gain, final_gain,
    is_final, completion_status, last_round, last_dance
  )
) AS s
ON t.competition_id = :competitionId AND t.competitor_id = s.competitor_id
WHEN MATCHED AND (
     t.start_number IS DISTINCT FROM s.start_number
  OR t.ranking IS DISTINCT FROM s.ranking
  OR t.ranking_to IS DISTINCT FROM s.ranking_to
  OR t.point_gain IS DISTINCT FROM s.point_gain
  OR t.final_gain IS DISTINCT FROM s.final_gain
  OR t.is_final IS DISTINCT FROM s.is_final
  OR t.completion_status IS DISTINCT FROM s.completion_status
  OR t.last_round IS DISTINCT FROM s.last_round
  OR t.last_dance IS DISTINCT FROM s.last_dance
) THEN
  UPDATE SET start_number = s.start_number,
             ranking = s.ranking,
             ranking_to = s.ranking_to,
             point_gain = s.point_gain,
             final_gain = s.final_gain,
             is_final = s.is_final,
             completion_status = s.completion_status,
             last_round = s.last_round,
             last_dance = s.last_dance
WHEN NOT MATCHED THEN
  INSERT (
    competition_id, competitor_id, start_number, ranking, ranking_to, point_gain, final_gain,
    is_final, completion_status, last_round, last_dance
  ) VALUES (
    :competitionId, s.competitor_id, s.start_number, s.ranking, s.ranking_to, s.point_gain, s.final_gain,
    s.is_final, s.completion_status, s.last_round, s.last_dance
  )
WHEN NOT MATCHED BY SOURCE
  AND t.competition_id = :competitionId THEN
  DELETE;

/* @name UpsertCompetitionRounds */
WITH input AS (
  SELECT *
  FROM unnest(
    :roundKey::text[],
    :roundLabel::text[],
    :roundIndex::int[],
    :danceProgramId::bigint[],
    :scoringMethod::federated.scoring_method[]
  ) AS input(round_key, round_label, round_index, dance_program_id, scoring_method)
), stale_rounds AS (
  SELECT cr.id
  FROM federated.competition_round cr
  WHERE cr.competition_id = :competitionId
    AND NOT EXISTS (
      SELECT 1
      FROM input
      WHERE input.round_key = cr.round_key
    )
), deleted_rounds AS (
  DELETE FROM federated.competition_round cr
  USING stale_rounds
  WHERE cr.id = stale_rounds.id
), upserted AS (
  INSERT INTO federated.competition_round (
    competition_id, round_key, round_label, round_index, dance_program_id, scoring_method
  )
  SELECT
    :competitionId, round_key, round_label, round_index, dance_program_id, scoring_method
  FROM input
  ON CONFLICT (competition_id, round_key) DO UPDATE
    SET round_label = EXCLUDED.round_label,
        round_index = EXCLUDED.round_index,
        dance_program_id = EXCLUDED.dance_program_id,
        scoring_method = EXCLUDED.scoring_method
  RETURNING id, round_key
)
SELECT id AS "id!", round_key AS "roundKey!"
FROM upserted;

/* @name ClearRoundDetails */
WITH rounds AS (
  SELECT unnest(:roundId::bigint[]) AS id
), deleted_judges AS (
  DELETE FROM federated.competition_round_judge crj
  USING rounds
  WHERE crj.round_id = rounds.id
), deleted_results AS (
  DELETE FROM federated.competition_round_result crr
  USING rounds
  WHERE crr.round_id = rounds.id
)
DELETE FROM federated.round_dance rd
USING rounds
WHERE rd.round_id = rounds.id;

/* @name InsertRoundDetails */
WITH inserted_dances AS (
  INSERT INTO federated.round_dance (round_id, dance_code, dance_order)
  SELECT round_id, dance_code, dance_order
  FROM unnest(
    :danceRoundId::bigint[],
    :danceCode::text[],
    :danceOrder::int[]
  ) AS input(round_id, dance_code, dance_order)
), inserted_judges AS (
  INSERT INTO federated.competition_round_judge (
    round_id, person_judge_id, judge_index, judge_label
  )
  SELECT round_id, person_judge_id, judge_index, nullif(judge_label, '')
  FROM unnest(
    :judgeRoundId::bigint[],
    :personJudgeId::text[],
    :judgeIndex::int[],
    :judgeLabel::text[]
  ) AS input(round_id, person_judge_id, judge_index, judge_label)
), inserted_results AS (
  INSERT INTO federated.competition_round_result (
    round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score, dance_results
  )
  SELECT
    round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score, CASE
      WHEN dance_results = '' THEN NULL
      ELSE string_to_array(dance_results, ',')::real[]
    END
  FROM unnest(
    :resultRoundId::bigint[],
    :resultCompetitorId::text[],
    :overallRanking::int[],
    :overallRankingTo::int[],
    :qualifiedNext::boolean[],
    :overallScore::numeric(10,3)[],
    :danceResults::text[]
  ) AS input(
    round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score, dance_results
  )
)
INSERT INTO federated.judge_score (
  federation, event_date, event_id, competition_id, category_id, round_id, dance_order, dance_code,
  judge_person_id, competitor_id, component, score, raw_score
)
SELECT
  federation, event_date, event_id, competition_id, category_id, round_id, dance_order, dance_code,
  judge_person_id, competitor_id, component, score, raw_score
FROM unnest(
  :scoreFederation::text[],
  :scoreEventDate::date[],
  :scoreEventId::bigint[],
  :scoreCompetitionId::bigint[],
  :scoreCategoryId::bigint[],
  :scoreRoundId::bigint[],
  :scoreDanceOrder::int[],
  :scoreDanceCode::text[],
  :scoreJudgePersonId::text[],
  :scoreCompetitorId::text[],
  :scoreComponent::federated.score_component[],
  :score::numeric(10,3)[],
  :rawScore::text[]
) AS input(
  federation, event_date, event_id, competition_id, category_id, round_id, dance_order, dance_code,
  judge_person_id, competitor_id, component, score, raw_score
);
