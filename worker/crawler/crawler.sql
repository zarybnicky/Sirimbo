/* @name GetFrontierForUpdate */
SELECT id, federation, kind, key, fetch_status, error_count, meta
FROM crawler.frontier
WHERE id = :id::bigint FOR UPDATE;

/* @name ReserveRequest */
SELECT granted, allowed_at
FROM crawler.reserve_request(:host::text, :prefixes::text[]);

/* @name MarkFrontierFetchError */
UPDATE crawler.frontier
SET last_fetched_at = now(),
    fetch_status    = 'error',
    error_count     = error_count + 1,
    next_fetch_at   = now() + least(interval '5 minutes', (2 ^ (error_count + 1)) * interval '5 second')
WHERE id = :id::bigint;

/* @name MarkFrontierFetchSuccess */
UPDATE crawler.frontier
SET last_fetched_at = now(),
    fetch_status = :fetchStatus,
    process_status = :processStatus,
    error_count = 0,
    next_fetch_at = now() + :revalidatePeriod::interval
WHERE id = :id::bigint;

/* @name RescheduleFrontier */
UPDATE crawler.frontier
SET next_fetch_at = :nextRetryAt
WHERE id = :id::bigint;

/* @name InsertHtmlResponse */
INSERT INTO crawler.html_response (frontier_id, url, http_status, error, content)
VALUES (:frontierId, :url, :httpStatus, :error, :content);

/* @name InsertJsonResponse */
INSERT INTO crawler.json_response (frontier_id, url, http_status, error, content)
VALUES (:frontierId, :url, :httpStatus, :error, :content::jsonb);

/* @name InsertDiscoveredCstsMember */
with frontier as (
  INSERT INTO crawler.frontier (federation, kind, key)
    VALUES ('csts','member', :id)
    ON CONFLICT (federation, kind, key) DO NOTHING
    RETURNING key
)
UPDATE crawler.incremental_ranges
SET last_known = GREATEST(last_known, (SELECT key::bigint FROM frontier))
FROM frontier
WHERE federation = 'csts' AND kind = 'member_id';

/* @name UpsertFederationAthlete */
WITH existing AS (
  SELECT fa.athlete_id
  FROM federated.federation_athlete fa
  WHERE fa.federation = :federation
    AND fa.external_id = :externalId::text
  FOR UPDATE
), person_ins AS (
  INSERT INTO federated.person (canonical_name, gender)
    SELECT :canonicalName, :gender::federated.gender
    WHERE NOT EXISTS (SELECT 1 FROM existing)
    RETURNING id
), athlete_ins AS (
  INSERT INTO federated.athlete (person_id)
    SELECT id
    FROM person_ins
    WHERE NOT EXISTS (SELECT 1 FROM existing)
    RETURNING id AS athlete_id
), athlete_final AS (
  SELECT athlete_id FROM existing
  UNION ALL
  SELECT athlete_id FROM athlete_ins
)
INSERT INTO federated.federation_athlete (federation, external_id, athlete_id)
SELECT :federation, :externalId, athlete_id
FROM athlete_final
ON CONFLICT (federation, external_id)
  DO UPDATE SET athlete_id = EXCLUDED.athlete_id;

/* @name UpsertCategory */
INSERT INTO federated.category (
  series,
  discipline,
  age_group,
  gender_group,
  class,
  name
)
VALUES (
  :series,
  :discipline,
  :ageGroup,
  :genderGroup,
  :class,
  :series || ' ' || :discipline || ' ' || :ageGroup || ' ' || :class
)
ON CONFLICT (series, discipline, age_group, gender_group, class)
  DO UPDATE SET name = EXCLUDED.name
RETURNING id;

/* @name UpsertFederationCouple */
WITH fc_existing AS (
  SELECT competitor_id
  FROM federated.federation_competitor
  WHERE federation = :federation
    AND external_id = :externalCompetitorId::text
    FOR UPDATE
), fc_ins AS (
  INSERT INTO federated.federation_competitor (federation, external_id)
    SELECT :federation, :externalCompetitorId::text
    WHERE NOT EXISTS (SELECT 1 FROM fc_existing)
    RETURNING competitor_id
), comp_seed AS (
  SELECT competitor_id FROM fc_existing
  UNION ALL
  SELECT competitor_id FROM fc_ins
), comp_ins AS (
  INSERT INTO federated.competitor (competitor_type, name)
    SELECT 'couple', :competitorLabel
    FROM comp_seed
    WHERE competitor_id IS NULL
    RETURNING id AS competitor_id
), comp_update AS (
  UPDATE federated.competitor
    SET name = :competitorLabel
    FROM comp_seed
    WHERE federated.competitor.id = comp_seed.competitor_id
      AND comp_seed.competitor_id IS NOT NULL
    RETURNING federated.competitor.id AS competitor_id
), comp_final AS (
  SELECT competitor_id FROM comp_update
  UNION ALL
  SELECT competitor_id FROM comp_ins
), comp_link AS (
  UPDATE federated.federation_competitor f
    SET competitor_id = c.competitor_id
    FROM comp_final c
    WHERE f.federation = :federation
      AND f.external_id = :externalCompetitorId::text
    RETURNING c.competitor_id
), ids AS (
  SELECT competitor_id FROM comp_link LIMIT 1
), comp_lead AS (
  INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)
    SELECT
      ids.competitor_id,
      :externalLeadId::bigint,
      'lead'::federated.competitor_role
    FROM ids
    ON CONFLICT (competitor_id, athlete_id) DO UPDATE SET role = EXCLUDED.role
), comp_follow AS (
  INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)
    SELECT
      ids.competitor_id,
      :externalFollowerId::bigint,
      'follow'::federated.competitor_role
    FROM ids
    ON CONFLICT (competitor_id, athlete_id) DO UPDATE SET role = EXCLUDED.role
)
SELECT competitor_id FROM ids;

/* @name UpsertFederationCoupleProgress */
INSERT INTO federated.competitor_category_progress (
  federation,
  competitor_id,
  category_id,
  points,
  domestic_finale,
  foreign_finale
)
VALUES (
  :federation,
  :federatedCompetitorId,
  :categoryId,
  :points::numeric(10, 3),
  :domesticFinale::int,
  :foreignFinale::int
)
ON CONFLICT (federation, competitor_id, category_id)
  DO UPDATE
  SET points          = EXCLUDED.points,
      domestic_finale = EXCLUDED.domestic_finale,
      foreign_finale  = EXCLUDED.foreign_finale;
