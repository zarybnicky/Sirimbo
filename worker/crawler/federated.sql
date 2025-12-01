
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
), fa_final as (
  INSERT INTO federated.federation_athlete (federation, external_id, athlete_id)
    SELECT :federation, :externalId, athlete_id
    FROM athlete_final
    ON CONFLICT (federation, external_id)
      DO UPDATE SET athlete_id = EXCLUDED.athlete_id
)
SELECT athlete_id FROM athlete_final;

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
WITH fc AS (
  -- Ensure federation_competitor row exists, get current competitor_id (may be NULL)
  INSERT INTO federated.federation_competitor (federation, external_id)
    VALUES (:federation, :externalCompetitorId::text)
    ON CONFLICT (federation, external_id)
      DO UPDATE SET external_id = EXCLUDED.external_id
    RETURNING competitor_id
), comp_ins AS (
  -- If there's no competitor yet, create one
  INSERT INTO federated.competitor (competitor_type, name)
    SELECT 'couple'::federated.competitor_type, :competitorLabel
    WHERE (SELECT competitor_id FROM fc) IS NULL
    RETURNING id AS competitor_id
), comp_final AS (
  -- Final competitor_id: existing or newly inserted
  SELECT COALESCE(
           (SELECT competitor_id FROM fc),
           (SELECT competitor_id FROM comp_ins)
         ) AS competitor_id
), comp_link AS (
  -- Link federation_competitor to the final competitor
  UPDATE federated.federation_competitor f
    SET competitor_id = c.competitor_id
    FROM comp_final c
    WHERE f.federation  = :federation
      AND f.external_id = :externalCompetitorId::text
), comp_name AS (
  -- Keep competitor name in sync with latest label
  UPDATE federated.competitor co
    SET name = :competitorLabel
    FROM comp_final c
    WHERE co.id = c.competitor_id
), comp_lead AS (
  INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)
    SELECT
      c.competitor_id,
      :federatedLeadId::bigint,
      'lead'::federated.competitor_role
    FROM comp_final c
    ON CONFLICT (competitor_id, athlete_id)
      DO UPDATE SET role = EXCLUDED.role
), comp_follow AS (
  INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)
    SELECT
      c.competitor_id,
      :federatedFollowerId::bigint,
      'follow'::federated.competitor_role
    FROM comp_final c
    ON CONFLICT (competitor_id, athlete_id)
      DO UPDATE SET role = EXCLUDED.role
)
SELECT competitor_id FROM comp_final;

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
