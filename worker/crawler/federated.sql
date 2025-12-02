
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

/* @name UpsertCompetitor */
SELECT federated.upsert_competitor(
  in_federation => :federation,
  in_external_id => :federationCompetitorId,
  in_type => :type::federated.competitor_type,
  in_label => :label,
  in_components => (select array_agg(x) from json_populate_recordset(null::federated.competitor_component_input, array_to_json(:components::json[])) x)
) as competitor_id;

/* @name UpsertCompetitorProgress */
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
  :competitorId,
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
