/* @name UpsertPeople */
INSERT INTO federated.person (federation, external_id, canonical_name, gender)
SELECT input.federation, input.external_id, input.canonical_name, input.gender
FROM unnest(
  :federation::text[],
  :externalId::text[],
  :canonicalName::text[],
  :gender::federated.gender[]
) AS input(federation, external_id, canonical_name, gender)
ON CONFLICT (federation, external_id)
  DO UPDATE SET canonical_name = EXCLUDED.canonical_name
  WHERE federated.person.canonical_name IS DISTINCT FROM EXCLUDED.canonical_name AND EXCLUDED.canonical_name <> '';

/* @name UpdatePerson */
UPDATE federated.person
SET age_group = :ageGroup,
    medical_checkup_expiration = :medicalCheckupExpiration::date
WHERE federation = :federation
  AND external_id = :externalId;

/* @name GetAllCategories */
SELECT id, series, discipline, age_group AS "ageGroup",
       gender_group AS "genderGroup", class, competitor_type AS "competitorType"
FROM federated.category;

/* @name UpsertCategory */
WITH input (series, discipline, age_group, gender_group, class, competitor_type) AS (
  VALUES (
    :series,
    :discipline,
    :ageGroup,
    :genderGroup,
    :class,
    :competitorType::federated.competitor_type
  )
),
ins AS (
  INSERT INTO federated.category (series, discipline, age_group, gender_group, class, competitor_type, name)
  SELECT *, concat_ws(' ', series, age_group, nullif(competitor_type, 'couple'), nullif(class, ''), discipline)
  FROM input
  ON CONFLICT (series, discipline, age_group, gender_group, class, competitor_type) DO NOTHING
  RETURNING id
)
SELECT id FROM ins
UNION ALL
SELECT c.id
FROM federated.category c
JOIN input i USING (series, discipline, age_group, gender_group, class, competitor_type)
WHERE NOT EXISTS (SELECT 1 FROM ins);

/* @name UpsertCompetitors */
INSERT INTO federated.competitor (federation, external_id, competitor_type, name)
SELECT input.federation, input.external_id, input.competitor_type, input.name
FROM unnest(
  :federation::text[],
  :externalId::text[],
  :type::federated.competitor_type[],
  :label::text[]
) AS input(federation, external_id, competitor_type, name)
ON CONFLICT (federation, external_id) DO UPDATE
  SET name = EXCLUDED.name
  WHERE federated.competitor.name IS DISTINCT FROM EXCLUDED.name;

/* @name MergeCompetitorComponents */
MERGE INTO federated.competitor_component AS t
USING (
  SELECT DISTINCT ON (competitor_id, person_id)
    competitor_id,
    person_id,
    role
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
  AND t.competitor_id IN (SELECT unnest(:competitorIds::text[])) THEN
  DELETE;

/* @name MergeCompetitorProgress */
MERGE INTO federated.competitor_category_progress AS t
USING (
  SELECT
    entry.competitor_id,
    entry.category_id,
    entry.points,
    entry.domestic_finals,
    entry.foreign_finals
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
),
delete_old AS (
  DELETE FROM federated.ranklist_entry WHERE snapshot_id = (SELECT id FROM upsert_snapshot)
),
insert_entries AS (
  INSERT INTO federated.ranklist_entry (snapshot_id, competitor_id, ranking, ranking_to, points)
  SELECT s.id, e.competitor_id, e.ranking, e.ranking_to, e.points
  FROM upsert_snapshot s
  CROSS JOIN jsonb_to_recordset(COALESCE(:entries, '[]'::jsonb)) AS e(
    competitor_id text,
    ranking       integer,
    ranking_to    integer,
    points        numeric(10,3)
  )
)
SELECT id FROM upsert_snapshot;

/* @name UpsertEvent */
INSERT INTO federated.event (federation, external_id, name, start_date, end_date, location, country, organizing_club_id)
VALUES (:federation, :externalId, :name, :startDate::date, :endDate::date, :location, :country, :organizingClubId)
ON CONFLICT (federation, external_id) DO UPDATE
  SET name = EXCLUDED.name,
      start_date = EXCLUDED.start_date,
      end_date = EXCLUDED.end_date,
      location = EXCLUDED.location,
      country = EXCLUDED.country,
      organizing_club_id = EXCLUDED.organizing_club_id;
