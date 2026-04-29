
/* @name UpsertPerson */
INSERT INTO federated.person AS person (federation, external_id, canonical_name, gender)
VALUES (:federation, :externalId, :canonicalName, :gender)
ON CONFLICT (federation, external_id)
  DO UPDATE SET canonical_name = EXCLUDED.canonical_name
  WHERE person.canonical_name <> EXCLUDED.canonical_name;

/* @name UpdatePerson */
UPDATE federated.person
SET age_group = :ageGroup,
    medical_checkup_expiration = :medicalCheckupExpiration::date
WHERE federation = :federation
  AND external_id = :externalId;

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

/* @name UpsertCompetitor */
SELECT federated.upsert_competitor(
  in_federation => :federation,
  in_external_id => :externalId,
  in_type => :type::federated.competitor_type,
  in_label => :label,
  in_components => ARRAY(
    SELECT (u.person_id, u.role)::federated.competitor_component_input
    FROM unnest(
      :component_person_ids::text[],
      :component_roles::federated.competitor_role[]
    ) AS u(person_id, role)
  )
) as competitor_id;

/* @name UpsertManyCompetitors */
INSERT INTO federated.competitor (federation, external_id, competitor_type, name)
SELECT input.federation, input.external_id, input.competitor_type, input.name
FROM unnest(
  :federations::text[],
  :external_ids::text[],
  :types::federated.competitor_type[],
  :labels::text[]
) AS input(federation, external_id, competitor_type, name)
ON CONFLICT DO NOTHING;

/* @name ReplaceCompetitorProgress */
SELECT federated.replace_competitor_category_progress(
  in_competitor_id => :competitorId,
  in_entries       => ARRAY(
    SELECT (
      u.category_id,
      u.points,
      u.domestic_finale,
      u.foreign_finale
    )::federated.competitor_category_progress_input
    FROM unnest(
      :category_ids::bigint[],
      :points::numeric(10,3)[],
      :domestic_finales::int[],
      :foreign_finales::int[]
    ) AS u(category_id, points, domestic_finale, foreign_finale)
  )
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
