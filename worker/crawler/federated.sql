
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
  in_class        => :class
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
