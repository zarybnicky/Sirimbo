
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
  in_components => (SELECT array_agg(x::federated.competitor_component_input) FROM jsonb_to_recordset(COALESCE(:components::text::jsonb, '[]'::jsonb)) as x(
    athlete_id bigint,
    role federated.competitor_role
  ))
) as competitor_id;

/*
 @name UpsertManyCompetitors
 @param competitors -> ((federation, federationCompetitorId, type, label)...)
*/
SELECT
  federation_id,
  federated.upsert_competitor(
    in_federation => federation,
    in_external_id => federation_id,
    in_type => type::federated.competitor_type,
    in_label => label,
    in_components => '{}'::federated.competitor_component_input[]
  ) as federated_id
FROM (VALUES :competitors) as t(federation, federation_id, type, label);

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
