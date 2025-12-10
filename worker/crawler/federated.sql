
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
  in_components => (
    select array_agg(x)
    from json_populate_recordset(
      null::federated.competitor_component_input,
      array_to_json(:components::json[])
    ) x
  )
) as competitor_id;

/* @name UpsertCompetitorProgress */
SELECT federated.upsert_competitor_category_progress(
  in_federation      => :federation,
  in_competitor_id   => :competitorId,
  in_category_id     => :categoryId,
  in_points          => :points::numeric(10,3),
  in_domestic_finale => :domesticFinale::int,
  in_foreign_finale  => :foreignFinale::int
);
