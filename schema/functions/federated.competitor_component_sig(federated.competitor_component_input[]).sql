CREATE FUNCTION federated.competitor_component_sig(in_components federated.competitor_component_input[]) RETURNS text
    LANGUAGE sql IMMUTABLE PARALLEL SAFE
    AS $$
  SELECT
    CASE
      WHEN in_components IS NULL OR cardinality(in_components) = 0 THEN NULL
      ELSE (
        SELECT jsonb_agg(jsonb_build_array(athlete_id, role::text) ORDER BY athlete_id, role::text)::text
        FROM (
          SELECT DISTINCT ON (athlete_id) athlete_id, role
          FROM unnest(in_components) AS c (athlete_id, role)
          ORDER BY athlete_id, role
        ) t
      )
  END
$$;
