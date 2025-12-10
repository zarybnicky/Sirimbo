CREATE FUNCTION federated.upsert_category(in_series text, in_discipline text, in_age_group text, in_gender_group text, in_class text, in_name text DEFAULT NULL::text) RETURNS bigint
    LANGUAGE sql
    SET search_path TO 'federated', 'pg_temp'
    AS $$
  INSERT INTO federated.category (
    series,
    discipline,
    age_group,
    gender_group,
    class,
    name
  )
  VALUES (
    in_series,
    in_discipline,
    in_age_group,
    in_gender_group,
    in_class,
    COALESCE(
      in_name,
      concat_ws(' ', in_series, in_discipline, in_age_group, in_class)
    )
  )
  ON CONFLICT (series, discipline, age_group, gender_group, class)
    DO UPDATE SET name = EXCLUDED.name
RETURNING id;
$$;
