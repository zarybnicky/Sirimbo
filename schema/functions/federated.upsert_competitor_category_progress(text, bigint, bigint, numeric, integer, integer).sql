CREATE FUNCTION federated.upsert_competitor_category_progress(in_federation text, in_competitor_id bigint, in_category_id bigint, in_points numeric, in_domestic_finale integer, in_foreign_finale integer) RETURNS void
    LANGUAGE sql
    SET search_path TO 'federated', 'pg_temp'
    AS $$
  INSERT INTO federated.competitor_category_progress (
    federation,
    competitor_id,
    category_id,
    points,
    domestic_finale,
    foreign_finale
  )
  VALUES (
    in_federation,
    in_competitor_id,
    in_category_id,
    in_points,
    in_domestic_finale,
    in_foreign_finale
  )
  ON CONFLICT (federation, competitor_id, category_id)
    DO UPDATE
    SET points          = EXCLUDED.points,
        domestic_finale = EXCLUDED.domestic_finale,
        foreign_finale  = EXCLUDED.foreign_finale;
$$;
