CREATE FUNCTION federated.upsert_ranklist_snapshot(in_federation text, in_category_id bigint, in_ranklist_name text, in_as_of_date date, in_kind text DEFAULT 'default'::text, in_entries jsonb DEFAULT '[]'::jsonb) RETURNS bigint
    LANGUAGE plpgsql
    SET search_path TO 'federated', 'pg_temp'
    AS $$
DECLARE
  v_ranklist_id  bigint;
  v_snapshot_id  bigint;
BEGIN
  INSERT INTO federated.ranklist (federation, category_id, name)
  VALUES (in_federation, in_category_id, in_ranklist_name)
  ON CONFLICT (federation, category_id)
    DO UPDATE SET name = EXCLUDED.name
  RETURNING id INTO v_ranklist_id;

  -- 2) upsert snapshot
  INSERT INTO federated.ranklist_snapshot (ranklist_id, as_of_date, kind)
  VALUES (v_ranklist_id, in_as_of_date, COALESCE(in_kind, 'default'))
  ON CONFLICT (ranklist_id, as_of_date, kind)
    DO UPDATE SET kind = EXCLUDED.kind
  RETURNING id INTO v_snapshot_id;

  -- 3) replace entries for this snapshot (so removals are reflected too)
  DELETE FROM federated.ranklist_entry
  WHERE snapshot_id = v_snapshot_id;

  INSERT INTO federated.ranklist_entry (
    snapshot_id,
    competitor_id,
    ranking,
    ranking_to,
    points
  )
  SELECT
    v_snapshot_id,
    e.competitor_id,
    e.ranking,
    e.ranking_to,
    e.points
  FROM jsonb_to_recordset(COALESCE(in_entries, '[]'::jsonb)) AS e(
    competitor_id bigint,
    ranking integer,
    ranking_to integer,
    points numeric(10,3)
  );

  RETURN v_snapshot_id;
END;
$$;
