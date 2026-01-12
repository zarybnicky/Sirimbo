CREATE FUNCTION federated.upsert_competitor(in_federation text, in_external_id text, in_type federated.competitor_type, in_label text, in_components federated.competitor_component_input[]) RETURNS bigint
    LANGUAGE plpgsql
    SET search_path TO 'federated', 'pg_temp'
    AS $$
DECLARE
  v_competitor_id bigint;
  v_sig text;
  v_competitor_id_by_sig bigint;
BEGIN
  if in_federation is null or in_external_id is null or in_type is null then
    raise exception 'Missing argument to upsert_competitor';
  end if;

  -- Ensure the mapping row exists and lock it (even if we don't have a competitor yet).
  INSERT INTO federated.federation_competitor (federation, external_id, competitor_id)
  VALUES (in_federation, in_external_id, NULL)
  ON CONFLICT (federation, external_id) DO NOTHING;

  SELECT competitor_id
  INTO v_competitor_id
  FROM federated.federation_competitor
  WHERE federation = in_federation
    AND external_id = in_external_id
    FOR UPDATE;

  -- signature is null if in_components is null or empty
  v_sig = federated.competitor_component_sig(in_components);

  IF v_sig IS NOT NULL THEN
    SELECT c.id
    INTO v_competitor_id_by_sig
    FROM federated.competitor c
    WHERE c.competitor_type = in_type
      AND c.component_sig = v_sig;

    -- if mapping exists but points elsewhere, merge old -> canonical
    IF v_competitor_id_by_sig IS NOT NULL AND v_competitor_id IS NOT NULL AND v_competitor_id_by_sig <> v_competitor_id THEN
      PERFORM federated.merge_competitor(v_competitor_id, v_competitor_id_by_sig);
      v_competitor_id := v_competitor_id_by_sig;
    END IF;

    -- if mapping is missing, reuse link
    IF v_competitor_id IS NULL AND v_competitor_id_by_sig IS NOT NULL THEN
      v_competitor_id := v_competitor_id_by_sig;
    END IF;
  END IF;

  -- create if still missing (dedupe by unique (type,sig) when sig known)
  IF v_competitor_id IS NULL THEN
    IF v_sig IS NOT NULL THEN
      INSERT INTO federated.competitor (competitor_type, name, component_sig)
      VALUES (in_type, in_label, v_sig)
      ON CONFLICT (competitor_type, component_sig)
        DO UPDATE SET name = EXCLUDED.name
      RETURNING id INTO v_competitor_id;
    ELSE
      INSERT INTO federated.competitor (competitor_type, name)
      VALUES (in_type, in_label)
      RETURNING id INTO v_competitor_id;
    END IF;
  END IF;

  -- re-label if necessary
  if in_label is not null then
    UPDATE federated.competitor SET name = in_label WHERE id = v_competitor_id AND name IS DISTINCT FROM in_label;
  end if;

  -- if in_components provided and differ from existing ones, update them
  IF v_sig IS NOT NULL AND v_competitor_id_by_sig IS NULL THEN
    WITH src AS MATERIALIZED (
      SELECT
        (c).athlete_id AS athlete_id,
        min((c).role)  AS role
      FROM unnest(in_components) AS c
      GROUP BY 1
    ), upserted AS (
      INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)
        SELECT v_competitor_id, s.athlete_id, s.role
        FROM src s
        ON CONFLICT (competitor_id, athlete_id) DO UPDATE SET role = EXCLUDED.role
        WHERE federated.competitor_component.role IS DISTINCT FROM EXCLUDED.role
        RETURNING 1
    ), deleted AS (
      DELETE FROM federated.competitor_component t
        WHERE t.competitor_id = v_competitor_id
          AND NOT EXISTS (SELECT 1 FROM src s WHERE s.athlete_id = t.athlete_id)
             RETURNING 1
    )
    UPDATE federated.competitor c
    SET component_sig = v_sig
    WHERE c.id = v_competitor_id
      AND (SELECT count(*) FROM upserted) >= 0
      AND (SELECT count(*) FROM deleted)  >= 0;
  END IF;

  -- Update the already-locked mapping row (cheap).
  UPDATE federated.federation_competitor
  SET competitor_id = v_competitor_id
  WHERE federation = in_federation
    AND external_id = in_external_id
    AND competitor_id IS DISTINCT FROM v_competitor_id;

  RETURN v_competitor_id;
END;
$$;
