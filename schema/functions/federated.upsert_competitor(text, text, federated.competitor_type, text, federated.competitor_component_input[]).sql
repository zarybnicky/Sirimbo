CREATE FUNCTION federated.upsert_competitor(in_federation text, in_external_id text, in_type federated.competitor_type, in_label text, in_components federated.competitor_component_input[]) RETURNS bigint
    LANGUAGE plpgsql
    SET search_path TO 'federated', 'pg_temp'
    AS $$
DECLARE
  v_competitor_id bigint;
BEGIN
  -- 1) Try existing mapping by federation + external_id
  IF in_external_id IS NOT NULL THEN
    SELECT competitor_id
    INTO v_competitor_id
    FROM federation_competitor
    WHERE federation = in_federation
      AND external_id = in_external_id
      FOR UPDATE;

    -- If competitor already mapped, we just reuse it and fix components below
    IF v_competitor_id IS NOT NULL THEN
      -- keep name in sync
      UPDATE competitor
      SET name = in_label
      WHERE id = v_competitor_id;

      -- upsert components
      INSERT INTO competitor_component (competitor_id, athlete_id, role)
      SELECT v_competitor_id, (c).athlete_id, (c).role
      FROM unnest(in_components) AS c
      ON CONFLICT (competitor_id, athlete_id)
        DO UPDATE SET role = EXCLUDED.role;

      RETURN v_competitor_id;
    END IF;
  END IF;

  -- 2) Try to find competitor by exact component set
  WITH input AS (
    SELECT (c).athlete_id AS athlete_id, (c).role AS role
    FROM unnest(in_components) AS c
  ), candidates AS (
    SELECT cc.competitor_id
    FROM competitor_component cc
    GROUP BY cc.competitor_id
    HAVING count(*) = (SELECT count(*) FROM input)
       AND bool_and(EXISTS(SELECT 1 FROM input i WHERE i.athlete_id = cc.athlete_id AND i.role = cc.role))
  )
  SELECT competitor_id
  INTO v_competitor_id
  FROM candidates
  LIMIT 1;

  -- 3) If no competitor found, create one
  IF v_competitor_id IS NULL THEN
    INSERT INTO competitor (competitor_type, name)
    VALUES (in_type, in_label)
    RETURNING id INTO v_competitor_id;

    INSERT INTO competitor_component (competitor_id, athlete_id, role)
    SELECT v_competitor_id, (c).athlete_id, (c).role
    FROM unnest(in_components) AS c;
  END IF;

  -- 4) Link federation_competitor if external_id present
  IF in_external_id IS NOT NULL THEN
    INSERT INTO federation_competitor (federation, external_id, competitor_id)
    VALUES (in_federation, in_external_id, v_competitor_id)
    ON CONFLICT (federation, external_id)
      DO UPDATE SET competitor_id = EXCLUDED.competitor_id;
  END IF;

  RETURN v_competitor_id;
END;
$$;
