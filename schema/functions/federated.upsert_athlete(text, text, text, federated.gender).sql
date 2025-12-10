CREATE FUNCTION federated.upsert_athlete(in_federation text, in_external_id text, in_canonical_name text, in_gender federated.gender) RETURNS bigint
    LANGUAGE plpgsql
    SET search_path TO 'federated', 'pg_temp'
    AS $$
DECLARE
  v_person_id  bigint;
  v_athlete_id bigint;
BEGIN
  -- Try to find existing mapping and lock it
  SELECT fa.athlete_id
  INTO v_athlete_id
  FROM federation_athlete fa
  WHERE fa.federation = in_federation
    AND fa.external_id = in_external_id
    FOR UPDATE;

  -- If no mapping, create person + athlete
  IF v_athlete_id IS NULL THEN
    INSERT INTO person (canonical_name, gender)
    VALUES (in_canonical_name, in_gender)
    RETURNING id INTO v_person_id;

    INSERT INTO athlete (person_id)
    VALUES (v_person_id)
    RETURNING id INTO v_athlete_id;
  END IF;

  -- Ensure mapping row exists / is updated
  INSERT INTO federation_athlete (federation, external_id, athlete_id)
  VALUES (in_federation, in_external_id, v_athlete_id)
  ON CONFLICT (federation, external_id)
    DO UPDATE SET athlete_id = EXCLUDED.athlete_id;

  RETURN v_athlete_id;
END;
$$;
