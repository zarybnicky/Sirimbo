CREATE FUNCTION federated.upsert_athlete(in_federation text, in_external_id text, in_canonical_name text, in_gender federated.gender) RETURNS bigint
    LANGUAGE plpgsql
    SET search_path TO 'federated', 'pg_temp'
    AS $$
DECLARE
  v_person_id  bigint;
  v_athlete_id bigint;
BEGIN
  -- Ensure the mapping row exists and lock it (even if athlete_id is NULL).
  INSERT INTO federated.federation_athlete (federation, external_id, athlete_id)
  VALUES (in_federation, in_external_id, NULL)
  ON CONFLICT (federation, external_id) DO NOTHING;

  SELECT athlete_id
  INTO v_athlete_id
  FROM federated.federation_athlete
  WHERE federation=in_federation AND external_id=in_external_id
    FOR UPDATE;

  -- If no mapping, create person + athlete
  IF v_athlete_id IS NULL THEN
    INSERT INTO person (canonical_name, gender)
    VALUES (in_canonical_name, in_gender)
    RETURNING id INTO v_person_id;

    INSERT INTO athlete (person_id)
    VALUES (v_person_id)
    RETURNING id INTO v_athlete_id;

    UPDATE federated.federation_athlete
    SET athlete_id = v_athlete_id
    WHERE federation = in_federation
      AND external_id = in_external_id;
  END IF;

  RETURN v_athlete_id;
END;
$$;
