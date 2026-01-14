CREATE FUNCTION app_private.tg_cohort_membership__on_status() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'app_private'
    AS $$
begin
  -- Only react to status transitions (or INSERT).
  IF TG_OP <> 'INSERT' AND NEW.status IS NOT DISTINCT FROM OLD.status THEN
    RETURN NEW;
  END IF;

  if NEW.status = 'expired' then
    WITH affected AS (
      SELECT DISTINCT er.id AS registration_id
      FROM event_target_cohort etc
      JOIN event_registration er ON er.event_id = etc.event_id
      WHERE etc.cohort_id = NEW.cohort_id
        AND er.person_id = NEW.person_id
    ),
    future_att AS (
      SELECT ea.id, ea.registration_id
      FROM event_attendance ea
      JOIN event_instance ei ON ei.id = ea.instance_id
      JOIN affected a ON a.registration_id = ea.registration_id
      WHERE ei.since > NEW.until
    ),
    upd AS (
      UPDATE event_attendance ea
        SET status = 'cancelled'
        FROM future_att fa
        WHERE ea.id = fa.id
          AND ea.status IN ('unknown', 'not-excused')
        RETURNING ea.registration_id
    ),
    deletable AS (
      -- After the UPDATE, delete registrations where:
      --   (a) there exists at least one future attendance, and
      --   (b) all future attendances are cancelled.
      SELECT fa.registration_id
      FROM future_att fa
      JOIN event_attendance ea ON ea.id = fa.id
      GROUP BY fa.registration_id
      HAVING count(*) > 0
         AND bool_and(ea.status = 'cancelled')
    )
    DELETE FROM event_registration er
      USING deletable d
    WHERE er.id = d.registration_id;
  elsif NEW.status = 'active' then
    perform app_private.register_new_cohort_member_to_events(NEW);
    -- TODO: add payments
  end if;
  return NEW;
end;
$$;

GRANT ALL ON FUNCTION app_private.tg_cohort_membership__on_status() TO trainer;
