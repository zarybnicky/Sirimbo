CREATE or replace FUNCTION app_private.tg_cohort_membership__on_status()
  RETURNS trigger LANGUAGE plpgsql
  security definer
  SET search_path = pg_catalog, public, app_private
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
        AND (er.person_id = NEW.person_id OR (
          er.couple_id IS NOT NULL AND EXISTS (
            SELECT 1 FROM couple c WHERE c.id = er.couple_id AND NEW.person_id IN (c.man_id, c.woman_id))
          )
        )
    ),
    future_att AS (
      -- Capture future attendance rows with their pre-update status
      SELECT ea.id, ea.registration_id,
             (ea.status IN ('unknown', 'not-excused', 'cancelled')) AS will_be_cancelled
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
      -- Delete registrations where:
      --   (a) all future attendances will be cancelled (pre-update check avoids CTE
      --       visibility issue with upd), AND
      --   (b) there are no non-cancelled past/present attendance rows
      SELECT fa.registration_id
      FROM future_att fa
      GROUP BY fa.registration_id
      HAVING count(*) > 0
         AND bool_and(fa.will_be_cancelled)
         AND NOT EXISTS (
           SELECT 1 FROM event_attendance ea2
           JOIN event_instance ei2 ON ei2.id = ea2.instance_id
           WHERE ea2.registration_id = fa.registration_id
             AND ei2.since <= NEW.until
             AND ea2.status NOT IN ('unknown', 'cancelled')
         )
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

grant all on function app_private.tg_cohort_membership__on_status to trainer;
select verify_function('app_private.tg_cohort_membership__on_status', 'cohort_membership');

DROP TRIGGER IF EXISTS _500_on_status ON public.cohort_membership;
CREATE TRIGGER _500_on_status AFTER INSERT OR UPDATE ON public.cohort_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg_cohort_membership__on_status();
