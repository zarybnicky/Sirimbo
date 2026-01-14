--! Previous: sha1:c04523ddc34020b2ae8b57f5f0df0860369d6af0
--! Hash: sha1:01412c05ba386c5e2b7167a4f55b941400ee7bd8

--! split: 1-current.sql
comment on function person_account is '@omit';
comment on function tenant_account is '@omit';

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

grant all on function app_private.tg_cohort_membership__on_status to trainer;
select verify_function('app_private.tg_cohort_membership__on_status', 'cohort_membership');

DROP TRIGGER IF EXISTS _500_on_status ON public.cohort_membership;
CREATE TRIGGER _500_on_status AFTER INSERT OR UPDATE ON public.cohort_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg_cohort_membership__on_status();


WITH ended_cm AS (
  SELECT
    cm.id        AS cohort_membership_id,
    cm.tenant_id,
    cm.person_id,
    cm.cohort_id,
    cm.until
  FROM cohort_membership cm
  WHERE cm.status = 'expired'
),
     affected_reg AS (
       SELECT DISTINCT
         ecm.tenant_id,
         ecm.until,
         er.id AS registration_id
       FROM ended_cm ecm
       JOIN event_target_cohort etc ON etc.tenant_id = ecm.tenant_id AND etc.cohort_id = ecm.cohort_id
       JOIN event_registration er ON er.tenant_id  = ecm.tenant_id AND er.event_id   = etc.event_id AND er.person_id  = ecm.person_id
     ),
     future_att AS (
       SELECT
         ar.tenant_id,
         ar.registration_id,
         ea.id AS attendance_id
       FROM affected_reg ar
       JOIN event_attendance ea ON ea.tenant_id = ar.tenant_id AND ea.registration_id = ar.registration_id
       JOIN event_instance ei ON ei.tenant_id = ar.tenant_id AND ei.id = ea.instance_id
       WHERE ei.since > ar.until
     ),
     upd AS (
       UPDATE event_attendance ea
         SET status = 'cancelled'
         FROM future_att fa
         WHERE ea.tenant_id = fa.tenant_id AND ea.id = fa.attendance_id AND ea.status IN ('unknown', 'not-excused')
         RETURNING ea.tenant_id, ea.registration_id, ea.id
     ),
     deletable AS (
       SELECT fa.tenant_id, fa.registration_id
       FROM future_att fa JOIN event_attendance ea ON ea.tenant_id = fa.tenant_id AND ea.id = fa.attendance_id
       GROUP BY fa.tenant_id, fa.registration_id
       HAVING count(*) > 0
          AND bool_and(ea.status = 'cancelled')
     ),
     del AS (
       DELETE FROM event_registration er
         USING deletable d
         WHERE er.tenant_id = d.tenant_id
           AND er.id = d.registration_id
         RETURNING er.tenant_id, er.id
     )
SELECT
  (SELECT count(*) FROM upd) AS attendances_cancelled,
  (SELECT count(*) FROM del) AS registrations_deleted;
