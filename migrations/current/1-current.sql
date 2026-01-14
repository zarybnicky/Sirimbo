comment on function person_account is '@omit';
comment on function tenant_account is '@omit';

--!include functions/tg_cohort_membership__on_status.sql

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
