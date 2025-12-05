DROP EXTENSION IF EXISTS plpgsql_check;
CREATE EXTENSION plpgsql_check;

DELETE FROM event_instance_trainer AS et1
WHERE EXISTS (
  SELECT 1
  FROM event_instance_trainer AS et2
  WHERE (et1.instance_id, et1.person_id) = (et2.instance_id, et2.person_id)
    AND et1.id < et2.id);

ALTER TABLE public.event_instance_trainer
  DROP CONSTRAINT IF EXISTS event_instance_trainer_trainer_id_key,
  ADD CONSTRAINT event_instance_trainer_trainer_id_key UNIQUE (instance_id, person_id);

DELETE FROM event_target_cohort AS et1
WHERE EXISTS (
  SELECT 1
  FROM event_target_cohort AS et2
  WHERE (et1.event_id, et1.cohort_id) = (et2.event_id, et2.event_id)
    AND et1.id < et2.id);

ALTER TABLE public.event_target_cohort
  DROP CONSTRAINT IF EXISTS event_target_cohort_cohort_id_key,
  ADD CONSTRAINT event_target_cohort_cohort_id_key UNIQUE (event_id, cohort_id);

--!include functions/upsert_event.sql
