BEGIN;
CREATE SCHEMA IF NOT EXISTS tap;
CREATE EXTENSION IF NOT EXISTS pgtap SCHEMA tap;
SET LOCAL search_path = public, tap;
GRANT USAGE ON SCHEMA tap TO anonymous;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA tap TO anonymous;
SELECT tap.no_plan();

INSERT INTO tenant (id, name) VALUES (-9000, 'Camp host'), (-9001, 'Home club');
SELECT set_config('jwt.claims.tenant_id', '-9000', true);
SELECT set_config('jwt.claims.my_person_ids', '{-9000}', true);
SELECT set_config('jwt.claims.my_tenant_ids', '{}', true);
SELECT set_config('jwt.claims.shared.event_ids', '{}', true);
SELECT set_config('jwt.claims.shared.cohort_ids', '{}', true);
INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES (-9000, 'Visitor', 'Camp', 'unspecified', ''),
       (-9001, 'Peer', 'Camp', 'unspecified', ''),
       (-9002, 'Child', 'Camp', 'unspecified', ''),
       (-9003, 'Trainer', 'Camp', 'unspecified', ''),
       (-9004, 'Cancelled', 'Camp', 'unspecified', ''),
       (-9005, 'Foreign', 'Camp', 'unspecified', '');
INSERT INTO event_instance (id, tenant_id, parent_id, since, until, is_visible, is_public)
OVERRIDING SYSTEM VALUE
VALUES (-9000, -9000, null, now(), now()+interval '1 day', false, false),
       (-9001, -9000, -9000, now(), now()+interval '1 day', false, false),
       (-9002, -9000, -9001, now(), now()+interval '1 day', false, false),
       (-9003, -9000, null, now(), now()+interval '1 day', false, false),
       (-9004, -9001, null, now(), now()+interval '1 day', false, false);
INSERT INTO event_instance_registration (id, tenant_id, instance_id, person_id, status, registration_status)
OVERRIDING SYSTEM VALUE
VALUES (-9000, -9000, -9000, -9000, 'unknown', 'active'),
       (-9001, -9000, -9000, -9001, 'unknown', 'active'),
       (-9002, -9000, -9001, -9002, 'unknown', 'active'),
       (-9003, -9000, -9000, -9004, 'unknown', 'cancelled'),
       (-9004, -9001, -9004, -9000, 'unknown', 'active'),
       (-9005, -9001, -9004, -9005, 'unknown', 'active');
INSERT INTO event_instance_trainer (id, tenant_id, instance_id, person_id) OVERRIDING SYSTEM VALUE
VALUES (-9000, -9000, -9000, -9003), (-9001, -9000, -9002, -9003);
INSERT INTO cohort (id, tenant_id, name, color_rgb, is_visible) OVERRIDING SYSTEM VALUE
VALUES (-9000, -9000, 'Hidden target', '000000', false),
       (-9001, -9000, 'Unrelated hidden cohort', '000000', false);
INSERT INTO event_instance_target_cohort (tenant_id, instance_id, cohort_id)
VALUES (-9000, -9002, -9000);
INSERT INTO event_lesson_demand (id, tenant_id, trainer_id, registration_id, lesson_count)
OVERRIDING SYSTEM VALUE VALUES (-9000, -9000, -9000, -9001, 1);

SELECT tap.is((SELECT count(*) FROM graphile_worker.jobs WHERE key='refresh_event_people'),
  1::bigint, 'bulk changes share one pending refresh key');
SELECT tap.ok(NOT has_table_privilege('anonymous', 'app_private.event_people', 'SELECT'),
  'callers cannot read the unscoped person-pair view');
-- Run the installed query on test data only; ROLLBACK restores the original view.
DO $$
DECLARE
  definition text := rtrim(pg_get_viewdef('app_private.event_people'::regclass), ';');
BEGIN
  DROP MATERIALIZED VIEW app_private.event_people;
  EXECUTE format($query$
    CREATE MATERIALIZED VIEW app_private.event_people AS
    WITH event_instance_registration AS (
      SELECT * FROM public.event_instance_registration WHERE tenant_id IN (-9000, -9001)
    ), event_instance_trainer AS (
      SELECT * FROM public.event_instance_trainer WHERE tenant_id IN (-9000, -9001)
    ), event_instance AS (
      SELECT * FROM public.event_instance WHERE tenant_id IN (-9000, -9001)
    )
    SELECT * FROM (%s) people
  $query$, definition);
END;
$$;

SET LOCAL ROLE anonymous;
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM event_instance),
  ARRAY[-9002,-9001,-9000]::bigint[], 'a nonmember sees the registered event, child, and grandchild only');
SELECT tap.is((SELECT count(*) FROM event_instance_registration), 4::bigint,
  'attendees see all registrations of accessible events, including cancellation history');
SELECT tap.is((SELECT count(*) FROM event_instance_trainer), 2::bigint, 'attendees see trainers throughout the schedule');
SELECT tap.is((SELECT count(*) FROM event_instance_target_cohort), 1::bigint, 'attendees see target assignments');
SELECT tap.is((SELECT array_agg(id) FROM cohort), ARRAY[-9000]::bigint[], 'attendees see the referenced hidden cohort only');
SELECT tap.is((SELECT count(*) FROM event_lesson_demand), 1::bigint, 'attendees see another participant lesson request');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM person), ARRAY[-9003,-9002,-9001,-9000]::bigint[],
  'people include other attendees, child attendees, and trainers, not cancelled or foreign attendees');
WITH changed AS (UPDATE event_instance SET name='Forbidden' WHERE id=-9000 RETURNING id)
SELECT tap.is(count(*), 0::bigint, 'attendee access does not grant event updates') FROM changed;
WITH changed AS (UPDATE person SET first_name='Forbidden' WHERE id=-9001 RETURNING id)
SELECT tap.is(count(*), 0::bigint, 'attendee access does not grant peer updates') FROM changed;

SELECT set_config('jwt.claims.my_person_ids', '{-9001}', true);
SELECT tap.is((SELECT count(*) FROM person WHERE id=-9000), 1::bigint, 'co-attendee person visibility works in both directions');
SELECT set_config('jwt.claims.my_person_ids', '{-9002}', true);
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM event_instance), ARRAY[-9002,-9001]::bigint[],
  'registration on a child grants descendants, not its parent or siblings');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM person), ARRAY[-9003,-9002]::bigint[],
  'a child attendee sees descendant trainers, not parent attendees');
SELECT set_config('jwt.claims.my_person_ids', '{}', true);
SELECT tap.is((SELECT count(*) FROM event_instance), 0::bigint, 'no person claims give no private-event access');
SELECT tap.is((SELECT count(*) FROM person WHERE id BETWEEN -9005 AND -9000), 0::bigint, 'no person claims give no cached attendee access');
SELECT set_config('jwt.claims.my_person_ids', '{-9000}', true);
SELECT set_config('jwt.claims.tenant_id', '-9001', true);
SELECT tap.is((SELECT array_agg(id) FROM event_instance), ARRAY[-9004]::bigint[], 'event access remains scoped to the request tenant');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM person), ARRAY[-9005,-9000]::bigint[], 'person-pair access remains scoped to the request tenant');

RESET ROLE;
SELECT set_config('jwt.claims.tenant_id', '-9000', true);
UPDATE event_instance SET parent_id=null WHERE id=-9001;
SET LOCAL ROLE anonymous;
SELECT tap.is((SELECT array_agg(id) FROM event_instance), ARRAY[-9000]::bigint[], 'reparenting changes schedule access immediately');
SELECT tap.is((SELECT count(*) FROM person WHERE id=-9002), 1::bigint,
  'reparenting retains cached person visibility until refresh');
RESET ROLE;
UPDATE event_instance_registration SET registration_status='cancelled' WHERE id=-9000;
SET LOCAL ROLE anonymous;
SELECT tap.is((SELECT count(*) FROM event_instance), 0::bigint, 'cancellation removes private-event access immediately');
SELECT tap.is((SELECT count(*) FROM event_instance_registration), 0::bigint, 'cancellation removes registration reads immediately');
SELECT tap.is((SELECT count(*) FROM event_lesson_demand), 0::bigint, 'cancellation removes lesson-demand reads immediately');
SELECT tap.is((SELECT count(*) FROM person WHERE id=-9001), 1::bigint, 'person visibility retains its documented delay until refresh');
RESET ROLE;

SELECT * FROM tap.finish(true);
ROLLBACK;
