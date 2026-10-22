BEGIN;
SET LOCAL search_path = public, tap;
SELECT tap.no_plan();

INSERT INTO tenant (id, name) VALUES (-9000, 'Camp host'), (-9001, 'Home club');
SELECT set_config('jwt.claims.tenant_id', '-9000', true);
SELECT set_config('jwt.claims.my_person_ids', '{-9000}', true);
SELECT set_config('jwt.claims.my_tenant_ids', '{}', true);
SELECT set_config('jwt.claims.shared.event_ids', '{}', true);
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
INSERT INTO couple (id, man_id, woman_id, since) OVERRIDING SYSTEM VALUE
VALUES (-9000, -9000, -9001, now() - interval '1 year'),
       (-9001, -9000, -9005, now() - interval '2 years');
INSERT INTO event_instance_registration (id, instance_id, couple_id) OVERRIDING SYSTEM VALUE
VALUES (-9006, -9000, -9000);
UPDATE event_instance_registration SET parent_registration_id=-9006 WHERE id IN (-9000, -9001);
INSERT INTO event_trainer (id, tenant_id, event_id, person_id) OVERRIDING SYSTEM VALUE
VALUES (-9000, -9000, -9000, -9003), (-9001, -9000, -9002, -9003);
INSERT INTO cohort (id, tenant_id, name, color_rgb, is_visible) OVERRIDING SYSTEM VALUE
VALUES (-9000, -9000, 'Hidden target', '000000', false),
       (-9001, -9000, 'Unrelated hidden cohort', '000000', false);
INSERT INTO event_target_cohort (tenant_id, event_id, cohort_id)
VALUES (-9000, -9002, -9000);
INSERT INTO event_lesson_request (id, tenant_id, trainer_id, registration_id, lesson_count)
OVERRIDING SYSTEM VALUE VALUES (-9000, -9000, -9000, -9001, 1);

SELECT tap.ok(NOT has_table_privilege('anonymous', 'app_private.event_people', 'SELECT'),
  'callers cannot read the unscoped person-pair view');
UPDATE event_instance SET share_token='test-camp-share' WHERE id=-9000;
UPDATE event_instance_registration SET registration_status='cancelled' WHERE id=-9001;
SELECT tap.is(
  (SELECT array_agg(id ORDER BY id) FROM app_private.event_share_claims(-9000, 'test-camp-share') s,
    unnest(s.instance_ids) id), ARRAY[-9002,-9001,-9000]::bigint[],
  'sharing includes all descendants, not unrelated or foreign events');
SELECT tap.is((SELECT count(*) FROM app_private.event_share_claims(-9001, 'test-camp-share')),
  0::bigint, 'a share token does not work in another tenant');
SELECT set_config('jwt.claims.shared.event_ids', instance_ids::text, true)
FROM app_private.event_share_claims(-9000, 'test-camp-share');
SELECT set_config('jwt.claims.my_person_ids', '{}', true);
SET LOCAL ROLE anonymous;
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM person), ARRAY[-9003,-9002,-9000]::bigint[],
  'sharing includes active person registrations and trainers, not cancelled partners');
SELECT tap.is((SELECT array_agg(id) FROM couple), ARRAY[-9000]::bigint[],
  'sharing exposes the registered couple, not other partnerships of its attendees');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM event_instance),
  ARRAY[-9002,-9001,-9000]::bigint[], 'share-only access includes the full event tree');
SELECT tap.is((SELECT array_agg(id) FROM cohort), ARRAY[-9000]::bigint[],
  'share-only access derives the grandchild cohort without a cohort claim');
SELECT tap.is((SELECT count(*) FROM event_trainer), 2::bigint,
  'share-only access includes trainer assignments');
SELECT tap.is((SELECT count(*) FROM event_target_cohort), 1::bigint,
  'share-only access includes target assignments');
SELECT set_config('jwt.claims.tenant_id', '-9001', true);
SELECT tap.is((SELECT count(*) FROM app_private.accessible_event_ids()), 0::bigint,
  'shared event IDs cannot grant access in another tenant');
SELECT tap.is((SELECT count(*) FROM person WHERE id BETWEEN -9005 AND -9000), 0::bigint,
  'shared event IDs cannot expose people in another tenant');
RESET ROLE;
SELECT set_config('jwt.claims.tenant_id', '-9000', true);
SELECT set_config('jwt.claims.shared.event_ids', '{}', true);
SET LOCAL ROLE administrator;
SELECT tap.is((SELECT count(*) FROM person WHERE id=-9001), 1::bigint,
  'admins need neither a registration nor a share link to see participants');
RESET ROLE;
UPDATE event_instance_registration SET registration_status='active' WHERE id=-9001;
SELECT set_config('jwt.claims.my_person_ids', '{-9000}', true);
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
    ), event_trainer AS (
      SELECT * FROM public.event_trainer WHERE tenant_id IN (-9000, -9001)
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
SELECT tap.is((SELECT count(*) FROM event_instance_registration), 5::bigint,
  'attendees see all registrations of accessible events, including cancellation history');
SELECT tap.is((SELECT count(*) FROM event_trainer), 2::bigint, 'attendees see trainers throughout the schedule');
SELECT tap.is((SELECT count(*) FROM event_target_cohort), 1::bigint, 'attendees see target assignments');
SELECT tap.is((SELECT array_agg(id) FROM cohort), ARRAY[-9000]::bigint[], 'attendees see the referenced hidden cohort only');
SELECT tap.is((SELECT count(*) FROM event_lesson_request), 1::bigint, 'attendees see another participant lesson request');
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
RESET ROLE;
UPDATE event_instance_registration SET registration_status='cancelled' WHERE id=-9000;
SET LOCAL ROLE anonymous;
SELECT tap.is((SELECT count(*) FROM event_instance), 0::bigint, 'cancellation removes private-event access immediately');
SELECT tap.is((SELECT count(*) FROM event_instance_registration), 0::bigint, 'cancellation removes registration reads immediately');
SELECT tap.is((SELECT count(*) FROM event_lesson_request), 0::bigint, 'cancellation removes lesson-demand reads immediately');
RESET ROLE;
REFRESH MATERIALIZED VIEW app_private.event_people;
SET LOCAL ROLE anonymous;
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM person WHERE id BETWEEN -9005 AND -9000),
  ARRAY[-9000]::bigint[], 'refresh removes former attendee access while preserving access to own person');
RESET ROLE;

SELECT * FROM tap.finish(true);
ROLLBACK;
