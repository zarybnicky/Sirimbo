BEGIN;
SET LOCAL search_path = public, tap;
SELECT tap.plan(10);

INSERT INTO tenant (id, name) VALUES (-8200, 'Event RLS'), (-8201, 'Other tenant');
SELECT set_config('jwt.claims.tenant_id', '-8200', true);
SELECT set_config('jwt.claims.my_person_ids', '{-8200}', true);
SELECT set_config('jwt.claims.my_tenant_ids', '{-8200}', true);
INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES (-8200, 'Trainer', 'Owner', 'unspecified', ''),
       (-8201, 'Trainer', 'Other', 'unspecified', ''),
       (-8202, 'Trainer', 'New', 'unspecified', '');
INSERT INTO cohort (id, tenant_id, name, color_rgb, is_visible) OVERRIDING SYSTEM VALUE
VALUES (-8200, -8200, 'Target cohort', '000000', true);
INSERT INTO event_instance (id, tenant_id, since, until, is_visible, is_public, is_locked)
OVERRIDING SYSTEM VALUE
VALUES (-8200, -8200, now(), now()+interval '1 hour', true, false, false),
       (-8201, -8200, now(), now()+interval '1 hour', true, false, false),
       (-8202, -8201, now(), now()+interval '1 hour', true, false, false);
INSERT INTO event_trainer (id, tenant_id, event_id, person_id) OVERRIDING SYSTEM VALUE
VALUES (-8200, -8200, -8200, -8200), (-8201, -8200, -8201, -8201),
       (-8202, -8201, -8202, -8201);
INSERT INTO event_instance_registration (id, tenant_id, instance_id, person_id, status)
OVERRIDING SYSTEM VALUE
VALUES (-8200, -8200, -8200, -8202, 'unknown'),
       (-8201, -8201, -8202, -8202, 'unknown');
INSERT INTO event_lesson_request (id, tenant_id, trainer_id, registration_id, lesson_count)
OVERRIDING SYSTEM VALUE
VALUES (-8200, -8200, -8200, -8200, 1), (-8201, -8201, -8202, -8201, 1);

SET LOCAL ROLE administrator;
SELECT tap.is((SELECT count(*) FROM event_lesson_request), 1::bigint, 'administrator lesson demands stay in tenant');
RESET ROLE;
SET LOCAL ROLE trainer;
SELECT tap.throws_ok(
  $$INSERT INTO event_trainer (event_id, person_id) VALUES (-8201,-8200)$$,
  '42501'::char(5), null, 'trainer cannot self-assign to another trainer event');
SELECT tap.throws_ok(
  $$UPDATE event_trainer SET event_id=-8201 WHERE id=-8200$$,
  '42501'::char(5), null, 'trainer cannot move an assignment onto an unauthorized event');
SELECT tap.throws_ok($$
  SELECT save_events(
    jsonb_populate_record(null::event_details_input,
      '{"name":"Blocked save","type":"lesson","capacity":5,"capacity_unit":"people"}'),
    ARRAY[ROW(-8201, now(), now()+interval '1 hour', false,
      '{}'::event_registration_input[])::event_input],
    ARRAY[ROW(-8200,2)::event_trainer_input]
  )
$$, 'P0001'::char(5), 'one or more events were not found or are not editable',
  'save_events cannot take over another trainer event');

CREATE TEMP TABLE saved_policy_event ON COMMIT DROP AS
SELECT id FROM save_events(
  jsonb_populate_record(null::event_details_input,
    '{"name":"RLS save","type":"lesson","capacity":5,"capacity_unit":"people","is_visible":true}'),
  ARRAY[ROW(null, now(), now()+interval '1 hour', false,
    ARRAY[ROW(-8202, null, null, false)::event_registration_input])::event_input],
  ARRAY[ROW(-8200,2)::event_trainer_input, ROW(-8201,3)::event_trainer_input],
  ARRAY[-8200]::bigint[]
);
SELECT tap.is((SELECT count(*) FROM saved_policy_event), 1::bigint, 'save_events creates an event as trainer');
SELECT tap.is((SELECT count(*) FROM event_trainer WHERE event_id IN (SELECT id FROM saved_policy_event)), 2::bigint, 'save_events creates multiple trainer assignments');
SELECT tap.lives_ok($$
  SELECT save_events(
    jsonb_populate_record(null::event_details_input,
      '{"name":"RLS handoff","type":"lesson","capacity":6,"capacity_unit":"people","is_visible":true}'),
    ARRAY[ROW((SELECT id FROM saved_policy_event), now(), now()+interval '2 hours', false,
      ARRAY[ROW(-8202, null, null, false)::event_registration_input])::event_input],
    ARRAY[ROW(-8201,4)::event_trainer_input, ROW(-8202,5)::event_trainer_input],
    ARRAY[-8200]::bigint[]
  )
$$, 'save_events edits and hands off ownership while retaining another trainer');
SELECT tap.is((SELECT array_agg(person_id ORDER BY person_id) FROM event_trainer WHERE event_id IN (SELECT id FROM saved_policy_event)), ARRAY[-8202,-8201]::bigint[], 'handoff removes caller and saves replacement trainers');
SELECT tap.ok((SELECT NOT app_private.can_trainer_edit_instance(id) FROM saved_policy_event), 'caller loses edit rights after handoff');
SELECT tap.is((SELECT count(*) FROM event_target_cohort WHERE event_id IN (SELECT id FROM saved_policy_event)), 1::bigint, 'cohort target survives handoff');

RESET ROLE;
SELECT * FROM tap.finish(true);
ROLLBACK;
