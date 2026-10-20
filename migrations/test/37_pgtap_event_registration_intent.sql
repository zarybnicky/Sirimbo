BEGIN;
SET LOCAL search_path = public, tap;
SELECT tap.no_plan();

INSERT INTO tenant (id, name) VALUES (-8600, 'Registration intent tests');
SELECT set_config('jwt.claims.tenant_id', '-8600', true);
INSERT INTO users (id, u_email, u_pass)
VALUES (-8650, 'registration-creator@test.invalid', 'test-password');
INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES (-8601, 'Alice', 'Intent', 'unspecified', ''),
       (-8602, 'Bob', 'Intent', 'unspecified', ''),
       (-8603, 'Charlie', 'Intent', 'unspecified', ''),
       (-8604, 'Trainer', 'Intent', 'unspecified', '');
INSERT INTO tenant_membership (tenant_id, person_id, since)
SELECT -8600, id, now() - interval '1 year' FROM person WHERE id BETWEEN -8604 AND -8601;
INSERT INTO tenant_trainer (tenant_id, person_id, since) VALUES (-8600, -8604, now() - interval '1 year');
INSERT INTO cohort (id, tenant_id, name, color_rgb) OVERRIDING SYSTEM VALUE
VALUES (-8601, -8600, 'A', '000000'), (-8602, -8600, 'B', '000000');
INSERT INTO cohort_membership (cohort_id, person_id, since)
VALUES (-8601, -8601, now() - interval '1 year'),
       (-8601, -8602, now() - interval '1 year'),
       (-8602, -8601, now() - interval '1 year');
INSERT INTO couple (id, man_id, woman_id, since) OVERRIDING SYSTEM VALUE
VALUES (-8601, -8601, -8602, now() - interval '1 year');

CREATE FUNCTION pg_temp.save_group(
  event_id bigint, cohorts bigint[], registrations jsonb DEFAULT '[]',
  starts_at timestamptz DEFAULT now() + interval '1 day'
) RETURNS bigint LANGUAGE sql AS $$
  SELECT saved.id FROM (VALUES (1)) seed(n)
  LEFT JOIN event_instance old ON old.id = event_id
  CROSS JOIN LATERAL save_events(
    details => ROW(null, 'Registration intent', 'group', null, '', 0, 'people',
      true, false, false, false, false)::event_details_input,
    events => ARRAY[ROW(event_id, coalesce(old.since, starts_at),
      coalesce(old.until, starts_at + interval '1 hour'), false,
      ARRAY(SELECT r FROM jsonb_populate_recordset(null::event_registration_input, registrations) r)
    )::event_input],
    trainers => ARRAY[ROW(-8604, 0)::event_trainer_input],
    cohort_ids => cohorts
  ) saved;
$$;

CREATE TEMP TABLE cases (name text, id bigint);
INSERT INTO cases VALUES ('fresh', pg_temp.save_group(null, ARRAY[-8601]::bigint[], '[
  {"person_id":-8601,"target_cohort_id":-8601},
  {"person_id":-8602,"is_cancelled":true},
  {"person_id":-8603}
]'));
SELECT tap.is((SELECT array_agg(person_id ORDER BY person_id) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'fresh') AND registration_status = 'active'),
  ARRAY[-8603,-8601]::bigint[], 'a fresh event saves exactly the displayed participants');
SELECT tap.ok(EXISTS (SELECT 1 FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'fresh') AND person_id = -8602
    AND source = 'manager' AND registration_status = 'cancelled'), 'removal before first save is recorded');

INSERT INTO cases VALUES ('duplicates', pg_temp.save_group(null, ARRAY[-8601]::bigint[], '[
  {"person_id":-8601,"target_cohort_id":-8601},
  {"person_id":-8601,"target_cohort_id":-8601}
]'));
SELECT tap.is((SELECT count(*) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'duplicates')
    AND parent_registration_id IS NULL AND person_id = -8601),
  1::bigint, 'duplicate registrations are saved once');
INSERT INTO cases VALUES ('duplicate couple', pg_temp.save_group(null, ARRAY[-8601]::bigint[], '[
  {"couple_id":-8601},
  {"couple_id":-8601}
]'));
SELECT tap.is((SELECT count(*) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'duplicate couple')
    AND parent_registration_id IS NULL AND couple_id = -8601),
  1::bigint, 'duplicate couple registrations are saved once');

UPDATE event_instance_registration SET status = 'attended', attendance_note = 'Preserve me'
WHERE instance_id = (SELECT id FROM cases WHERE name = 'fresh') AND person_id = -8601;
CREATE TEMP TABLE original AS SELECT id FROM event_instance_registration
WHERE instance_id = (SELECT id FROM cases WHERE name = 'fresh') AND person_id = -8601;
SELECT pg_temp.save_group(id, ARRAY[-8601]::bigint[], '[
  {"person_id":-8601,"target_cohort_id":-8601},{"person_id":-8603}
]') FROM cases WHERE name = 'fresh';
UPDATE cohort_membership SET status = status WHERE cohort_id = -8601;
SELECT tap.is((SELECT count(*) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'fresh') AND registration_status = 'active'),
  2::bigint, 'unchanged saves and background updates preserve individual removals');

SELECT pg_temp.save_group(id, '{}', '[{"person_id":-8601},{"person_id":-8603}]')
FROM cases WHERE name = 'fresh';
SELECT tap.ok(EXISTS (SELECT 1 FROM event_instance_registration WHERE id = (SELECT id FROM original)
  AND source = 'manager' AND registration_status = 'active'
  AND status = 'attended' AND attendance_note = 'Preserve me' AND target_cohort_id IS NULL),
  'removing a cohort and re-adding a person preserves the registration and attendance');

SELECT pg_temp.save_group(id, ARRAY[-8601]::bigint[], '[
  {"person_id":-8601,"target_cohort_id":-8601},
  {"person_id":-8602,"target_cohort_id":-8601},
  {"person_id":-8603,"is_cancelled":true}
]') FROM cases WHERE name = 'fresh';
SELECT tap.is((SELECT array_agg(person_id ORDER BY person_id) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'fresh') AND registration_status = 'active'),
  ARRAY[-8602,-8601]::bigint[], 'adding a cohort again restores previously removed members');
SELECT tap.ok(EXISTS (SELECT 1 FROM event_instance_registration WHERE id = (SELECT id FROM original)
  AND status = 'attended' AND attendance_note = 'Preserve me'), 're-addition retains attendance');
SELECT tap.ok(EXISTS (SELECT 1 FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'fresh') AND person_id = -8603
    AND registration_status = 'cancelled'), 'unrelated removals remain cancelled');

SELECT pg_temp.save_group(id, ARRAY[-8601]::bigint[], '[
  {"person_id":-8601,"is_cancelled":true},{"person_id":-8602,"target_cohort_id":-8601}
]') FROM cases WHERE name = 'fresh';
UPDATE cohort_membership SET status = status WHERE cohort_id = -8601;
SELECT tap.is((SELECT count(*) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'fresh') AND registration_status = 'active'),
  1::bigint, 'removing a person after adding the cohort wins, including after a background update');

INSERT INTO cases VALUES ('overlap', pg_temp.save_group(null, ARRAY[-8601,-8602]::bigint[], '[
  {"person_id":-8601,"target_cohort_id":-8601},{"person_id":-8602,"target_cohort_id":-8601}
]'));
SELECT pg_temp.save_group(id, ARRAY[-8602]::bigint[], '[{"person_id":-8601,"target_cohort_id":-8602}]')
FROM cases WHERE name = 'overlap';
SELECT tap.ok(EXISTS (SELECT 1 FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'overlap') AND person_id = -8601
    AND registration_status = 'active' AND target_cohort_id = -8602)
  AND EXISTS (SELECT 1 FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'overlap') AND person_id = -8602
    AND registration_status = 'cancelled' AND source = 'cohort'),
  'removing one cohort keeps shared members and cancels only omitted registrations');

CREATE TEMP TABLE original_partners AS
SELECT id, person_id FROM event_instance_registration
WHERE instance_id = (SELECT id FROM cases WHERE name = 'overlap');
UPDATE event_instance_registration SET status='attended', attendance_note='Keep partner attendance'
WHERE id IN (SELECT id FROM original_partners);
INSERT INTO event_lesson_demand (registration_id, trainer_id, lesson_count)
SELECT r.id, t.id, 1 FROM original_partners r
JOIN event_instance_trainer t ON t.instance_id = (SELECT id FROM cases WHERE name = 'overlap')
WHERE r.person_id=-8601;
SELECT pg_temp.save_group(id, ARRAY[-8601]::bigint[], '[{"couple_id":-8601}]')
FROM cases WHERE name = 'overlap';
SELECT tap.is((SELECT count(*) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'overlap') AND person_id IS NOT NULL
    AND registration_status = 'active' AND parent_registration_id IS NOT NULL), 2::bigint,
  'a couple replaces individual registrations without duplicate person rows');
SELECT tap.ok((SELECT count(*) = 2 FROM event_instance_registration
  WHERE id IN (SELECT id FROM original_partners) AND parent_registration_id IS NOT NULL
    AND registration_status='active' AND source='manager' AND attendance_note='Keep partner attendance')
  AND EXISTS (SELECT 1 FROM event_lesson_demand WHERE registration_id IN (SELECT id FROM original_partners)),
  'forming a couple reuses person rows, attendance and personal lesson requests');
SELECT pg_temp.save_group(id, ARRAY[-8601]::bigint[], '[
  {"couple_id":-8601,"is_cancelled":true},
  {"person_id":-8601,"is_cancelled":true},{"person_id":-8602,"is_cancelled":true}
]') FROM cases WHERE name = 'overlap';
UPDATE cohort_membership SET status = status WHERE cohort_id = -8601;
SELECT tap.is((SELECT count(*) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'overlap') AND registration_status = 'active'),
  0::bigint, 'explicitly removing a couple and its members survives background membership updates');
SELECT pg_temp.save_group(id, ARRAY[-8601]::bigint[], '[
  {"person_id":-8601,"target_cohort_id":-8601},{"person_id":-8602,"target_cohort_id":-8601}
]') FROM cases WHERE name = 'overlap';
SELECT tap.is((SELECT count(*) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'overlap') AND registration_status = 'active'),
  2::bigint, 'adding a cohort restores people removed as a couple');
SELECT pg_temp.save_group(id, ARRAY[-8601]::bigint[], '[{"couple_id":-8601}]')
FROM cases WHERE name = 'overlap';
SELECT pg_temp.save_group(id, ARRAY[-8601]::bigint[], '[{"person_id":-8601},{"person_id":-8602}]')
FROM cases WHERE name = 'overlap';
SELECT tap.is((SELECT count(*) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'overlap') AND registration_status = 'active'
    AND parent_registration_id IS NULL AND person_id IS NOT NULL), 2::bigint,
  'individuals can replace an existing couple');
SELECT tap.ok((SELECT count(*) = 2 FROM event_instance_registration
  WHERE id IN (SELECT id FROM original_partners) AND parent_registration_id IS NULL
    AND registration_status='active' AND attendance_note='Keep partner attendance')
  AND EXISTS (SELECT 1 FROM event_lesson_demand WHERE registration_id IN (SELECT id FROM original_partners)),
  'repeated couple/individual changes preserve the original person rows and requests');

INSERT INTO cases VALUES ('self couple', pg_temp.save_group(null, '{}'));
UPDATE event_instance SET is_public=true, capacity=2 WHERE id=(SELECT id FROM cases WHERE name='self couple');
UPDATE event_instance_trainer SET lessons_offered=10 WHERE instance_id=(SELECT id FROM cases WHERE name='self couple');
SELECT set_config('jwt.claims.my_person_ids', '{-8601,-8602}', true);
SELECT set_config('jwt.claims.my_couple_ids', '{-8601}', true);
GRANT SELECT ON cases TO anonymous;
SET LOCAL ROLE anonymous;
SELECT set_event_registration(id, -8601, null, true, 'Personal note') FROM cases WHERE name='self couple';
RESET ROLE;
CREATE TEMP TABLE self_person AS SELECT r.id, t.id AS trainer_id
FROM event_instance_registration r JOIN event_instance_trainer t ON t.instance_id=r.instance_id
WHERE r.instance_id=(SELECT id FROM cases WHERE name='self couple') AND r.person_id=-8601;
GRANT SELECT ON self_person TO anonymous;
SET LOCAL ROLE anonymous;
SELECT set_lesson_request(r.id, t.id, 1) FROM self_person r
CROSS JOIN event_instance_trainer t WHERE t.instance_id=(SELECT id FROM cases WHERE name='self couple');
SELECT tap.lives_ok(format('select set_event_registration(%s, null, -8601, true)',
  (SELECT id FROM cases WHERE name='self couple')),
  'a couple needs only one free place when one partner is already registered');
SELECT tap.is((SELECT id FROM event_instance_registration
  WHERE instance_id=(SELECT id FROM cases WHERE name='self couple') AND person_id=-8601 AND registration_status='active'),
  (SELECT id FROM self_person), 'self registration attaches the original person row');
SELECT tap.ok(NOT EXISTS (SELECT 1 FROM event_instance_registration
  WHERE instance_id=(SELECT id FROM cases WHERE name='self couple') AND source IS DISTINCT FROM 'self'),
  'both reused and new partner rows inherit the couple registration source');
SELECT tap.lives_ok(format('select set_lesson_request(%s, %s, 2)',
  (SELECT id FROM self_person),
  (SELECT id FROM event_instance_trainer WHERE instance_id=(SELECT id FROM cases WHERE name='self couple'))),
  'personal lesson requests remain editable after joining a couple registration');
SELECT set_config('jwt.claims.tenant_id', '1', true);
SELECT tap.throws_ok(format('select set_lesson_request(%s, %s, 1)', id, trainer_id),
  '28000', 'INSTANCE_NOT_FOUND', 'lesson requests cannot be written through another tenant') FROM self_person;
SELECT set_config('jwt.claims.tenant_id', '-8600', true);
RESET ROLE;
UPDATE event_instance SET capacity_unit='registrations', capacity=1 WHERE id=(SELECT id FROM cases WHERE name='self couple');
SET LOCAL ROLE anonymous;
SELECT set_event_registration(id, -8601, null, false) FROM cases WHERE name='self couple';
SELECT tap.lives_ok(format('select set_event_registration(%s, -8601, null, true)',
  (SELECT id FROM cases WHERE name='self couple')),
  'rejoining an active couple needs no additional registration-unit place');
SELECT set_event_registration(id, null, -8601, false) FROM cases WHERE name='self couple';
SELECT tap.ok(NOT EXISTS (SELECT 1 FROM event_instance_registration
  WHERE instance_id=(SELECT id FROM cases WHERE name='self couple') AND source IS DISTINCT FROM 'self'),
  'cancelling a couple propagates the source to its person rows');
SELECT set_event_registration(id, -8601, null, true) FROM cases WHERE name='self couple';
SELECT tap.ok(EXISTS (SELECT 1 FROM event_instance_registration r
  JOIN event_lesson_demand d ON d.registration_id=r.id
  WHERE r.id=(SELECT id FROM self_person) AND r.parent_registration_id IS NULL
    AND r.registration_status='active' AND r.note='Personal note' AND d.lesson_count=2),
  'individual signup after couple cancellation keeps the original row and personal requests');
RESET ROLE;
SELECT set_config('jwt.claims.my_person_ids', '{}', true);
SELECT set_config('jwt.claims.my_couple_ids', '{}', true);

INSERT INTO cases VALUES ('past', pg_temp.save_group(null, ARRAY[-8601]::bigint[], '[
  {"person_id":-8601,"target_cohort_id":-8601},{"person_id":-8602,"target_cohort_id":-8601}
]', now() - interval '2 years'));
UPDATE event_instance_registration SET status = 'attended'
WHERE instance_id = (SELECT id FROM cases WHERE name = 'past');
UPDATE cohort_membership SET status = 'expired', until = now()
WHERE cohort_id = -8601 AND person_id = -8601;
SELECT tap.is((SELECT count(*) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'past') AND registration_status = 'active'),
  2::bigint, 'membership changes leave past registrations alone');
SELECT pg_temp.save_group(id, ARRAY[-8601]::bigint[], '[
  {"person_id":-8601,"target_cohort_id":-8601},{"person_id":-8602,"target_cohort_id":-8601}
]') FROM cases WHERE name = 'past';
SELECT tap.is((SELECT count(*) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'past') AND registration_status = 'active'),
  2::bigint, 'saving a past lesson retains its displayed list regardless of current membership');
INSERT INTO cases VALUES ('copy', pg_temp.save_group(null, ARRAY[-8601]::bigint[],
  '[{"person_id":-8602,"target_cohort_id":-8601}]'));
SELECT tap.is((SELECT array_agg(person_id) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'copy') AND registration_status = 'active'),
  ARRAY[-8602]::bigint[], 'a future copy saves only the submitted list');
SELECT pg_temp.save_group(id, '{}') FROM cases WHERE name = 'past';
SELECT tap.ok(NOT EXISTS (SELECT 1 FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'past') AND registration_status = 'active')
  AND (SELECT count(*) FROM event_instance_registration
    WHERE instance_id = (SELECT id FROM cases WHERE name = 'past') AND status = 'attended') = 2,
  'removing a past cohort preserves attendance while cancelling its registrations');
SELECT pg_temp.save_group(id, ARRAY[-8602]::bigint[], '[{"person_id":-8601,"target_cohort_id":-8602}]')
FROM cases WHERE name = 'past';
SELECT tap.ok(EXISTS (SELECT 1 FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'past') AND person_id = -8601
    AND registration_status = 'active' AND target_cohort_id = -8602 AND status = 'attended'),
  'a replacement past cohort can register someone who joined after the lesson');

SELECT tap.throws_ok($$SELECT pg_temp.save_group(null, '{}',
  '[{"person_id":-8601,"target_cohort_id":-8601}]')$$, '22023', null,
  'a registration cannot name an unselected cohort');

GRANT SELECT ON cases TO trainer;
SELECT set_config('jwt.claims.my_person_ids', '[-8604]', true);
SET LOCAL ROLE trainer;
SELECT tap.lives_ok(format('select pg_temp.save_group(%s, array[-8601]::bigint[], %L::jsonb)',
  (SELECT id FROM cases WHERE name = 'fresh'), '[{"person_id":-8603}]'), 'an assigned trainer can save');
SELECT set_config('jwt.claims.tenant_id', '1', true);
SELECT tap.throws_ok(format('select pg_temp.save_group(%s, array[]::bigint[])',
  (SELECT id FROM cases WHERE name = 'fresh')), 'P0001', null, 'cross-tenant event saves are rejected');
RESET ROLE;
SET LOCAL ROLE anonymous;
SELECT tap.throws_ok('select app_private.reconcile_event_instance_cohort_registrations(array[-1]::bigint[])',
  '42501', null, 'ordinary members cannot directly invoke cohort reconciliation');
RESET ROLE;

SELECT set_config('jwt.claims.tenant_id', '-8600', true);
INSERT INTO event_instance (id, tenant_id, since, until, is_public, capacity)
OVERRIDING SYSTEM VALUE VALUES (-8650, -8600, now()+interval '1 day', now()+interval '2 days', true, 1);
INSERT INTO event_instance_trainer (id, instance_id, person_id, lessons_offered)
OVERRIDING SYSTEM VALUE VALUES (-8650, -8650, -8604, 2);
CREATE FUNCTION pg_temp.signup(
  first_name text DEFAULT 'Camp', note text DEFAULT 'Food note', lessons integer DEFAULT 1
) RETURNS event_instance_registration LANGUAGE sql AS $$
  SELECT set_event_registration(-8650, null, null, true,
    p_person => ROW(first_name, 'Guest', 'unspecified', '203', lower(first_name) || '@example.invalid',
      '+420123456789', null, null, null)::event_registration_person_input,
    p_note => note, p_lesson_trainer_ids => ARRAY[-8650]::bigint[], p_lesson_counts => ARRAY[lessons]);
$$;

CREATE TEMP TABLE submitted (id bigint, person_id bigint);
GRANT ALL ON submitted TO anonymous;
SELECT set_config('jwt.claims.my_person_ids', '{}', true);
SELECT set_config('jwt.claims.my_couple_ids', '{}', true);
SELECT set_config('jwt.claims.user_id', '', true);
SET LOCAL ROLE anonymous;
SELECT tap.throws_ok($$SELECT set_event_registration(-8650, -8601, null, true)$$,
  '42501', 'ACCESS_DENIED', 'public signup cannot select an unrelated person');
SELECT tap.is((SELECT array_agg(id) FROM event_instance_trainer
  WHERE instance_id IN (-8650, (SELECT id FROM cases WHERE name='fresh'))), ARRAY[-8650]::bigint[],
  'unregistered visitors see public-event trainers, not private-event trainers');
INSERT INTO submitted SELECT id, person_id FROM pg_temp.signup();
SELECT tap.is((SELECT event_instance_trainer_lessons_remaining(t)
  FROM event_instance_trainer t WHERE id=-8650), 1,
  'public lesson availability includes requests belonging to other people');
SELECT set_config('jwt.claims.tenant_id', '1', true);
SELECT tap.is((SELECT count(*) FROM event_instance_trainer WHERE id=-8650), 0::bigint,
  'public-event trainers remain hidden in another tenant');
SELECT set_config('jwt.claims.tenant_id', '-8600', true);
SELECT tap.is((SELECT id FROM pg_temp.signup()),
  (SELECT id FROM submitted), 'an identical retry returns the same registration even when capacity is now full');
SELECT tap.throws_ok($$SELECT pg_temp.signup(note => 'Changed note')$$,
  '22023', 'CAPACITY_EXCEEDED', 'different details do not match or overwrite the existing registration');
SELECT tap.throws_ok($$SELECT pg_temp.signup(lessons => 2)$$,
  '22023', 'CAPACITY_EXCEEDED', 'different lesson requests do not match or overwrite the existing registration');
SELECT set_config('jwt.claims.user_id', '-8650', true);
SELECT tap.throws_ok($$SELECT pg_temp.signup()$$,
  '22023', 'CAPACITY_EXCEEDED', 'matching details submitted by another creator do not reuse the registration');
SELECT set_config('jwt.claims.user_id', '', true);
SELECT tap.throws_ok($$SELECT pg_temp.signup('Second')$$,
  '22023', 'CAPACITY_EXCEEDED', 'new-person signup obeys event capacity');
RESET ROLE;
SELECT tap.is((SELECT count(*) FROM person WHERE email IN ('camp@example.invalid', 'second@example.invalid')),
  1::bigint, 'retries and failed submissions leave no extra people');
SELECT tap.ok(EXISTS (SELECT 1 FROM event_instance_registration r JOIN submitted s ON s.id=r.id
  JOIN event_lesson_demand d ON d.registration_id=r.id WHERE r.note='Food note' AND d.lesson_count=1),
  'signup saves the note and lesson request together');
SELECT tap.is((SELECT count(*) FROM person_invitation WHERE person_id=(SELECT person_id FROM submitted) AND instance_id=-8650),
  1::bigint, 'signup creates one event invitation across retries');
SELECT tap.is((SELECT count(*) FROM tenant_membership WHERE person_id=(SELECT person_id FROM submitted)),
  0::bigint, 'camp registration does not create club membership');
UPDATE event_instance SET is_locked=true, capacity=null WHERE id=-8650;
DELETE FROM event_instance_trainer WHERE id=-8650;
SET LOCAL ROLE anonymous;
SELECT tap.throws_ok($$SELECT pg_temp.signup('Second')$$,
  '28000', 'NOT_ALLOWED', 'an event without trainers still enforces its registration lock');
SELECT set_config('jwt.claims.tenant_id', '1', true);
SELECT tap.throws_ok($$SELECT pg_temp.signup('Second')$$,
  '22023', 'INSTANCE_NOT_FOUND', 'new-person signup cannot cross the request tenant');
RESET ROLE;

SELECT set_config('jwt.claims.tenant_id', '-8600', true);
SET LOCAL ROLE trainer;
SELECT tap.lives_ok($$SELECT set_event_registration(-8650, -8601, null, true)$$,
  'a tenant trainer can register people in a locked event without assigned trainers');
RESET ROLE;
SET LOCAL ROLE administrator;
SELECT set_config('jwt.claims.user_id', '-8650', true);
SELECT tap.is((SELECT created_by FROM set_event_registration(-8650, -8602, null, true)),
  -8650::bigint, 'administrator registration succeeds without a membership lookup and records its creator');
RESET ROLE;

SELECT * FROM tap.finish(true);
ROLLBACK;
