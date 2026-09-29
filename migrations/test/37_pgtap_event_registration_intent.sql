BEGIN;
CREATE SCHEMA IF NOT EXISTS tap;
CREATE EXTENSION IF NOT EXISTS pgtap SCHEMA tap;
SET LOCAL search_path = public, tap;
SELECT tap.no_plan();

INSERT INTO tenant (id, name) VALUES (-8600, 'Registration intent tests');
SELECT set_config('jwt.claims.tenant_id', '-8600', true);
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

SELECT pg_temp.save_group(id, ARRAY[-8601]::bigint[], '[{"couple_id":-8601}]')
FROM cases WHERE name = 'overlap';
SELECT tap.is((SELECT count(*) FROM event_instance_registration
  WHERE instance_id = (SELECT id FROM cases WHERE name = 'overlap') AND person_id IS NOT NULL
    AND registration_status = 'active' AND parent_registration_id IS NOT NULL), 2::bigint,
  'a couple replaces individual registrations without duplicate person rows');
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
GRANT USAGE ON SCHEMA tap TO trainer, anonymous;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA tap TO trainer, anonymous;
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

SELECT * FROM tap.finish(true);
ROLLBACK;
