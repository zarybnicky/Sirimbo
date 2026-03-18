BEGIN;

CREATE SCHEMA IF NOT EXISTS tap;
CREATE EXTENSION IF NOT EXISTS pgtap SCHEMA tap;
DO $$
BEGIN
  execute 'set search_path to ' || current_setting('search_path') || ',tap';
END
$$;

SELECT tap.plan(8);

-- Fixtures
-- People:
--   200001 = Alice   (active member, has past 'attended' → registration survives expiry)
--   200002 = Bob     (active member, has past 'attended' → registration survives expiry)
--   200003 = Charlie (active member, future 'not-excused' + past 'unknown' → future cancelled, survives)
--   200004 = Dana    (active member, only future 'unknown', no attended past → registration deleted)
-- Cohort:  800001
-- Event:   700001 (targets cohort 800001)
-- Instances:
--   900001 = past   (since = now()-2d, until = now()-1d)
--   900002 = future (since = now()+1d, until = now()+2d)

INSERT INTO tenant (id, name) VALUES (1000, 'Test Tenant') ON CONFLICT (id) DO NOTHING;

INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
  VALUES (200001, 'Alice',   'Test', 'unspecified', ''),
         (200002, 'Bob',     'Test', 'unspecified', ''),
         (200003, 'Charlie', 'Test', 'unspecified', ''),
         (200004, 'Dana',    'Test', 'unspecified', '')
  ON CONFLICT (id) DO NOTHING;

INSERT INTO cohort (id, tenant_id, name, color_rgb) OVERRIDING SYSTEM VALUE
  VALUES (800001, 1000, 'Test Cohort', '000000')
  ON CONFLICT (id) DO NOTHING;

INSERT INTO event (id, tenant_id, type, name, location_text, description) OVERRIDING SYSTEM VALUE
  VALUES (700001, 1000, 'lesson', 'Test Event', '', '')
  ON CONFLICT (id) DO NOTHING;

INSERT INTO event_target_cohort (id, event_id, cohort_id, tenant_id) OVERRIDING SYSTEM VALUE
  VALUES (850001, 700001, 800001, 1000)
  ON CONFLICT (event_id, cohort_id) DO NOTHING;

INSERT INTO event_instance (id, tenant_id, event_id, since, until) OVERRIDING SYSTEM VALUE
  VALUES (900001, 1000, 700001, now() - interval '2 days', now() - interval '1 day'),
         (900002, 1000, 700001, now() + interval '1 day',  now() + interval '2 days')
  ON CONFLICT (id) DO NOTHING;

-- Active memberships
INSERT INTO cohort_membership (id, tenant_id, cohort_id, person_id, since, status) OVERRIDING SYSTEM VALUE
  VALUES (860001, 1000, 800001, 200001, now() - interval '30 days', 'active'),
         (860002, 1000, 800001, 200002, now() - interval '30 days', 'active'),
         (860003, 1000, 800001, 200003, now() - interval '30 days', 'active'),
         (860004, 1000, 800001, 200004, now() - interval '30 days', 'active')
  ON CONFLICT (id) DO NOTHING;

-- Attendance:
--   Alice:   past=attended, future=unknown  → registration survives (attended past row)
--   Bob:     past=attended, future=unknown  → registration survives (attended past row)
--   Charlie: future=not-excused             → cancelled on expiry, registration survives (attended past row)
--   Dana:    future=unknown (no past row)   → registration deleted (all rows become cancelled, no history)
UPDATE event_attendance SET status = 'attended'    WHERE instance_id = 900001 AND person_id = 200001;
UPDATE event_attendance SET status = 'attended'    WHERE instance_id = 900001 AND person_id = 200002;
UPDATE event_attendance SET status = 'not-excused' WHERE instance_id = 900002 AND person_id = 200003;

-- ── Expire Alice ──────────────────────────────────────────────────────────────
UPDATE cohort_membership SET until = now() - interval '1 second', status = 'expired' WHERE id = 860001;

-- Test 1: future 'unknown' attendance is set to 'cancelled'
SELECT tap.is(
  (SELECT status FROM event_attendance WHERE instance_id = 900002 AND person_id = 200001),
  'cancelled'::attendance_type,
  'expiry cancels future unknown attendance'
);

-- Test 2: past 'attended' attendance is not touched (instance is not "future")
SELECT tap.is(
  (SELECT status FROM event_attendance WHERE instance_id = 900001 AND person_id = 200001),
  'attended'::attendance_type,
  'expiry does not touch past attended attendance'
);

-- Test 3: registration is NOT deleted because there is a past attended row
SELECT tap.ok(
  EXISTS (SELECT 1 FROM event_registration WHERE event_id = 700001 AND person_id = 200001),
  'registration with past attended row is NOT deleted on expiry'
);

-- ── Expire Bob ────────────────────────────────────────────────────────────────
UPDATE cohort_membership SET until = now() - interval '1 second', status = 'expired' WHERE id = 860002;

-- Test 4: future unknown attendance cancelled
SELECT tap.is(
  (SELECT status FROM event_attendance WHERE instance_id = 900002 AND person_id = 200002),
  'cancelled'::attendance_type,
  'expiry cancels Bob''s future unknown attendance'
);

-- Test 5: past 'attended' attendance is untouched
SELECT tap.is(
  (SELECT status FROM event_attendance WHERE instance_id = 900001 AND person_id = 200002),
  'attended'::attendance_type,
  'expiry does not overwrite attended attendance'
);

-- Test 6: registration with 'attended' past record is NOT deleted
SELECT tap.ok(
  EXISTS (SELECT 1 FROM event_registration WHERE event_id = 700001 AND person_id = 200002),
  'registration with attended attendance is NOT deleted on expiry'
);

-- ── Expire Dana ───────────────────────────────────────────────────────────────
UPDATE cohort_membership SET until = now() - interval '1 second', status = 'expired' WHERE id = 860004;

-- Test 7: registration with ONLY future unknown attendance IS deleted
-- (all attendance rows become cancelled → bool_and(cancelled) = true → deleted)
SELECT tap.ok(
  NOT EXISTS (SELECT 1 FROM event_registration WHERE event_id = 700001 AND person_id = 200004),
  'registration with only future attendance is deleted when all become cancelled'
);

-- ── Re-activate Alice ─────────────────────────────────────────────────────────
INSERT INTO cohort_membership (tenant_id, cohort_id, person_id, since, status)
  VALUES (1000, 800001, 200001, now(), 'active');

-- Test 8: re-activation re-registers the person (register_new_cohort_member_to_events)
SELECT tap.ok(
  EXISTS (
    SELECT 1 FROM event_registration
    WHERE event_id = 700001 AND person_id = 200001 AND tenant_id = 1000
  ),
  're-activating membership re-registers person to event with future instances'
);

SELECT tap.finish();

ROLLBACK;
