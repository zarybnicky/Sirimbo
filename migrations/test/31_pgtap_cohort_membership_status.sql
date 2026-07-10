BEGIN;

CREATE SCHEMA IF NOT EXISTS tap;
CREATE EXTENSION IF NOT EXISTS pgtap SCHEMA tap;
DO $$
BEGIN
  execute 'set search_path to ' || current_setting('search_path') || ',tap';
END
$$;

SELECT tap.plan(26);

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
--   Charlie: past=attended, future=not-excused → future cancelled, registration survives
--   Dana:    future=unknown (no past row)   → registration deleted (all rows become cancelled, no history)
UPDATE event_instance_registration SET status = 'attended'    WHERE instance_id = 900001 AND person_id = 200001;
UPDATE event_instance_registration SET status = 'attended'    WHERE instance_id = 900001 AND person_id = 200002;
UPDATE event_instance_registration SET status = 'attended'    WHERE instance_id = 900001 AND person_id = 200003;
UPDATE event_instance_registration SET status = 'not-excused' WHERE instance_id = 900002 AND person_id = 200003;

-- ── Expire Alice ──────────────────────────────────────────────────────────────
UPDATE cohort_membership SET until = now() - interval '1 second', status = 'expired' WHERE id = 860001;

-- Test 1: future 'unknown' attendance is set to 'cancelled'
SELECT tap.is(
  (SELECT status FROM event_instance_registration WHERE instance_id = 900002 AND person_id = 200001),
  'cancelled'::attendance_type,
  'expiry cancels future unknown attendance'
);

-- Test 2: past 'attended' attendance is not touched (instance is not "future")
SELECT tap.is(
  (SELECT status FROM event_instance_registration WHERE instance_id = 900001 AND person_id = 200001),
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
  (SELECT status FROM event_instance_registration WHERE instance_id = 900002 AND person_id = 200002),
  'cancelled'::attendance_type,
  'expiry cancels Bob''s future unknown attendance'
);

-- Test 5: past 'attended' attendance is untouched
SELECT tap.is(
  (SELECT status FROM event_instance_registration WHERE instance_id = 900001 AND person_id = 200002),
  'attended'::attendance_type,
  'expiry does not overwrite attended attendance'
);

-- Test 6: registration with 'attended' past record is NOT deleted
SELECT tap.ok(
  EXISTS (SELECT 1 FROM event_registration WHERE event_id = 700001 AND person_id = 200002),
  'registration with attended attendance is NOT deleted on expiry'
);

-- ── Expire Charlie ────────────────────────────────────────────────────────────
UPDATE cohort_membership SET until = now() - interval '1 second', status = 'expired' WHERE id = 860003;

SELECT tap.is(
  (SELECT status FROM event_instance_registration WHERE instance_id = 900002 AND person_id = 200003),
  'cancelled'::attendance_type,
  'expiry cancels future not-excused attendance'
);

SELECT tap.ok(
  EXISTS (SELECT 1 FROM event_registration WHERE event_id = 700001 AND person_id = 200003),
  'registration with past attended history survives not-excused cancellation'
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

SELECT tap.ok(
  NOT EXISTS (SELECT 1 FROM event_instance_registration WHERE registration_status <> 'active'),
  'legacy root and child rows start with an active registration lifecycle'
);

SELECT set_config('jwt.claims.tenant_id', '1000', true);

INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES (2900001, 'Stats', 'Test', 'unspecified', '');

INSERT INTO event_instance (id, tenant_id, since, until) OVERRIDING SYSTEM VALUE
VALUES
  (9900001, 1000, now(), now() + interval '1 hour'),
  (9900002, 1000, now() + interval '2 hours', now() + interval '3 hours');

INSERT INTO event_instance_registration (tenant_id, instance_id, person_id, status)
VALUES (1000, 9900001, 2900001, 'unknown');

UPDATE event_instance_registration
SET instance_id = 9900002
WHERE instance_id = 9900001 AND person_id = 2900001;

SELECT tap.ok(
  (SELECT stats->>'TOTAL' = '0' FROM event_instance WHERE id = 9900001)
    AND (SELECT stats->>'TOTAL' = '1' FROM event_instance WHERE id = 9900002),
  'moving attendance refreshes both the source and destination instance summaries'
);

UPDATE event_instance SET parent_id = 9900001 WHERE id = 9900002;

SELECT tap.is(
  (
    SELECT count(*)::int
    FROM event_instances_for_range(
      null,
      '-infinity'::timestamptz,
      null,
      null,
      null,
      false,
      9900001
    )
  ),
  1,
  'event instance range filtering scopes results to the requested parent'
);

INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES
  (2900002, 'Trainer', 'Policy', 'unspecified', ''),
  (2900003, 'Participant', 'Policy', 'unspecified', ''),
  (2900004, 'Other trainer', 'Policy', 'unspecified', ''),
  (2900005, 'Other participant', 'Policy', 'unspecified', '');

INSERT INTO event_instance (id, tenant_id, since, until) OVERRIDING SYSTEM VALUE
VALUES (9900003, 1000, now() + interval '4 hours', now() + interval '5 hours');

INSERT INTO event_instance_trainer (tenant_id, instance_id, person_id)
VALUES
  (1000, 9900001, 2900002),
  (1000, 9900003, 2900004);

UPDATE event_instance SET is_public = true WHERE id = 9900003;

INSERT INTO event_instance_registration (tenant_id, instance_id, person_id, status)
VALUES (1000, 9900003, 2900005, 'unknown');

INSERT INTO event_trainer (tenant_id, event_id, person_id, lessons_offered)
VALUES (1000, 700001, 2900002, 1);

INSERT INTO event_lesson_demand (
  tenant_id, trainer_id, registration_id, lesson_count, event_id
)
SELECT 1000, et.id, er.id, 1, 700001
FROM event_trainer et
JOIN event_registration er
  ON er.event_id = et.event_id AND er.person_id = 200002
WHERE et.event_id = 700001 AND et.person_id = 2900002;

SELECT set_config('jwt.claims.my_person_ids', '[2900002]', true);
GRANT USAGE ON SCHEMA tap TO trainer;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA tap TO trainer;
SET LOCAL ROLE trainer;

SELECT tap.lives_ok(
  $$
    insert into event_instance_registration (tenant_id, instance_id, person_id, status)
    values (1000, 9900001, 2900003, 'unknown')
  $$,
  'a trainer can insert attendance for an instance they manage'
);

SELECT tap.ok(
  (
    SELECT updated.id = eir.id
      AND updated.status = 'attended'
      AND updated.attendance_note = 'ID-targeted attendance note'
    FROM event_instance_registration eir
    CROSS JOIN LATERAL public.update_attendance(
      eir.id,
      'attended',
      'ID-targeted attendance note'
    ) updated
    WHERE eir.instance_id = 900001 AND eir.person_id = 200002
  ),
  'an assigned trainer can update attendance by registration ID'
);

SELECT tap.is(
  (
    public.update_attendance(
      (
        SELECT id
        FROM event_instance_registration
        WHERE instance_id = 9900003 AND person_id = 2900005
      ),
      'attended',
      'Forbidden update'
    )
  ).id,
  null::bigint,
  'RLS prevents a trainer from updating attendance for another trainer''s instance'
);

SELECT tap.throws_ok(
  $$
    update event_instance_registration
    set instance_id = 9900003
    where instance_id = 9900001 and person_id = 2900003
  $$,
  '42501'::char(5),
  null,
  'a trainer cannot move attendance to an instance they do not manage'
);

SELECT tap.throws_ok(
  $$
    insert into event_instance_registration (tenant_id, instance_id, person_id, status)
    values (1000, 9900003, 2900003, 'unknown')
  $$,
  '42501'::char(5),
  null,
  'a trainer cannot insert attendance for an instance they do not manage'
);

CREATE TEMP TABLE _scheduled_lesson ON COMMIT DROP AS
SELECT id
FROM public.quick_create_event_instances(
  ARRAY[
    ROW(
      now() + interval '6 hours',
      now() + interval '7 hours',
      'lesson'::event_type,
      null::bigint,
      'Scheduled lesson',
      ARRAY[2900002]::bigint[],
      ARRAY[ROW(2900003, null)::quick_event_registration_input]
    )::quick_event_input
  ],
  9900001
);

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_instance child
    WHERE child.id = (SELECT id FROM _scheduled_lesson)
      AND child.event_id is null
      AND child.parent_id = 9900001
      AND 2900002 = any(child.manager_person_ids)
  ) AND EXISTS (
    SELECT 1
    FROM event_instance_registration registration
    WHERE registration.instance_id = (SELECT id FROM _scheduled_lesson)
      AND registration.person_id = 2900003
      AND registration.registration_status = 'active'
  ) AND EXISTS (
    SELECT 1
    FROM event_instances_for_range(
      null, now(), now() + interval '1 day', null, null, false, 9900001
    ) child
    WHERE child.id = (SELECT id FROM _scheduled_lesson)
  )
  AND NOT EXISTS (
    SELECT 1
    FROM event_instances_for_range(
      null, now(), now() + interval '1 day', null, null, false, null
    ) root
    WHERE root.id = (SELECT id FROM _scheduled_lesson)
  ),
  'quick creation makes a managed lesson with registration and scoped range'
);

SELECT tap.lives_ok(
  format(
    'insert into event_instance (tenant_id, parent_id, since, until) values (1000, %s, now() + interval ''8 hours'', now() + interval ''9 hours'')',
    (SELECT id FROM _scheduled_lesson)
  ),
  'event instance schedules may be nested'
);

SELECT tap.throws_ok(
  'delete from event_instance where id = 9900001',
  '23503'::char(5),
  null,
  'a schedule parent cannot be deleted while it has lessons'
);

RESET ROLE;

INSERT INTO couple (id, man_id, woman_id, since) OVERRIDING SYSTEM VALUE
VALUES (2950001, 2900003, 2900005, now());

SELECT public.update_event_instance_details(
  instance.id,
  instance.since,
  instance.until,
  instance.name,
  instance.type,
  instance.location_id,
  instance.location_text,
  instance.is_visible,
  instance.is_public,
  instance.is_cancelled,
  null,
  ARRAY[ROW(null, 2950001)::quick_event_registration_input]
)
FROM event_instance instance
WHERE instance.id = (SELECT id FROM _scheduled_lesson);

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_instance_registration root
    WHERE root.instance_id = (SELECT id FROM _scheduled_lesson)
      AND root.couple_id = 2950001
      AND root.registration_status = 'active'
      AND 2 = (
        SELECT count(*)
        FROM event_instance_registration child
        WHERE child.parent_registration_id = root.id
          AND child.registration_status = 'active'
      )
  ) AND EXISTS (
    SELECT 1
    FROM event_instance_registration
    WHERE instance_id = (SELECT id FROM _scheduled_lesson)
      AND person_id = 2900003
      AND parent_registration_id is null
      AND registration_status = 'cancelled'
  ) AND (
    SELECT stats->>'TOTAL' = '2'
    FROM event_instance
    WHERE id = (SELECT id FROM _scheduled_lesson)
  ),
  'quick edit replaces a person registration with their couple'
);

SELECT public.update_event_instance_details(
  instance.id,
  instance.since,
  instance.until,
  instance.name,
  instance.type,
  instance.location_id,
  instance.location_text,
  instance.is_visible,
  instance.is_public,
  instance.is_cancelled,
  null,
  ARRAY[ROW(2900003, null)::quick_event_registration_input]
)
FROM event_instance instance
WHERE instance.id = (SELECT id FROM _scheduled_lesson);

SELECT public.update_event_instance_details(
  instance.id,
  instance.since,
  instance.until,
  'Scheduled lesson renamed',
  instance.type,
  instance.location_id,
  instance.location_text,
  instance.is_visible,
  instance.is_public,
  instance.is_cancelled,
  null,
  null,
  4,
  'registrations',
  true
)
FROM event_instance instance
WHERE instance.id = (SELECT id FROM _scheduled_lesson);

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_instance_registration root
    WHERE root.instance_id = (SELECT id FROM _scheduled_lesson)
      AND root.couple_id = 2950001
      AND root.registration_status = 'cancelled'
      AND NOT EXISTS (
        SELECT 1
        FROM event_instance_registration child
        WHERE child.parent_registration_id = root.id
          AND child.registration_status <> 'cancelled'
      )
  )
  AND EXISTS (
    SELECT 1
    FROM event_instance_registration
    WHERE instance_id = (SELECT id FROM _scheduled_lesson)
      AND person_id = 2900003
      AND parent_registration_id is null
      AND registration_status = 'active'
  )
  AND EXISTS (
    SELECT 1
    FROM event_instance
    WHERE id = (SELECT id FROM _scheduled_lesson)
      AND name = 'Scheduled lesson renamed'
      AND capacity = 4
      AND capacity_unit = 'registrations'
      AND is_locked
      AND stats->>'TOTAL' = '1'
  ),
  'quick edit updates details, capacity, capacity unit and lock while null leaves registrations unchanged'
);

SELECT tap.throws_ok(
  $$
    select public.update_event_instance_details(
      instance.id,
      instance.since,
      instance.until,
      instance.name,
      instance.type,
      instance.location_id,
      instance.location_text,
      instance.is_visible,
      instance.is_public,
      instance.is_cancelled,
      null,
      '{}'::quick_event_registration_input[]
    )
    from event_instance instance
    where instance.id = 900001
  $$,
  'P0001',
  'event-backed instance 900001 registrations must be edited through event_registration',
  'event-backed registrations stay on the legacy writer'
);

CREATE TEMP TABLE _stats_before_cancel ON COMMIT DROP AS
SELECT (stats->>'TOTAL')::int AS total
FROM event_instance
WHERE id = 900001;

UPDATE event_instance_registration
SET registration_status = 'cancelled'
WHERE instance_id = 900001 AND person_id = 200002;

CREATE TEMP TABLE _detached_audit_before ON COMMIT DROP AS
SELECT person_id, attendance_created_at, attendance_updated_at
FROM event_instance_registration
WHERE instance_id = 900001 AND person_id is not null;

SET LOCAL ROLE trainer;

SELECT tap.is(
  (public.detach_event_instance(900001, 'Detached test event')).name,
  'Detached test event',
  'an assigned trainer can detach an instance with lesson demand'
);

RESET ROLE;

SELECT tap.ok(
  NOT EXISTS (
    SELECT 1
    FROM _detached_audit_before before
    LEFT JOIN event_instance_registration eir
      ON eir.instance_id = 900001 AND eir.person_id = before.person_id
    WHERE eir.id is null
      OR eir.attendance_created_at is distinct from before.attendance_created_at
      OR eir.attendance_updated_at is distinct from before.attendance_updated_at
  ) AND EXISTS (
    SELECT 1
    FROM event_instance ei
    JOIN event_lesson_demand d ON d.event_id = ei.event_id
    WHERE ei.id = 900001
  ) AND EXISTS (
    SELECT 1
    FROM event_instance_registration
    WHERE instance_id = 900001
      AND person_id = 200002
      AND registration_status = 'cancelled'
  ) AND (
    SELECT (instance.stats->>'TOTAL')::int = before.total - 1
    FROM event_instance instance
    CROSS JOIN _stats_before_cancel before
    WHERE instance.id = 900001
  ),
  'detaching preserves registration, attendance audit, and lesson demand'
);

SELECT tap.finish();

ROLLBACK;
