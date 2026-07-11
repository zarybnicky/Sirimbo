BEGIN;

CREATE SCHEMA IF NOT EXISTS tap;
CREATE EXTENSION IF NOT EXISTS pgtap SCHEMA tap;
DO $$
BEGIN
  execute 'set search_path to ' || current_setting('search_path') || ',tap';
END
$$;

SELECT tap.plan(35);

-- Fixtures
-- People:
--   200001 = Alice   (active member, has past 'attended' → registration survives expiry)
--   200002 = Bob     (active member, has past 'attended' → registration survives expiry)
--   200003 = Charlie (active member, future 'not-excused' + past 'unknown' → future cancelled, survives)
--   200004 = Dana    (active member, only future 'unknown' → future lifecycle cancelled)
--   200005 = Evan    (active member, no legacy registration → exact registration is created)
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
         (200004, 'Dana',    'Test', 'unspecified', ''),
         (200005, 'Evan',    'Test', 'unspecified', '')
  ON CONFLICT (id) DO NOTHING;

INSERT INTO cohort (id, tenant_id, name, color_rgb) OVERRIDING SYSTEM VALUE
  VALUES (800001, 1000, 'Test Cohort', '000000')
  ON CONFLICT (id) DO NOTHING;

INSERT INTO event (id, tenant_id, type, name, location_text, description) OVERRIDING SYSTEM VALUE
  VALUES (700001, 1000, 'lesson', 'Test Event', '', '')
  ON CONFLICT (id) DO NOTHING;

INSERT INTO event_series (id, tenant_id) OVERRIDING SYSTEM VALUE
  VALUES (700001, 1000)
  ON CONFLICT (id) DO NOTHING;

INSERT INTO event_target_cohort (id, event_id, cohort_id, tenant_id) OVERRIDING SYSTEM VALUE
  VALUES (850001, 700001, 800001, 1000)
  ON CONFLICT (event_id, cohort_id) DO NOTHING;

INSERT INTO event_instance (id, tenant_id, event_id, series_id, since, until) OVERRIDING SYSTEM VALUE
  VALUES (900001, 1000, 700001, 700001, now() - interval '2 days', now() - interval '1 day'),
         (900002, 1000, 700001, 700001, now() + interval '1 day',  now() + interval '2 days')
  ON CONFLICT (id) DO NOTHING;

INSERT INTO event_instance_target_cohort (id, tenant_id, instance_id, cohort_id) OVERRIDING SYSTEM VALUE
  VALUES (850101, 1000, 900001, 800001)
  ON CONFLICT (instance_id, cohort_id) DO NOTHING;

-- Existing registrations remain bridged during this slice, but cohort automation
-- now changes their exact per-instance lifecycle directly.
INSERT INTO event_registration (tenant_id, event_id, target_cohort_id, person_id)
SELECT 1000, 700001, 850001, person_id
FROM unnest(ARRAY[200001, 200002, 200003, 200004]::bigint[]) person(person_id)
ON CONFLICT ON CONSTRAINT event_registration_unique_event_person_couple_key DO NOTHING;

-- Active memberships
INSERT INTO cohort_membership (id, tenant_id, cohort_id, person_id, since, status) OVERRIDING SYSTEM VALUE
  VALUES (860001, 1000, 800001, 200001, now() - interval '30 days', 'active'),
         (860002, 1000, 800001, 200002, now() - interval '30 days', 'active'),
         (860003, 1000, 800001, 200003, now() - interval '30 days', 'active'),
         (860004, 1000, 800001, 200004, now() - interval '30 days', 'active'),
         (860005, 1000, 800001, 200005, now() - interval '30 days', 'active')
  ON CONFLICT (id) DO NOTHING;

-- The submitted participant list predates the cohort selection. Participant
-- edits must run first so adding the target can append Evan afterwards.
SELECT public.update_event_instance_details(
  p_instance_id => instance.id,
  p_since => instance.since,
  p_until => instance.until,
  p_name => instance.name,
  p_type => instance.type,
  p_location_id => instance.location_id,
  p_location_text => instance.location_text,
  p_is_visible => instance.is_visible,
  p_is_public => instance.is_public,
  p_is_cancelled => instance.is_cancelled,
  p_registrations => ARRAY[
    ROW(200001, null)::public.quick_event_registration_input,
    ROW(200002, null)::public.quick_event_registration_input,
    ROW(200003, null)::public.quick_event_registration_input,
    ROW(200004, null)::public.quick_event_registration_input
  ],
  p_cohort_ids => ARRAY[800001, 800001]
)
FROM event_instance instance
WHERE instance.id = 900002;

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200005
      AND parent_registration_id is null
      AND legacy_registration_id is null
      AND registration_status = 'active'
      AND source = 'cohort'
      AND target_cohort_id = 800001
  ) AND 1 = (
    SELECT count(*)
    FROM event_instance_target_cohort
    WHERE instance_id = 900002
      AND cohort_id = 800001
  ) AND NOT EXISTS (
    SELECT 1
    FROM event_instance_registration
    WHERE instance_id = 900001
      AND person_id = 200005
  ),
  'active cohort membership creates a registration only for the future instance'
);

-- Attendance:
--   Alice:   past=attended, future=unknown  → registration survives (attended past row)
--   Bob:     past=attended, future=unknown  → registration survives (attended past row)
--   Charlie: past=attended, future=not-excused → future cancelled, registration survives
--   Dana:    future=unknown (no past row)   → future registration lifecycle cancelled
UPDATE event_instance_registration SET status = 'attended'    WHERE instance_id = 900001 AND person_id = 200001;
UPDATE event_instance_registration SET status = 'attended'    WHERE instance_id = 900001 AND person_id = 200002;
UPDATE event_instance_registration SET status = 'attended'    WHERE instance_id = 900001 AND person_id = 200003;
UPDATE event_instance_registration SET status = 'not-excused' WHERE instance_id = 900002 AND person_id = 200003;

-- ── Expire Alice ──────────────────────────────────────────────────────────────
UPDATE cohort_membership SET until = now() - interval '1 second', status = 'expired' WHERE id = 860001;

-- Test 1: future registration is cancelled without changing attendance.
SELECT tap.ok(
  EXISTS (
    SELECT 1 FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200001
      AND registration_status = 'cancelled'
      AND source = 'cohort'
      AND target_cohort_id = 800001
      AND status = 'unknown'
  ),
  'expiry cancels the future registration without changing attendance'
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

-- Test 4: future registration cancelled.
SELECT tap.is(
  (SELECT registration_status FROM event_instance_registration WHERE instance_id = 900002 AND person_id = 200002),
  'cancelled'::event_instance_registration_status,
  'expiry cancels Bob''s future registration'
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
  (SELECT registration_status FROM event_instance_registration WHERE instance_id = 900002 AND person_id = 200003),
  'cancelled'::event_instance_registration_status,
  'expiry cancels the future registration with not-excused attendance'
);

SELECT tap.ok(
  EXISTS (SELECT 1 FROM event_registration WHERE event_id = 700001 AND person_id = 200003),
  'registration with past attended history survives not-excused cancellation'
);

-- ── Expire Dana ───────────────────────────────────────────────────────────────
UPDATE cohort_membership SET until = now() - interval '1 second', status = 'expired' WHERE id = 860004;

-- Test 7: the legacy source remains while only the future instance registration is cancelled.
SELECT tap.ok(
  EXISTS (SELECT 1 FROM event_registration WHERE event_id = 700001 AND person_id = 200004)
  AND EXISTS (
    SELECT 1 FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200004
      AND registration_status = 'cancelled'
  ),
  'expiry preserves the bridge source and cancels only the future registration'
);

-- ── Re-activate Alice ─────────────────────────────────────────────────────────
INSERT INTO cohort_membership (tenant_id, cohort_id, person_id, since, status)
  VALUES (1000, 800001, 200001, now(), 'active');

-- Test 8: re-activation restores the exact future registration.
SELECT tap.ok(
  EXISTS (
    SELECT 1 FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200001
      AND registration_status = 'active'
  ),
  're-activating membership restores the exact future registration'
);

SELECT tap.ok(
  EXISTS (
    SELECT 1 FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200001
      AND target_cohort_id = 800001
      AND source = 'cohort'
      AND registration_status = 'active'
  ),
  'cohort management stays on the exact registration'
);

CREATE TEMP TABLE _target_unchanged_before ON COMMIT DROP AS
SELECT id, created_at, updated_at
FROM event_instance_target_cohort
WHERE instance_id = 900002 AND cohort_id = 800001;

SELECT public.update_event_instance_details(
  p_instance_id => instance.id,
  p_since => instance.since,
  p_until => instance.until,
  p_name => instance.name,
  p_type => instance.type,
  p_location_id => instance.location_id,
  p_location_text => instance.location_text,
  p_is_visible => instance.is_visible,
  p_is_public => instance.is_public,
  p_is_cancelled => instance.is_cancelled,
  p_cohort_ids => ARRAY[800001]
)
FROM event_instance instance
WHERE instance.id = 900002;

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM _target_unchanged_before before
    JOIN event_instance_target_cohort target ON target.id = before.id
    WHERE target.instance_id = 900002
      AND target.cohort_id = 800001
      AND target.created_at = before.created_at
      AND target.updated_at = before.updated_at
  ),
  'saving an unchanged instance cohort preserves its exact target row'
);

SELECT public.update_event_instance_details(
  p_instance_id => instance.id,
  p_since => instance.since,
  p_until => instance.until,
  p_name => instance.name,
  p_type => instance.type,
  p_location_id => instance.location_id,
  p_location_text => instance.location_text,
  p_is_visible => instance.is_visible,
  p_is_public => instance.is_public,
  p_is_cancelled => instance.is_cancelled,
  p_cohort_ids => '{}'::bigint[]
)
FROM event_instance instance
WHERE instance.id = 900002;

CREATE TEMP TABLE _target_removal ON COMMIT DROP AS
SELECT registration_status = 'cancelled' AS cancelled,
  source = 'cohort' AND target_cohort_id = 800001 AS cohort_managed,
  EXISTS (
    SELECT 1 FROM event_instance_target_cohort
    WHERE instance_id = 900001 AND cohort_id = 800001
  ) AS past_target_preserved
FROM event_instance_registration
WHERE instance_id = 900002 AND person_id = 200001;

SELECT public.update_event_instance_details(
  p_instance_id => instance.id,
  p_since => instance.since,
  p_until => instance.until,
  p_name => instance.name,
  p_type => instance.type,
  p_location_id => instance.location_id,
  p_location_text => instance.location_text,
  p_is_visible => instance.is_visible,
  p_is_public => instance.is_public,
  p_is_cancelled => instance.is_cancelled,
  p_cohort_ids => ARRAY[800001, 800001]
)
FROM event_instance instance
WHERE instance.id = 900002;

SELECT tap.ok(
  (SELECT cancelled AND cohort_managed AND past_target_preserved FROM _target_removal)
  AND EXISTS (
    SELECT 1 FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200001
      AND registration_status = 'active'
      AND source = 'cohort'
      AND target_cohort_id = 800001
  ) AND 1 = (
    SELECT count(*)
    FROM event_instance_target_cohort
    WHERE instance_id = 900002
      AND cohort_id = 800001
  ),
  'removing and restoring an instance target reconciles its registration'
);

SELECT set_config('jwt.claims.tenant_id', '1000', true);
SELECT set_config('jwt.claims.my_person_ids', '[200001]', true);

SELECT set_event_instance_registration(900002, 200001, null, false);

SELECT app_private.reconcile_event_instance_cohort_registrations(
  ARRAY[900002], ARRAY[200001]
);

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200001
      AND parent_registration_id is null
      AND registration_status = 'cancelled'
      AND source = 'self'
      AND target_cohort_id is null
  ),
  'cohort reconciliation preserves a member''s own cancellation'
);

UPDATE event_instance SET is_public = true WHERE id = 900002;
SELECT set_config('jwt.claims.my_tenant_ids', '[1000]', true);
SELECT set_config('jwt.claims.my_person_ids', '[200003]', true);
SELECT set_event_instance_registration(900002, 200003, null, true);

SELECT app_private.reconcile_event_instance_cohort_registrations(
  ARRAY[900002], ARRAY[200003]
);

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200003
      AND parent_registration_id is null
      AND registration_status = 'active'
      AND source = 'self'
      AND target_cohort_id is null
  ),
  'member re-registration remains self-managed through reconciliation'
);

INSERT INTO cohort_membership (tenant_id, cohort_id, person_id, since, status)
VALUES (1000, 800001, 200002, now(), 'active');

CREATE TEMP TABLE _manager_removal_before ON COMMIT DROP AS
SELECT registration_status = 'active'
    AND source = 'cohort'
    AND target_cohort_id = 800001 AS cohort_managed
FROM event_instance_registration
WHERE instance_id = 900002
  AND person_id = 200002
  AND parent_registration_id is null;

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
  ARRAY[
    ROW(200003, null)::public.quick_event_registration_input,
    ROW(200005, null)::public.quick_event_registration_input
  ]
)
FROM event_instance instance
WHERE instance.id = 900002;

SELECT app_private.reconcile_event_instance_cohort_registrations(
  ARRAY[900002], ARRAY[200002]
);

SELECT tap.ok(
  (SELECT cohort_managed FROM _manager_removal_before)
  AND EXISTS (
    SELECT 1
    FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200002
      AND parent_registration_id is null
      AND registration_status = 'cancelled'
      AND source = 'manager'
      AND target_cohort_id is null
  ),
  'manager removal remains manager-controlled through reconciliation'
);

CREATE TEMP TABLE _manager_registration_before ON COMMIT DROP AS
SELECT registration_status = 'cancelled'
    AND source = 'cohort'
    AND target_cohort_id = 800001 AS cohort_managed
FROM event_instance_registration
WHERE instance_id = 900002
  AND person_id = 200004
  AND parent_registration_id is null;

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
  ARRAY[
    ROW(200003, null)::public.quick_event_registration_input,
    ROW(200004, null)::public.quick_event_registration_input,
    ROW(200005, null)::public.quick_event_registration_input
  ]
)
FROM event_instance instance
WHERE instance.id = 900002;

SELECT app_private.reconcile_event_instance_cohort_registrations(
  ARRAY[900002], ARRAY[200004]
);

SELECT tap.ok(
  (SELECT cohort_managed FROM _manager_registration_before)
  AND EXISTS (
    SELECT 1
    FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200004
      AND parent_registration_id is null
      AND registration_status = 'active'
      AND source = 'manager'
      AND target_cohort_id is null
  ),
  'manager registration overrides an automatically cancelled cohort registration'
);

INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES (2900001, 'Stats', 'Test', 'unspecified', '');

INSERT INTO event_instance (id, tenant_id, since, until) OVERRIDING SYSTEM VALUE
VALUES
  (9900001, 1000, now(), now() + interval '1 hour'),
  (9900002, 1000, now() + interval '2 hours', now() + interval '3 hours');

INSERT INTO event_instance_registration (tenant_id, instance_id, person_id, source, status)
VALUES (1000, 9900001, 2900001, 'manager', 'unknown');

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

INSERT INTO tenant_trainer (tenant_id, person_id, since)
VALUES (1000, 2900002, now() - interval '1 day');

INSERT INTO event_instance (id, tenant_id, since, until) OVERRIDING SYSTEM VALUE
VALUES (9900003, 1000, now() + interval '4 hours', now() + interval '5 hours');

INSERT INTO event_instance_trainer (tenant_id, instance_id, person_id, lessons_offered)
VALUES
  (1000, 900001, 2900002, 1),
  (1000, 9900001, 2900002, 0),
  (1000, 9900003, 2900004, 0);

UPDATE event_instance SET is_public = true WHERE id = 9900003;

INSERT INTO event_instance_registration (tenant_id, instance_id, person_id, source, status)
VALUES (1000, 9900003, 2900005, 'manager', 'unknown');

INSERT INTO event_trainer (tenant_id, event_id, person_id, lessons_offered)
VALUES (1000, 700001, 2900002, 1);

SELECT set_lesson_demand(eir.id, trainer.id, 1)
FROM event_instance_registration eir
JOIN event_instance_trainer trainer
  ON trainer.instance_id = eir.instance_id AND trainer.person_id = 2900002
WHERE eir.instance_id = 900001
  AND eir.person_id = 200002
  AND eir.parent_registration_id IS NULL;

SELECT set_config('jwt.claims.my_person_ids', '[2900002]', true);
GRANT USAGE ON SCHEMA tap TO trainer;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA tap TO trainer;
SET LOCAL ROLE trainer;

SELECT tap.lives_ok(
  $test$
    do $body$
    declare
      v_series_id bigint;
    begin
      insert into event_series (tenant_id, name)
      values (1000, 'Trainer-created series')
      returning id into v_series_id;

      update event_series
      set name = 'Trainer-renamed series'
      where id = v_series_id;

      update event_instance set series_id = v_series_id where id = 9900001;
      delete from event_series where id = v_series_id;

      if (select series_id is not null from event_instance where id = 9900001) then
        raise exception 'Deleting a series did not ungroup its event';
      end if;
    end;
    $body$
  $test$,
  'a trainer can create, rename and delete a series, leaving its events ungrouped'
);

SELECT tap.lives_ok(
  $$
    insert into event_instance_registration (tenant_id, instance_id, person_id, source, status)
    values (1000, 9900001, 2900003, 'manager', 'unknown')
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
    insert into event_instance_registration (tenant_id, instance_id, person_id, source, status)
    values (1000, 9900003, 2900003, 'manager', 'unknown')
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

UPDATE event_instance_trainer
SET lessons_offered = 1
WHERE instance_id = (SELECT id FROM _scheduled_lesson)
  AND person_id = 2900002;

SELECT set_lesson_demand(registration.id, trainer.id, 1)
FROM event_instance_registration registration
JOIN event_instance_trainer trainer ON trainer.instance_id = registration.instance_id
WHERE registration.instance_id = (SELECT id FROM _scheduled_lesson)
  AND registration.person_id = 2900003
  AND registration.parent_registration_id is null
  AND trainer.person_id = 2900002;

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_instance child
    WHERE child.id = (SELECT id FROM _scheduled_lesson)
      AND child.event_id is null
      AND child.series_id is null
      AND child.parent_id = 9900001
      AND 2900002 = any(child.manager_person_ids)
  ) AND EXISTS (
    SELECT 1
    FROM event_instance_registration registration
    WHERE registration.instance_id = (SELECT id FROM _scheduled_lesson)
      AND registration.person_id = 2900003
      AND registration.registration_status = 'active'
      AND registration.source = 'manager'
      AND EXISTS (
        SELECT 1
        FROM event_lesson_demand demand
        JOIN event_instance_trainer trainer ON trainer.id = demand.trainer_id
        WHERE demand.registration_id = registration.id
          AND trainer.instance_id = registration.instance_id
          AND trainer.person_id = 2900002
      )
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

SELECT set_config('jwt.claims.my_tenant_ids', '[1000]', true);
SELECT set_config('jwt.claims.my_person_ids', '[2900005]', true);
SELECT set_event_instance_registration(
  (SELECT id FROM _scheduled_lesson), 2900005, null, true
);
CREATE TEMP TABLE _self_registration_created ON COMMIT DROP AS
SELECT event_instance_remaining_person_spots(instance) = 0 AS at_capacity
FROM event_instance instance
WHERE id = (SELECT id FROM _scheduled_lesson);
SELECT set_event_instance_registration(
  (SELECT id FROM _scheduled_lesson), 2900005, null, false
);

SELECT tap.ok(
  (SELECT at_capacity FROM _self_registration_created)
  AND EXISTS (
    SELECT 1 FROM event_instance_registration
    WHERE instance_id = (SELECT id FROM _scheduled_lesson)
      AND person_id = 2900005
      AND registration_status = 'cancelled'
      AND source = 'self'
  ),
  'self-registration respects capacity and can be cancelled'
);

SELECT set_config('jwt.claims.my_person_ids', '[2900002]', true);

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
      AND root.source = 'manager'
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
      AND source = 'manager'
  ) AND (
    SELECT stats->>'TOTAL' = '2'
    FROM event_instance
    WHERE id = (SELECT id FROM _scheduled_lesson)
  ) AND EXISTS (
    SELECT 1
    FROM event_instance_trainer trainer
    JOIN event_lesson_demand demand ON demand.trainer_id = trainer.id
    JOIN event_instance_registration registration ON registration.id = demand.registration_id
    WHERE trainer.instance_id = (SELECT id FROM _scheduled_lesson)
      AND registration.registration_status = 'cancelled'
      AND event_instance_trainer_lessons_remaining(trainer) = 1
  ),
  'quick edit replaces a person registration and releases its lesson capacity'
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
      AND root.source = 'manager'
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
      AND source = 'manager'
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
  ARRAY[
    ROW(200001, null)::quick_event_registration_input,
    ROW(200002, null)::quick_event_registration_input,
    ROW(200003, null)::quick_event_registration_input
  ]
)
FROM event_instance instance
WHERE instance.id = 900001;

SELECT set_config(
  'jwt.claims.my_person_ids',
  (SELECT jsonb_build_array(person_id)::text
   FROM event_instance_registration
   WHERE instance_id = 900001
     AND parent_registration_id is null
     AND legacy_registration_id is not null
     AND registration_status = 'active'
     AND person_id is not null
   LIMIT 1),
  true
);
SELECT set_event_instance_registration(
  instance_id, person_id, null, true, 'Instance note'
)
FROM event_instance_registration
WHERE instance_id = 900001
  AND parent_registration_id is null
  AND legacy_registration_id is not null
  AND registration_status = 'active'
  AND person_id is not null
LIMIT 1;

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_instance_registration
    WHERE instance_id = 900001
      AND parent_registration_id is null
      AND person_id is not null
      AND legacy_registration_id is not null
      AND registration_status = 'active'
      AND note = 'Instance note'
  ),
  'event-backed quick edit and self-registration reuse bridge rows'
);

SELECT set_config('jwt.claims.my_person_ids', '[2900002]', true);

CREATE TEMP TABLE _stats_before_cancel ON COMMIT DROP AS
SELECT (stats->>'TOTAL')::int AS total
FROM event_instance
WHERE id = 900001;

UPDATE event_instance_registration
SET registration_status = 'cancelled'
WHERE instance_id = 900001 AND person_id = 200002;

CREATE TEMP TABLE _detached_audit_before ON COMMIT DROP AS
SELECT
  (SELECT jsonb_agg(to_jsonb(registration)
      - 'event_id' - 'legacy_registration_id' order by registration.id)
   FROM event_instance_registration registration
   WHERE registration.instance_id = 900001) as registrations,
  (SELECT jsonb_agg(to_jsonb(registration) order by registration.id)
   FROM event_instance_registration registration
   WHERE registration.instance_id = 900002) as sibling_registrations,
  (SELECT jsonb_agg(to_jsonb(demand) - 'event_id' order by demand.id)
   FROM event_lesson_demand demand
   JOIN event_instance_registration registration
     ON registration.id = demand.registration_id
   WHERE registration.instance_id = 900001) as demands,
  (SELECT jsonb_agg(to_jsonb(trainer) - 'event_id' order by trainer.id)
   FROM event_instance_trainer trainer
   WHERE trainer.instance_id = 900001) as trainers;

SET LOCAL ROLE trainer;

SELECT tap.is(
  (public.detach_event_instance(900001, 'Detached test event')).name,
  'Detached test event',
  'an assigned trainer can detach an instance with lesson demand'
);

RESET ROLE;

SELECT tap.ok(
  (SELECT
    before.registrations = (
      SELECT jsonb_agg(to_jsonb(registration)
        - 'event_id' - 'legacy_registration_id' order by registration.id)
      FROM event_instance_registration registration
      WHERE registration.instance_id = 900001
    ) AND before.sibling_registrations = (
      SELECT jsonb_agg(to_jsonb(registration) order by registration.id)
      FROM event_instance_registration registration
      WHERE registration.instance_id = 900002
    ) AND before.demands = (
      SELECT jsonb_agg(to_jsonb(demand) - 'event_id' order by demand.id)
      FROM event_lesson_demand demand
      JOIN event_instance_registration registration
        ON registration.id = demand.registration_id
      WHERE registration.instance_id = 900001
    ) AND before.trainers = (
      SELECT jsonb_agg(to_jsonb(trainer) - 'event_id' order by trainer.id)
      FROM event_instance_trainer trainer
      WHERE trainer.instance_id = 900001
    )
   FROM _detached_audit_before before)
  AND NOT EXISTS (
    SELECT 1
    FROM event_instance_registration registration
    JOIN event_instance instance ON instance.id = registration.instance_id
    JOIN event_registration legacy
      ON legacy.id = registration.legacy_registration_id
    WHERE registration.instance_id = 900001
      AND registration.parent_registration_id is null
      AND legacy.event_id <> instance.event_id
  ) AND EXISTS (
    SELECT 1
    FROM event_instance ei
    JOIN event_instance_registration eir ON eir.instance_id = ei.id
    JOIN event_lesson_demand d ON d.registration_id = eir.id
    JOIN event_instance_trainer trainer ON trainer.id = d.trainer_id
    WHERE ei.id = 900001
      AND ei.series_id is null
      AND trainer.instance_id = ei.id
      AND trainer.person_id = 2900002
      AND trainer.lessons_offered = 1
      AND EXISTS (
        SELECT 1
        FROM event_trainer legacy_trainer
        WHERE legacy_trainer.event_id = ei.event_id
          AND legacy_trainer.person_id = trainer.person_id
      )
      AND EXISTS (
        SELECT 1
        FROM event_target_cohort target
        WHERE target.event_id = ei.event_id
          AND target.cohort_id = 800001
      )
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
  'detaching preserves exact rows, siblings, and compatibility links'
);

SELECT tap.finish();

ROLLBACK;
