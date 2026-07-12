BEGIN;

CREATE SCHEMA IF NOT EXISTS tap;
CREATE EXTENSION IF NOT EXISTS pgtap SCHEMA tap;
DO $$
BEGIN
  execute 'set search_path to ' || current_setting('search_path') || ',tap';
END
$$;

SELECT tap.plan(25);

SELECT tap.ok(
  NOT EXISTS (
    SELECT 1
    FROM pg_catalog.pg_enum enum
    JOIN pg_catalog.pg_type type ON type.oid = enum.enumtypid
    JOIN pg_catalog.pg_namespace namespace ON namespace.oid = type.typnamespace
    WHERE namespace.nspname = 'public'
      AND type.typname = 'attendance_type'
      AND enum.enumlabel = 'cancelled'
  ),
  'registration cancellation is not an attendance status'
);

SELECT tap.ok(
  to_regprocedure('public.delete_event_instance(bigint)') is null
    AND to_regprocedure('public.quick_create_events(public.quick_event_input[])') is null
    AND to_regprocedure(
      'public.upsert_event(public.event_type_input,public.event_instance_type_input[],public.event_trainer_type_input[])'
    ) is null
    AND to_regprocedure('public.tg_event__propagate_to_instances()') is null
    AND NOT has_table_privilege('anonymous', 'public.event', 'INSERT')
    AND NOT has_table_privilege('anonymous', 'public.event', 'UPDATE')
    AND NOT has_table_privilege('anonymous', 'public.event', 'DELETE')
    AND NOT has_table_privilege('anonymous', 'public.event', 'SELECT')
    AND NOT has_table_privilege('anonymous', 'public.event_registration', 'SELECT')
    AND NOT has_table_privilege('anonymous', 'public.event_target_cohort', 'SELECT')
    AND NOT has_table_privilege('anonymous', 'public.event_trainer', 'SELECT')
    AND to_regprocedure('public.event_my_registrations(public.event)') is null
    AND to_regprocedure(
      'public.event_instance_my_registrations(public.event_instance)'
    ) is null
    AND to_regprocedure(
      'public.event_instance_registrations(public.event_instance)'
    ) is null
    AND to_regprocedure('public.tg_event_instance__fill_defaults()') is null
    AND to_regprocedure('public.tg_event_instance__pin_overrides()') is null
    AND to_regprocedure('app_private.tg_eir__legacy_created_at()') is null
    AND to_regprocedure('app_private.can_trainer_edit_event(bigint)') is null
    AND to_regtype('public.event_type_input') is null
    AND to_regtype('public.event_instance_type_input') is null
    AND to_regtype('public.event_instance_trainer_type_input') is null
    AND to_regtype('public.event_trainer_type_input') is null
    AND NOT EXISTS (
      SELECT 1
      FROM pg_catalog.pg_type type
      JOIN pg_catalog.pg_namespace namespace ON namespace.oid = type.typnamespace
      JOIN pg_catalog.pg_class class ON class.oid = type.typrelid
      JOIN pg_catalog.pg_attribute attribute ON attribute.attrelid = class.oid
      WHERE namespace.nspname = 'public'
        AND type.typname = 'event_overlaps_conflict'
        AND attribute.attname IN ('first_event_id', 'second_event_id')
        AND NOT attribute.attisdropped
    ),
  'legacy event tables and helpers have no application surface'
);

-- Fixtures
-- People:
--   200001 = Alice   (legacy bridge, attendance history and self-cancellation)
--   200002 = Bob     (manager removal)
--   200003 = Charlie (self-registration)
--   200004 = Dana    (manager registration)
--   200005 = Evan    (exact-only cohort registration)
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
  VALUES (800001, 1000, 'Test Cohort', '000000'),
         (800002, 1000, 'Empty Test Cohort', '000000')
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

INSERT INTO event_instance (
  id, tenant_id, event_id, series_id, since, until, name, type,
  location_text, is_visible, is_public, capacity, description, summary,
  is_locked, enable_notes, files_legacy
) OVERRIDING SYSTEM VALUE
  VALUES
    (900001, 1000, 700001, 700001, now() - interval '2 days', now() - interval '1 day',
      'Test Event', 'lesson', '', false, false, 0, '', '', false, false, ''),
    (900002, 1000, 700001, 700001, now() + interval '1 day', now() + interval '2 days',
      'Test Event', 'lesson', '', false, false, 0, '', '', false, false, '')
  ON CONFLICT (id) DO NOTHING;

INSERT INTO event_instance_target_cohort (id, tenant_id, instance_id, cohort_id) OVERRIDING SYSTEM VALUE
  VALUES (850101, 1000, 900001, 800001)
  ON CONFLICT (instance_id, cohort_id) DO NOTHING;

-- Historical legacy rows and their existing exact bridges are seeded separately;
-- changes to the frozen legacy table no longer synchronize exact registrations.
INSERT INTO event_registration (tenant_id, event_id, target_cohort_id, person_id)
SELECT 1000, 700001, 850001, person_id
FROM unnest(ARRAY[200001, 200002, 200003, 200004]::bigint[]) person(person_id)
ON CONFLICT ON CONSTRAINT event_registration_unique_event_person_couple_key DO NOTHING;

INSERT INTO event_instance_registration (
  legacy_registration_id, tenant_id, instance_id, person_id,
  target_cohort_id, source, status
)
SELECT registration.id, registration.tenant_id, instance.id,
  registration.person_id, 800001, 'cohort', 'unknown'
FROM event_registration registration
JOIN event_instance instance ON instance.event_id = registration.event_id
WHERE registration.event_id = 700001
  AND registration.person_id = ANY (ARRAY[200001, 200002, 200003, 200004]::bigint[])
ON CONFLICT DO NOTHING;

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
  p_cohort_ids => ARRAY[800001, 800001],
  p_enable_notes => true
)
FROM event_instance instance
WHERE instance.id = 900002;

SELECT tap.ok(
  (SELECT enable_notes FROM event_instance WHERE id = 900002)
  AND EXISTS (
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
  'instance settings and active-cohort registration are stored on the exact future instance'
);

UPDATE event_instance_registration
SET status = 'attended'
WHERE instance_id = 900001 AND person_id = 200001;

UPDATE cohort_membership SET until = now() - interval '1 second', status = 'expired' WHERE id = 860001;

SELECT tap.ok(
  EXISTS (
    SELECT 1 FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200001
      AND registration_status = 'cancelled'
      AND source = 'cohort'
      AND target_cohort_id = 800001
      AND status = 'unknown'
  ) AND EXISTS (
    SELECT 1 FROM event_instance_registration
    WHERE instance_id = 900001
      AND person_id = 200001
      AND registration_status = 'active'
      AND status = 'attended'
  ),
  'membership expiry cancels only future cohort participation and preserves attendance history'
);

INSERT INTO cohort_membership (tenant_id, cohort_id, person_id, since, status)
  VALUES (1000, 800001, 200001, now(), 'active');

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

UPDATE event_instance SET is_public = true WHERE id = 900002;
UPDATE cohort_membership
SET until = now() - interval '1 second', status = 'expired'
WHERE id = 860003;
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
      AND person_id = 200001
      AND parent_registration_id is null
      AND registration_status = 'cancelled'
      AND source = 'self'
      AND target_cohort_id is null
  ) AND EXISTS (
    SELECT 1
    FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200003
      AND parent_registration_id is null
      AND registration_status = 'active'
      AND source = 'self'
      AND target_cohort_id is null
  ),
  'reconciliation preserves explicit self-cancellation and self-registration'
);

UPDATE cohort_membership
SET until = now() - interval '1 second', status = 'expired'
WHERE id = 860004;

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
  EXISTS (
    SELECT 1
    FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200002
      AND parent_registration_id is null
      AND registration_status = 'cancelled'
      AND source = 'manager'
      AND target_cohort_id is null
  ) AND EXISTS (
    SELECT 1
    FROM event_instance_registration
    WHERE instance_id = 900002
      AND person_id = 200004
      AND parent_registration_id is null
      AND registration_status = 'active'
      AND source = 'manager'
      AND target_cohort_id is null
  ),
  'manager edits remain authoritative through cohort reconciliation'
);

INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES (2900001, 'Stats', 'Test', 'unspecified', '');

INSERT INTO event_series (id, tenant_id) OVERRIDING SYSTEM VALUE
VALUES (9900001, 1000);

INSERT INTO event_instance (id, tenant_id, series_id, since, until) OVERRIDING SYSTEM VALUE
VALUES
  (9900001, 1000, 9900001, now(), now() + interval '1 hour'),
  (9900002, 1000, 9900001, now() + interval '2 hours', now() + interval '3 hours'),
  (9900099, 1000, 9900001, now() - interval '2 hours', now() - interval '1 hour');

INSERT INTO event_instance_registration (tenant_id, instance_id, person_id, source, status)
VALUES (1000, 9900001, 2900001, 'manager', 'unknown');

UPDATE event_instance_registration
SET instance_id = 9900002
WHERE instance_id = 9900001 AND person_id = 2900001;

INSERT INTO event_instance_registration (
  tenant_id, instance_id, person_id, source, status
)
VALUES (1000, 9900099, 2900001, 'manager', 'attended');

SELECT tap.ok(
  (SELECT stats->>'TOTAL' = '0' FROM event_instance WHERE id = 9900001)
    AND (SELECT stats->>'TOTAL' = '1' FROM event_instance WHERE id = 9900002)
    AND (
      SELECT event_instance_registration_last_attended(registration)
      FROM event_instance_registration registration
      WHERE registration.instance_id = 9900002
        AND registration.person_id = 2900001
    ) = (SELECT since FROM event_instance WHERE id = 9900099),
  'moving attendance refreshes summaries and series history uses exact registrations'
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

INSERT INTO event_instance (
  id, tenant_id, event_id, series_id, since, until
) OVERRIDING SYSTEM VALUE
VALUES
  (9900003, 1000, null, null, now() + interval '4 hours', now() + interval '5 hours'),
  (9900004, 1000, 700001, 700001, now() + interval '5 hours', now() + interval '6 hours');

INSERT INTO event_instance_trainer (tenant_id, instance_id, person_id, lessons_offered)
VALUES
  (1000, 900001, 2900002, 1),
  (1000, 9900001, 2900002, 0),
  (1000, 9900004, 2900002, 0),
  (1000, 9900003, 2900004, 0);

UPDATE event_instance SET is_public = true WHERE id = 9900003;

INSERT INTO event_instance_registration (tenant_id, instance_id, person_id, source, status)
VALUES (1000, 9900003, 2900005, 'manager', 'unknown');

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

DELETE FROM event_instance WHERE id = 9900004;

RESET ROLE;

SELECT tap.ok(
  NOT EXISTS (SELECT 1 FROM event_instance WHERE id = 9900004)
    AND EXISTS (SELECT 1 FROM event WHERE id = 700001)
    AND EXISTS (SELECT 1 FROM event_instance WHERE id = 900001)
    AND EXISTS (SELECT 1 FROM event_instance WHERE id = 900002),
  'deleting an exact instance leaves its legacy row and sibling instances intact'
);

SET LOCAL ROLE trainer;

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
  parent_id => 9900001,
  p_is_visible => false,
  p_is_public => true,
  p_is_locked => false,
  p_enable_notes => true,
  p_name => 'Scheduled lesson copy',
  p_capacity => 2,
  p_capacity_unit => 'registrations',
  p_description => 'Copied description',
  p_summary => 'Copied summary',
  p_files_legacy => 'copied-file',
  p_cohort_ids => ARRAY[800002]::bigint[],
  p_trainer_lessons_offered => ARRAY[2]
);

CREATE TEMP TABLE _created_series_lesson ON COMMIT DROP AS
SELECT series_id, count(*) instance_count
FROM public.quick_create_event_instances(
  ARRAY[
    ROW(
      now() + interval '2 days',
      now() + interval '2 days 1 hour',
      'lesson'::event_type,
      null::bigint,
      '',
      '{}'::bigint[],
      '{}'::quick_event_registration_input[]
    )::quick_event_input
  ],
  p_name => 'Created series',
  p_copies => ARRAY[
    ROW(
      now() + interval '3 days',
      now() + interval '3 days 1 hour',
      'lesson'::event_type,
      null::bigint,
      '',
      '{}'::bigint[],
      '{}'::quick_event_registration_input[]
    )::quick_event_input
  ]
)
GROUP BY series_id;

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
      AND child.name = 'Scheduled lesson copy'
      AND child.capacity = 2
      AND child.capacity_unit = 'registrations'
      AND child.is_visible = false
      AND child.is_public = true
      AND child.is_locked = false
      AND child.enable_notes = true
      AND child.description = 'Copied description'
      AND child.summary = 'Copied summary'
      AND child.files_legacy = 'copied-file'
      AND 2900002 = any(child.manager_person_ids)
  ) AND EXISTS (
    SELECT 1
    FROM event_series series
    JOIN _created_series_lesson created ON created.series_id = series.id
    WHERE created.instance_count = 2
      AND series.name = 'Created series'
  ) AND EXISTS (
    SELECT 1
    FROM event_instance_target_cohort target
    WHERE target.instance_id = (SELECT id FROM _scheduled_lesson)
      AND target.cohort_id = 800002
  ) AND EXISTS (
    SELECT 1
    FROM event_instance_trainer trainer
    WHERE trainer.instance_id = (SELECT id FROM _scheduled_lesson)
      AND trainer.person_id = 2900002
      AND trainer.lessons_offered = 2
      AND event_instance_trainer_lessons_remaining(trainer) = 1
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
  (SELECT id FROM _scheduled_lesson),
  2900005,
  null,
  true,
  'Self-registration note',
  ARRAY[
    (SELECT id FROM event_instance_trainer
     WHERE instance_id = (SELECT id FROM _scheduled_lesson)
       AND person_id = 2900002)
  ],
  ARRAY[1]
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
      AND note = 'Self-registration note'
      AND EXISTS (
        SELECT 1
        FROM event_lesson_demand demand
        JOIN event_instance_trainer trainer ON trainer.id = demand.trainer_id
        WHERE demand.registration_id = event_instance_registration.id
          AND demand.lesson_count = 1
          AND trainer.person_id = 2900002
      )
  ),
  'self-registration stores its note and lesson demands and can be cancelled'
);

SELECT set_config('jwt.claims.my_person_ids', '[2900002]', true);

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
  ARRAY[2900002]::bigint[],
  ARRAY[ROW(null, 2950001)::quick_event_registration_input],
  4,
  'registrations',
  true,
  ARRAY[2],
  ARRAY[800002]::bigint[],
  true,
  ARRAY[
    ROW(
      instance.since + interval '1 week',
      instance.until + interval '1 week',
      instance.type,
      instance.location_id,
      instance.location_text,
      ARRAY[2900002]::bigint[],
      ARRAY[ROW(null, 2950001)::quick_event_registration_input]
    )::quick_event_input
  ]
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
  ) AND EXISTS (
    SELECT 1
    FROM event_instance
    WHERE id = (SELECT id FROM _scheduled_lesson)
      AND name = 'Scheduled lesson renamed'
      AND capacity = 4
      AND capacity_unit = 'registrations'
      AND is_locked
      AND enable_notes
      AND stats->>'TOTAL' = '2'
      AND series_id is not null
  ) AND EXISTS (
    SELECT 1
    FROM event_instance copy
    JOIN event_instance original
      ON original.id = (SELECT id FROM _scheduled_lesson)
      AND copy.series_id = original.series_id
    WHERE copy.id <> original.id
      AND copy.since = original.since + interval '1 week'
      AND copy.until = original.until + interval '1 week'
      AND copy.name = original.name
      AND copy.description = original.description
      AND copy.summary = original.summary
      AND copy.files_legacy = original.files_legacy
      AND EXISTS (
        SELECT 1
        FROM event_instance_target_cohort target
        WHERE target.instance_id = copy.id
          AND target.cohort_id = 800002
      )
      AND EXISTS (
        SELECT 1
        FROM event_instance_trainer trainer
        WHERE trainer.instance_id = copy.id
          AND trainer.person_id = 2900002
          AND trainer.lessons_offered = 2
      )
      AND EXISTS (
        SELECT 1
        FROM event_instance_registration registration
        WHERE registration.instance_id = copy.id
          AND registration.couple_id = 2950001
          AND registration.registration_status = 'active'
      )
  ) AND EXISTS (
    SELECT 1
    FROM event_instance_trainer trainer
    JOIN event_lesson_demand demand ON demand.trainer_id = trainer.id
    JOIN event_instance_registration registration ON registration.id = demand.registration_id
    WHERE trainer.instance_id = (SELECT id FROM _scheduled_lesson)
      AND registration.registration_status = 'cancelled'
      AND event_instance_trainer_lessons_remaining(trainer) = 2
  ),
  'quick edit updates exact settings and replaces registrations consistently'
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
  p_copies => ARRAY[
    ROW(
      instance.since + interval '2 weeks',
      instance.until + interval '2 weeks',
      instance.type,
      instance.location_id,
      instance.location_text,
      '{}'::bigint[],
      '{}'::quick_event_registration_input[]
    )::quick_event_input
  ]
)
FROM event_instance instance
WHERE instance.id = (SELECT id FROM _scheduled_lesson);

SELECT tap.ok(
  (
    SELECT count(*) = 3
    FROM event_instance member
    JOIN event_instance original
      ON original.id = (SELECT id FROM _scheduled_lesson)
      AND member.series_id = original.series_id
  ) AND EXISTS (
    SELECT 1
    FROM event_instance copy
    JOIN event_instance original
      ON original.id = (SELECT id FROM _scheduled_lesson)
      AND copy.series_id = original.series_id
    WHERE copy.id <> original.id
      AND copy.since = original.since + interval '2 weeks'
      AND copy.until = original.until + interval '2 weeks'
  ),
  'quick edit adds copies to an existing series without replacing it'
);

SELECT tap.ok(
  (
    SELECT
      info.id = instance.series_id
      AND info.name = instance.name
      AND info.position = 1
      AND info.length = 3
      AND info.since = instance.since
      AND info.until = instance.until + interval '2 weeks'
    FROM event_instance instance
    CROSS JOIN LATERAL event_instance_series_info(instance) info
    WHERE instance.id = (SELECT id FROM _scheduled_lesson)
  ),
  'series info reports identity, range, position and length without loading sibling events'
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

SET LOCAL ROLE trainer;

UPDATE event_instance
SET series_id = null
WHERE id = 900001;

RESET ROLE;

SELECT tap.ok(
  (SELECT series_id is null FROM event_instance WHERE id = 900001),
  'an assigned trainer can detach an instance through the normal update policy'
);

SELECT tap.finish();

ROLLBACK;
