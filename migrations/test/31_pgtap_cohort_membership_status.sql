BEGIN;

SET LOCAL search_path = public, tap;

SELECT tap.plan(27);

-- Fixtures
-- People:
--   200001 = Alice   (attendance history and self-cancellation)
--   200002 = Bob     (manager removal)
--   200003 = Charlie (self-registration)
--   200004 = Dana    (manager registration)
--   200005 = Evan    (exact-only cohort registration)
-- Cohort:  800001
-- Series:  700001
-- Instances:
--   900001 = past   (since = now()-2d, until = now()-1d)
--   900002 = future (since = now()+1d, until = now()+2d)

INSERT INTO tenant (id, name) VALUES (1000, 'Test Tenant') ON CONFLICT (id) DO NOTHING;
SELECT set_config('jwt.claims.tenant_id', '1000', true);

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

INSERT INTO event_series (id, tenant_id) OVERRIDING SYSTEM VALUE
  VALUES (700001, 1000)
  ON CONFLICT (id) DO NOTHING;

INSERT INTO event (
  id, tenant_id, series_id, since, until, name, type,
  location_text, is_visible, is_public, capacity, description, summary,
  is_locked, enable_notes, files_legacy
) OVERRIDING SYSTEM VALUE
  VALUES
    (900001, 1000, 700001, now() - interval '2 days', now() - interval '1 day',
      'Test Event', 'lesson', '', false, false, 0, '', '', false, false, ''),
    (900002, 1000, 700001, now() + interval '1 day', now() + interval '2 days',
      'Test Event', 'lesson', '', false, false, 0, '', '', false, false, '')
  ON CONFLICT (id) DO NOTHING;

INSERT INTO event_target_cohort (id, tenant_id, event_id, cohort_id) OVERRIDING SYSTEM VALUE
  VALUES (850101, 1000, 900001, 800001)
  ON CONFLICT (event_id, cohort_id) DO NOTHING;

INSERT INTO event_registration (
  tenant_id, event_id, person_id,
  target_cohort_id, source, status
)
SELECT 1000, instance.id, person.id, 800001, 'cohort', 'unknown'
FROM event instance
CROSS JOIN unnest(ARRAY[200001, 200002, 200003, 200004]::bigint[]) person(id)
WHERE instance.id IN (900001, 900002)
ON CONFLICT DO NOTHING;

-- Active memberships
INSERT INTO cohort_membership (id, tenant_id, cohort_id, person_id, since, status) OVERRIDING SYSTEM VALUE
  VALUES (860001, 1000, 800001, 200001, now() - interval '30 days', 'active'),
         (860002, 1000, 800001, 200002, now() - interval '30 days', 'active'),
         (860003, 1000, 800001, 200003, now() - interval '30 days', 'active'),
         (860004, 1000, 800001, 200004, now() - interval '30 days', 'active'),
         (860005, 1000, 800001, 200005, now() - interval '30 days', 'active')
  ON CONFLICT (id) DO NOTHING;

-- The form submits its complete participant list, including cohort members.
SELECT public.save_events(
  details => ROW(
    instance.parent_id, instance.name, instance.type,
    instance.location_id, instance.location_text,
    instance.capacity, instance.capacity_unit,
    instance.is_visible, instance.is_public, instance.has_public_details,
    instance.is_locked, true
  )::public.event_details_input,
  events => ARRAY[
    ROW(
      instance.id, instance.since, instance.until, instance.is_cancelled,
      ARRAY(SELECT ROW(person_id, null, cohort_id, false)::public.event_registration_input
        FROM cohort_membership WHERE cohort_id = 800001 AND status = 'active' AND active_range @> now())
    )::public.event_input
  ],
  cohort_ids => ARRAY[800001, 800001]
)
FROM event instance
WHERE instance.id = 900002;

SELECT tap.ok(
  (SELECT enable_notes FROM event WHERE id = 900002)
  AND EXISTS (
    SELECT 1
    FROM event_registration
    WHERE event_id = 900002
      AND person_id = 200005
      AND parent_registration_id is null
      AND registration_status = 'active'
      AND source = 'cohort'
      AND target_cohort_id = 800001
  ) AND 1 = (
    SELECT count(*)
    FROM event_target_cohort
    WHERE event_id = 900002
      AND cohort_id = 800001
  ) AND NOT EXISTS (
    SELECT 1
    FROM event_registration
    WHERE event_id = 900001
      AND person_id = 200005
  ),
  'instance settings and active-cohort registration are stored on the exact future instance'
);

UPDATE event_registration
SET status = 'attended'
WHERE event_id = 900001 AND person_id = 200001;

UPDATE cohort_membership SET until = now() - interval '1 second', status = 'expired' WHERE id = 860001;

SELECT tap.ok(
  EXISTS (
    SELECT 1 FROM event_registration
    WHERE event_id = 900002
      AND person_id = 200001
      AND registration_status = 'cancelled'
      AND source = 'cohort'
      AND target_cohort_id = 800001
      AND status = 'unknown'
  ) AND EXISTS (
    SELECT 1 FROM event_registration
    WHERE event_id = 900001
      AND person_id = 200001
      AND registration_status = 'active'
      AND status = 'attended'
  ),
  'membership expiry cancels only future cohort participation and preserves attendance history'
);

INSERT INTO cohort_membership (tenant_id, cohort_id, person_id, since, status)
  VALUES (1000, 800001, 200001, now(), 'active');

-- Restore participation for the self-cancellation check below.
UPDATE event_registration SET registration_status = 'active'
WHERE event_id = 900002 AND person_id = 200001;

SELECT set_config('jwt.claims.tenant_id', '1000', true);
SELECT set_config('jwt.claims.my_person_ids', '[200001]', true);

SET LOCAL ROLE anonymous;
SELECT set_event_registration(900002, 200001, null, false);
RESET ROLE;

SELECT app_private.reconcile_event_cohort_registrations(
  ARRAY[900002], ARRAY[200001]
);

UPDATE event SET is_public = true WHERE id = 900002;
UPDATE cohort_membership
SET until = now() - interval '1 second', status = 'expired'
WHERE id = 860003;
SELECT set_config('jwt.claims.my_tenant_ids', '[1000]', true);
SELECT set_config('jwt.claims.my_person_ids', '[200003]', true);
SET LOCAL ROLE member;
SELECT set_event_registration(900002, 200003, null, true);
RESET ROLE;

SELECT app_private.reconcile_event_cohort_registrations(
  ARRAY[900002], ARRAY[200003]
);

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_registration
    WHERE event_id = 900002
      AND person_id = 200001
      AND parent_registration_id is null
      AND registration_status = 'cancelled'
      AND source = 'self'
      AND target_cohort_id is null
  ) AND EXISTS (
    SELECT 1
    FROM event_registration
    WHERE event_id = 900002
      AND person_id = 200003
      AND parent_registration_id is null
      AND registration_status = 'active'
      AND source = 'self'
      AND target_cohort_id is null
  ),
  'reconciliation preserves explicit self-cancellation and self-registration'
);

INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES (2900001, 'Stats', 'Test', 'unspecified', '');

INSERT INTO event_series (id, tenant_id) OVERRIDING SYSTEM VALUE
VALUES (9900001, 1000);

INSERT INTO event (id, tenant_id, series_id, since, until) OVERRIDING SYSTEM VALUE
VALUES
  (9900001, 1000, 9900001, now(), now() + interval '1 hour'),
  (9900002, 1000, 9900001, now() + interval '2 hours', now() + interval '3 hours'),
  (9900099, 1000, 9900001, now() - interval '2 hours', now() - interval '1 hour');

INSERT INTO event_registration (tenant_id, event_id, person_id, source, status)
VALUES (1000, 9900001, 2900001, 'manager', 'unknown');

UPDATE event_registration
SET event_id = 9900002
WHERE event_id = 9900001 AND person_id = 2900001;

INSERT INTO event_registration (
  tenant_id, event_id, person_id, source, status
)
VALUES (1000, 9900099, 2900001, 'manager', 'attended');

SELECT tap.ok(
  (SELECT stats->>'TOTAL' = '0' FROM event WHERE id = 9900001)
    AND (SELECT stats->>'TOTAL' = '1' FROM event WHERE id = 9900002)
    AND (
      SELECT event_registration_last_attended(registration)
      FROM event_registration registration
      WHERE registration.event_id = 9900002
        AND registration.person_id = 2900001
    ) = (SELECT since FROM event WHERE id = 9900099),
  'moving attendance refreshes summaries and series history uses exact registrations'
);

UPDATE event SET parent_id = 9900001 WHERE id = 9900002;

INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES
  (2900002, 'Trainer', 'Policy', 'unspecified', ''),
  (2900003, 'Participant', 'Policy', 'unspecified', ''),
  (2900004, 'Other trainer', 'Policy', 'unspecified', ''),
  (2900005, 'Other participant', 'Policy', 'unspecified', '');

INSERT INTO tenant_trainer (tenant_id, person_id, since)
VALUES (1000, 2900002, now() - interval '1 day');

INSERT INTO event (id, tenant_id, series_id, since, until) OVERRIDING SYSTEM VALUE
VALUES
  (9900003, 1000, null, now() + interval '4 hours', now() + interval '5 hours'),
  (9900004, 1000, 700001, now() + interval '5 hours', now() + interval '6 hours');

INSERT INTO event (id, tenant_id, parent_id, since, until) OVERRIDING SYSTEM VALUE
VALUES (9900005, 1000, 9900003, now() + interval '4 hours', now() + interval '5 hours');

INSERT INTO event_trainer (tenant_id, event_id, person_id, lessons_offered)
VALUES
  (1000, 900001, 2900002, 1),
  (1000, 9900001, 2900002, 0),
  (1000, 9900004, 2900002, 0),
  (1000, 9900003, 2900004, 0);

UPDATE event SET is_public = true WHERE id = 9900003;

INSERT INTO event_registration (tenant_id, event_id, person_id, source, status)
VALUES (1000, 9900003, 2900005, 'manager', 'unknown');

SELECT set_config('jwt.claims.my_person_ids', '[2900002]', true);

WITH expected(scope, parent_id, ids) AS (VALUES
  ('all'::event_instance_range_scope, null::bigint, ARRAY[9900001,9900002,9900003,9900004,9900005]::bigint[]),
  ('top_level', null, ARRAY[9900001,9900003,9900004]),
  ('mine', null, ARRAY[9900001,9900004]),
  ('relevant', null, ARRAY[9900001,9900002,9900003,9900004]),
  ('all', 9900001, ARRAY[9900002])
)
SELECT tap.ok(NOT EXISTS (
  SELECT FROM expected
  WHERE ids IS DISTINCT FROM (
    SELECT array_agg(id ORDER BY id)
    FROM event_instances_for_range(null, now(), now() + interval '1 day',
      parent_id => expected.parent_id, scope => expected.scope)
    WHERE id BETWEEN 9900001 AND 9900005
  )
), 'event range scopes and parent filtering select the expected events');

SET LOCAL ROLE trainer;
SELECT set_lesson_request(eir.id, trainer.id, 1)
FROM event_registration eir
JOIN event_trainer trainer
  ON trainer.event_id = eir.event_id AND trainer.person_id = 2900002
WHERE eir.event_id = 900001
  AND eir.person_id = 200002
  AND eir.parent_registration_id IS NULL;
RESET ROLE;

SELECT set_config('jwt.claims.my_person_ids', '[2900002]', true);
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

      update event set series_id = v_series_id where id = 9900001;
      delete from event_series where id = v_series_id;

      if (select series_id is not null from event where id = 9900001) then
        raise exception 'Deleting a series did not ungroup its event';
      end if;
    end;
    $body$
  $test$,
  'a trainer can create, rename and delete a series, leaving its events ungrouped'
);

SELECT tap.lives_ok(
  $$
    insert into event_registration (tenant_id, event_id, person_id, source, status)
    values (1000, 9900001, 2900003, 'manager', 'unknown')
  $$,
  'a trainer can insert attendance for an instance they manage'
);

SELECT tap.ok(
  (
    SELECT updated.id = eir.id
      AND updated.status = 'attended'
      AND updated.attendance_note = 'ID-targeted attendance note'
    FROM event_registration eir
    CROSS JOIN LATERAL public.update_attendance(
      eir.id,
      'attended',
      'ID-targeted attendance note'
    ) updated
    WHERE eir.event_id = 900001 AND eir.person_id = 200002
  ),
  'an assigned trainer can update attendance by registration ID'
);

SELECT tap.is(
  (
    public.update_attendance(
      (
        SELECT id
        FROM event_registration
        WHERE event_id = 9900003 AND person_id = 2900005
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
    update event_registration
    set event_id = 9900003
    where event_id = 9900001 and person_id = 2900003
  $$,
  '42501'::char(5),
  null,
  'a trainer cannot move attendance to an instance they do not manage'
);

SELECT tap.throws_ok(
  $$
    insert into event_registration (tenant_id, event_id, person_id, source, status)
    values (1000, 9900003, 2900003, 'manager', 'unknown')
  $$,
  '42501'::char(5),
  null,
  'a trainer cannot insert attendance for an instance they do not manage'
);

DELETE FROM event WHERE id = 9900004;

RESET ROLE;

SELECT tap.ok(
  NOT EXISTS (SELECT 1 FROM event WHERE id = 9900004)
    AND EXISTS (SELECT 1 FROM event WHERE id = 900001)
    AND EXISTS (SELECT 1 FROM event WHERE id = 900002),
  'deleting an exact instance leaves sibling instances intact'
);

SET LOCAL ROLE trainer;

CREATE TEMP TABLE _scheduled_lesson ON COMMIT DROP AS
SELECT id
FROM public.save_events(
  details => ROW(
    9900001, 'Scheduled lesson copy', 'lesson'::event_type,
    null::bigint, 'Scheduled lesson', 2, 'registrations'::event_capacity_unit,
    false, true, false, false, true
  )::public.event_details_input,
  events => ARRAY[
    ROW(
      null::bigint,
      now() + interval '6 hours',
      now() + interval '7 hours',
      false,
      ARRAY[ROW(2900003, null, null, false)::public.event_registration_input]
    )::public.event_input
  ],
  trainers => ARRAY[ROW(2900002, 2)::public.event_trainer_input],
  cohort_ids => ARRAY[800002]::bigint[]
);

SELECT set_event_registration(
  (SELECT id FROM _scheduled_lesson),
  2900005,
  null,
  true,
  'Manager registration note',
  ARRAY[
    (SELECT id FROM event_trainer
     WHERE event_id = (SELECT id FROM _scheduled_lesson)
       AND person_id = 2900002)
  ],
  ARRAY[1]
);

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_registration registration
    WHERE registration.event_id = (SELECT id FROM _scheduled_lesson)
      AND registration.person_id = 2900005
      AND registration.registration_status = 'active'
      AND registration.source = 'manager'
      AND registration.note = 'Manager registration note'
      AND EXISTS (
        SELECT 1
        FROM event_lesson_request demand
        WHERE demand.registration_id = registration.id
          AND demand.lesson_count = 1
      )
  ),
  'an assigned trainer can create a managed registration with lesson demands'
);

SELECT set_event_registration(
  (SELECT id FROM _scheduled_lesson), 2900002, null, true
);
SELECT set_event_registration(
  (SELECT id FROM _scheduled_lesson), 2900002, null, false
);

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_registration registration
    WHERE registration.event_id = (SELECT id FROM _scheduled_lesson)
      AND registration.person_id = 2900002
      AND registration.registration_status = 'cancelled'
      AND registration.source = 'self'
  ),
  'a manager cancelling their own registration records a self action'
);

SELECT tap.throws_ok(
  $$SELECT set_event_registration(9900003, 2900003, null, true)$$,
  '42501'::char(5),
  null,
  'a trainer cannot use the registration function for another trainer''s instance'
);

CREATE TEMP TABLE _created_series_lesson ON COMMIT DROP AS
SELECT series_id, count(*) instance_count
FROM public.save_events(
  details => ROW(
    null::bigint, 'Created series', 'lesson'::event_type,
    null::bigint, '', 2, 'people'::event_capacity_unit,
    null, null, null, null, null
  )::public.event_details_input,
  events => ARRAY[
    ROW(
      null::bigint,
      now() + interval '2 days',
      now() + interval '2 days 1 hour',
      false,
      '{}'::public.event_registration_input[]
    )::public.event_input,
    ROW(
      null::bigint,
      now() + interval '3 days',
      now() + interval '3 days 1 hour',
      false,
      '{}'::public.event_registration_input[]
    )::public.event_input
  ],
  series => ROW(null::bigint, 'Created series')::public.event_series_input
)
GROUP BY series_id;

SELECT set_lesson_request(registration.id, trainer.id, 1)
FROM event_registration registration
JOIN event_trainer trainer ON trainer.event_id = registration.event_id
WHERE registration.event_id = (SELECT id FROM _scheduled_lesson)
  AND registration.person_id = 2900003
  AND registration.parent_registration_id is null
  AND trainer.person_id = 2900002;

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event child
    WHERE child.id = (SELECT id FROM _scheduled_lesson)
      AND child.series_id is null
      AND child.parent_id = 9900001
      AND child.name = 'Scheduled lesson copy'
      AND child.capacity = 2
      AND child.capacity_unit = 'registrations'
      AND child.is_visible = false
      AND child.is_public = true
      AND child.is_locked = false
      AND child.enable_notes = true
      AND child.description = ''
      AND child.summary = ''
      AND child.files_legacy = ''
      AND 2900002 = any(child.manager_person_ids)
  ),
  'event save preserves scheduled lesson settings and managers'
);

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_series series
    JOIN _created_series_lesson created ON created.series_id = series.id
    WHERE created.instance_count = 2
      AND series.name = 'Created series'
      AND NOT EXISTS (
        SELECT 1
        FROM event instance
        WHERE instance.series_id = created.series_id
          AND (instance.is_visible, instance.is_public,
            instance.has_public_details, instance.is_locked, instance.enable_notes)
            IS DISTINCT FROM (true, false, false, false, false)
      )
  ),
  'event save creates a series with two instances and default settings'
);

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_target_cohort target
    WHERE target.event_id = (SELECT id FROM _scheduled_lesson)
      AND target.cohort_id = 800002
  ),
  'event save preserves the scheduled lesson cohort target'
);

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_trainer trainer
    WHERE trainer.event_id = (SELECT id FROM _scheduled_lesson)
      AND trainer.person_id = 2900002
      AND trainer.lessons_offered = 2
      AND event_trainer_lessons_remaining(trainer) = 0
  ),
  'scheduled trainer has two offered lessons and no remaining lessons'
);

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_registration registration
    WHERE registration.event_id = (SELECT id FROM _scheduled_lesson)
      AND registration.person_id = 2900003
      AND registration.registration_status = 'active'
      AND registration.source = 'manager'
      AND EXISTS (
        SELECT 1
        FROM event_lesson_request demand
        JOIN event_trainer trainer ON trainer.id = demand.trainer_id
        WHERE demand.registration_id = registration.id
          AND trainer.event_id = registration.event_id
          AND trainer.person_id = 2900002
      )
  ),
  'event save preserves managed registration and lesson demand'
);

SELECT tap.lives_ok(
  format(
    'insert into event (tenant_id, parent_id, since, until) values (1000, %s, now() + interval ''8 hours'', now() + interval ''9 hours'')',
    (SELECT id FROM _scheduled_lesson)
  ),
  'event instance schedules may be nested'
);

SELECT tap.throws_ok(
  'delete from event where id = 9900001',
  '23503'::char(5),
  null,
  'a schedule parent cannot be deleted while it has lessons'
);

RESET ROLE;

INSERT INTO couple (id, man_id, woman_id, since) OVERRIDING SYSTEM VALUE
VALUES (2950001, 2900003, 2900005, now());

SELECT set_config('jwt.claims.my_tenant_ids', '[1000]', true);
SELECT set_config('jwt.claims.my_person_ids', '[2900005]', true);
GRANT SELECT ON _scheduled_lesson TO member;
SET LOCAL ROLE member;
SELECT set_event_registration(
  (SELECT id FROM _scheduled_lesson),
  2900005,
  null,
  true,
  'Self-registration note',
  ARRAY[
    (SELECT id FROM event_trainer
     WHERE event_id = (SELECT id FROM _scheduled_lesson)
       AND person_id = 2900002)
  ],
  ARRAY[1]
);
RESET ROLE;
CREATE TEMP TABLE _self_registration_created ON COMMIT DROP AS
SELECT event_remaining_person_spots(instance) = 0 AS at_capacity
FROM event instance
WHERE id = (SELECT id FROM _scheduled_lesson);
SET LOCAL ROLE member;
SELECT set_event_registration(
  (SELECT id FROM _scheduled_lesson), 2900005, null, false
);
RESET ROLE;

SELECT tap.ok(
  (SELECT at_capacity FROM _self_registration_created)
  AND EXISTS (
    SELECT 1 FROM event_registration
    WHERE event_id = (SELECT id FROM _scheduled_lesson)
      AND person_id = 2900005
      AND registration_status = 'cancelled'
      AND source = 'self'
      AND note = 'Self-registration note'
      AND EXISTS (
        SELECT 1
        FROM event_lesson_request demand
        JOIN event_trainer trainer ON trainer.id = demand.trainer_id
        WHERE demand.registration_id = event_registration.id
          AND demand.lesson_count = 1
          AND trainer.person_id = 2900002
      )
  ),
  'self-registration stores its note and lesson demands and can be cancelled'
);

SELECT set_config('jwt.claims.my_person_ids', '[2900002]', true);

SELECT public.save_events(
  details => ROW(
    instance.parent_id, 'Scheduled lesson renamed', instance.type,
    instance.location_id, instance.location_text,
    4, 'registrations'::event_capacity_unit,
    instance.is_visible, instance.is_public, instance.has_public_details,
    true, true
  )::public.event_details_input,
  events => ARRAY[
    ROW(
      instance.id, instance.since, instance.until, instance.is_cancelled,
      ARRAY[ROW(null, 2950001, null, false)::public.event_registration_input]
    )::public.event_input,
    ROW(
      null::bigint,
      instance.since + interval '1 week',
      instance.until + interval '1 week',
      false,
      ARRAY[ROW(null, 2950001, null, false)::public.event_registration_input]
    )::public.event_input
  ],
  trainers => ARRAY[ROW(2900002, 2)::public.event_trainer_input],
  cohort_ids => ARRAY[800002]::bigint[],
  series => ROW(null::bigint, 'Scheduled lesson renamed')::public.event_series_input
)
FROM event instance
WHERE instance.id = (SELECT id FROM _scheduled_lesson);

SELECT tap.ok(
  EXISTS (
    SELECT 1
    FROM event_registration root
    WHERE root.event_id = (SELECT id FROM _scheduled_lesson)
      AND root.couple_id = 2950001
      AND root.registration_status = 'active'
      AND root.source = 'manager'
      AND 2 = (
        SELECT count(*)
        FROM event_registration child
        WHERE child.parent_registration_id = root.id
          AND child.registration_status = 'active'
      )
  ) AND EXISTS (
    SELECT 1
    FROM event_registration
    WHERE event_id = (SELECT id FROM _scheduled_lesson)
      AND person_id = 2900003
      AND parent_registration_id is not null
      AND registration_status = 'active'
      AND source = 'manager'
  ) AND EXISTS (
    SELECT 1
    FROM event
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
    FROM event copy
    JOIN event original
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
        FROM event_target_cohort target
        WHERE target.event_id = copy.id
          AND target.cohort_id = 800002
      )
      AND EXISTS (
        SELECT 1
        FROM event_trainer trainer
        WHERE trainer.event_id = copy.id
          AND trainer.person_id = 2900002
          AND trainer.lessons_offered = 2
      )
      AND EXISTS (
        SELECT 1
        FROM event_registration registration
        WHERE registration.event_id = copy.id
          AND registration.couple_id = 2950001
          AND registration.registration_status = 'active'
      )
  ) AND EXISTS (
    SELECT 1
    FROM event_trainer trainer
    JOIN event_lesson_request demand ON demand.trainer_id = trainer.id
    JOIN event_registration registration ON registration.id = demand.registration_id
    WHERE trainer.event_id = (SELECT id FROM _scheduled_lesson)
      AND registration.registration_status = 'active'
      AND registration.parent_registration_id is not null
      AND event_trainer_lessons_remaining(trainer) = 0
  ),
  'event save updates exact settings and replaces registrations consistently'
);

SELECT public.save_events(
  details => ROW(
    instance.parent_id, instance.name, instance.type,
    instance.location_id, instance.location_text,
    instance.capacity, instance.capacity_unit,
    instance.is_visible, instance.is_public, instance.has_public_details,
    instance.is_locked, instance.enable_notes
  )::public.event_details_input,
  events => ARRAY[
    ROW(
      instance.id, instance.since, instance.until, instance.is_cancelled,
      ARRAY[ROW(null, 2950001, null, false)::public.event_registration_input]
    )::public.event_input,
    ROW(
      null::bigint,
      instance.since + interval '2 weeks',
      instance.until + interval '2 weeks',
      false,
      ARRAY[ROW(null, 2950001, null, false)::public.event_registration_input]
    )::public.event_input
  ],
  trainers => ARRAY[ROW(2900002, 2)::public.event_trainer_input],
  cohort_ids => ARRAY[800002]::bigint[],
  series => ROW(instance.series_id, null::text)::public.event_series_input
)
FROM event instance
WHERE instance.id = (SELECT id FROM _scheduled_lesson);

SELECT tap.ok(
  (
    SELECT count(*) = 3
    FROM event member
    JOIN event original
      ON original.id = (SELECT id FROM _scheduled_lesson)
      AND member.series_id = original.series_id
  ) AND EXISTS (
    SELECT 1
    FROM event copy
    JOIN event original
      ON original.id = (SELECT id FROM _scheduled_lesson)
      AND copy.series_id = original.series_id
    WHERE copy.id <> original.id
      AND copy.since = original.since + interval '2 weeks'
      AND copy.until = original.until + interval '2 weeks'
  ),
  'event save adds a date to an existing series without replacing it'
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
    FROM event instance
    CROSS JOIN LATERAL (SELECT (instance.series_info).*) info
    WHERE instance.id = (SELECT id FROM _scheduled_lesson)
  ),
  'series info reports identity, range, position and length without loading sibling events'
);

SELECT set_config('jwt.claims.my_person_ids', '[2900002]', true);

SET LOCAL ROLE trainer;

UPDATE event
SET series_id = null
WHERE id = 900001;

RESET ROLE;

SELECT tap.ok(
  (SELECT series_id is null FROM event WHERE id = 900001),
  'an assigned trainer can detach an instance through the normal update policy'
);

SELECT tap.finish(true);

ROLLBACK;
