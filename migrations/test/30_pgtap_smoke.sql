BEGIN;

CREATE SCHEMA IF NOT EXISTS tap;
CREATE EXTENSION IF NOT EXISTS pgtap SCHEMA tap;
DO $$
BEGIN
  execute 'set search_path to ' || current_setting('search_path') || ',tap';
END
$$;

SELECT tap.plan(6);

-- Fixtures for testing event_instance_trainers(event_instance).
-- current_tenant_id() returns 1 when jwt.claims.tenant_id is unset.
--
-- People:
--   1001 = Alice, active tenant_trainer
--   1002 = Bob,   active tenant_trainer  (used as instance-level override)
--   1003 = Carol, expired tenant_trainer (used to test active_range filtering)

INSERT INTO tenant (id, name) VALUES (1, 'Test Tenant') ON CONFLICT (id) DO NOTHING;

INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
  VALUES (1001, 'Alice', 'Trainer', 'unspecified', ''),
         (1002, 'Bob',   'Trainer', 'unspecified', ''),
         (1003, 'Carol', 'Trainer', 'unspecified', '')
  ON CONFLICT (id) DO NOTHING;

INSERT INTO tenant_trainer (tenant_id, person_id, since)
  VALUES (1, 1001, now() - interval '1 year'),  -- active
         (1, 1002, now() - interval '1 year')   -- active
  ON CONFLICT DO NOTHING;

INSERT INTO tenant_trainer (tenant_id, person_id, since, until)
  VALUES (1, 1003, now() - interval '2 years', now() - interval '1 day')  -- expired
  ON CONFLICT DO NOTHING;

INSERT INTO event (id, tenant_id, type, name, location_text, description)
  VALUES (3001, 1, 'lesson', 'Test Event', '', '')
  ON CONFLICT (id) DO NOTHING;

-- instance 4001: no instance-level trainers → should fall back to event_trainer
-- instance 4002: has instance-level trainer (Bob/1002) → should NOT fall back
INSERT INTO event_instance (id, tenant_id, event_id, since, until)
  VALUES (4001, 1, 3001, now(), now() + interval '1 hour'),
         (4002, 1, 3001, now(), now() + interval '1 hour')
  ON CONFLICT (id) DO NOTHING;

-- event-level trainer: Alice (active) + Carol (expired) on the event
INSERT INTO event_trainer (tenant_id, event_id, person_id)
  VALUES (1, 3001, 1001),
         (1, 3001, 1003)
  ON CONFLICT DO NOTHING;

-- instance-level trainer: Bob on instance 4002 only
-- omit event_id — trigger _100_event_id fills it from instance_id
INSERT INTO event_instance_trainer (tenant_id, instance_id, person_id)
  VALUES (1, 4002, 1002)
  ON CONFLICT (instance_id, person_id) DO NOTHING;

-- Test 1: instance with NO instance-level trainers falls back to event_trainer
SELECT tap.is(
  (SELECT count(*)::int FROM public.event_instance_trainers(
    (SELECT i FROM event_instance i WHERE i.id = 4001)
  )),
  1,
  'instance with no instance trainers falls back to event_trainer (active only)'
);

-- Test 2: the fallback trainer is Alice (1001), not the expired Carol (1003)
SELECT tap.is(
  (SELECT person_id FROM public.event_instance_trainers(
    (SELECT i FROM event_instance i WHERE i.id = 4001)
  )),
  1001::bigint,
  'fallback returns the active event-level trainer'
);

-- Test 3: instance WITH instance-level trainer returns only that, no fallback
SELECT tap.is(
  (SELECT count(*)::int FROM public.event_instance_trainers(
    (SELECT i FROM event_instance i WHERE i.id = 4002)
  )),
  1,
  'instance with instance trainer returns only instance-level trainer (no fallback)'
);

-- Test 4: the result is the instance-level trainer Bob (1002), not event-level Alice (1001)
SELECT tap.is(
  (SELECT person_id FROM public.event_instance_trainers(
    (SELECT i FROM event_instance i WHERE i.id = 4002)
  )),
  1002::bigint,
  'instance-level trainer takes precedence over event-level trainer'
);

-- Test 5: expired tenant_trainer (Carol/1003) excluded from fallback results
SELECT tap.ok(
  NOT EXISTS (
    SELECT 1 FROM public.event_instance_trainers(
      (SELECT i FROM event_instance i WHERE i.id = 4001)
    ) WHERE person_id = 1003
  ),
  'expired tenant_trainer is excluded from fallback results'
);

-- Test 6: active trainer (Alice/1001) is included in fallback results
SELECT tap.ok(
  EXISTS (
    SELECT 1 FROM public.event_instance_trainers(
      (SELECT i FROM event_instance i WHERE i.id = 4001)
    ) WHERE person_id = 1001
  ),
  'active tenant_trainer is included in fallback results'
);

SELECT tap.finish();

ROLLBACK;
