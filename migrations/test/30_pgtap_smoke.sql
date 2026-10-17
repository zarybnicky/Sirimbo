BEGIN;

SET LOCAL search_path = public, tap;

SELECT tap.plan(2);

-- Fixtures for testing event_instance_trainers(event_instance).
--
-- People:
--   -3001 = Alice, active tenant_trainer
--   -3002 = Bob,   active tenant_trainer  (used as instance-level override)
--   -3003 = Carol, expired tenant_trainer (used to test active_range filtering)

INSERT INTO tenant (id, name) VALUES (-3000, 'Trainer smoke test');
SELECT set_config('jwt.claims.tenant_id', '-3000', true);

INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
  VALUES (-3001, 'Alice', 'Trainer', 'unspecified', ''),
         (-3002, 'Bob',   'Trainer', 'unspecified', ''),
         (-3003, 'Carol', 'Trainer', 'unspecified', '');

INSERT INTO tenant_trainer (tenant_id, person_id, since)
  VALUES (-3000, -3001, now() - interval '1 year'),
         (-3000, -3002, now() - interval '1 year');

INSERT INTO tenant_trainer (tenant_id, person_id, since, until)
  VALUES (-3000, -3003, now() - interval '2 years', now() - interval '1 day');

INSERT INTO event_series (id, tenant_id) OVERRIDING SYSTEM VALUE
  VALUES (-3000, -3000);

INSERT INTO event_instance (id, tenant_id, series_id, since, until) OVERRIDING SYSTEM VALUE
  VALUES (-3001, -3000, -3000, now(), now() + interval '1 hour'),
         (-3002, -3000, -3000, now(), now() + interval '1 hour');

INSERT INTO event_instance_trainer (tenant_id, instance_id, person_id)
  VALUES (-3000, -3001, -3001),
         (-3000, -3001, -3003),
         (-3000, -3002, -3002);

SELECT tap.is(
  (SELECT person_id FROM public.event_instance_trainers(
    (SELECT i FROM event_instance i WHERE i.id = -3001)
  )),
  -3001::bigint,
  'expired trainers do not participate in occurrence lookups'
);

SELECT tap.is(
  (SELECT person_id FROM public.event_instance_trainers(
    (SELECT i FROM event_instance i WHERE i.id = -3002)
  )),
  -3002::bigint,
  'occurrence trainer lists are independent'
);

SELECT tap.finish(true);

ROLLBACK;
