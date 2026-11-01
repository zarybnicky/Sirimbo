BEGIN;

SET LOCAL search_path = public, tap;

SELECT tap.plan(5);

-- Fixtures for testing event_trainers(event).
--
-- People:
--   -3001 = Alice, active tenant_trainer
--   -3002 = Bob,   active tenant_trainer for a separate event
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

INSERT INTO event (id, tenant_id, series_id, since, until) OVERRIDING SYSTEM VALUE
  VALUES (-3001, -3000, -3000, now(), now() + interval '1 hour'),
         (-3002, -3000, -3000, now(), now() + interval '1 hour');

INSERT INTO event_trainer (tenant_id, event_id, person_id)
  VALUES (-3000, -3001, -3001),
         (-3000, -3001, -3003),
         (-3000, -3002, -3002);

SELECT tap.is(
  (SELECT person_id FROM public.event_trainers(
    (SELECT i FROM event i WHERE i.id = -3001)
  )),
  -3001::bigint,
  'expired trainers do not participate in occurrence lookups'
);

SELECT tap.is(
  (SELECT person_id FROM public.event_trainers(
    (SELECT i FROM event i WHERE i.id = -3002)
  )),
  -3002::bigint,
  'occurrence trainer lists are independent'
);

-- Adjacent periods must select the event-time rate, not the current/first row.
UPDATE tenant_trainer SET until = now(),
  member_price_45min = row(450, 'CZK')::price
WHERE tenant_id = -3000 AND person_id = -3001;
INSERT INTO tenant_trainer (tenant_id, person_id, since, member_price_45min)
VALUES (-3000, -3001, now(), row(900, 'CZK')::price);

SELECT tap.is(
  (SELECT member_price_45min_amount FROM public.event_trainers(
    (SELECT e FROM event e WHERE e.id = -3001)
  )),
  900::numeric,
  'compatibility wrapper uses the rate starting at the event boundary'
);

UPDATE event SET since = now() - interval '1 day', until = now() - interval '23 hours',
  type = 'lesson', stats = row(2, 2, 0, 0)::event_stats
WHERE id = -3001;

SELECT tap.is(
  (SELECT member_price_45min_amount FROM public.event_trainers(
    (SELECT e FROM event e WHERE e.id = -3001)
  )),
  450::numeric,
  'compatibility wrapper follows the historical rate when an event moves'
);

SELECT tap.is(
  (SELECT amount FROM public.event_approx_price(
    (SELECT e FROM event e WHERE e.id = -3001)
  )),
  300::numeric,
  'lesson estimate uses the historical rate and excludes expired trainers'
);

SELECT tap.finish(true);

ROLLBACK;
