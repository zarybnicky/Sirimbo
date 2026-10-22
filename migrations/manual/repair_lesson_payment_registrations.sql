BEGIN;
SET LOCAL jwt.claims.tenant_id = '2';

CREATE TEMP TABLE repair_instances ON COMMIT DROP AS
SELECT event_id AS id FROM payment
WHERE tenant_id=2 AND id=ANY('{14558,15965,14709,14814,15043,14902,15044,16575,15447,15966,15477,15798,15799,16576,15967,15968,16577,16339,16426,16461,16578,16742,17834,18558,18643,18648,18861,18904,18902}'::bigint[]);

SELECT i.id FROM event i JOIN repair_instances r USING (id) FOR NO KEY UPDATE OF i;

DELETE FROM payment WHERE event_id IN (SELECT id FROM repair_instances);

SELECT create_event_payment(i)
FROM event i JOIN repair_instances r USING (id)
WHERE i.type='lesson' AND NOT i.is_cancelled AND i.since<now();
UPDATE payment SET status='unpaid' WHERE event_id IN (SELECT id FROM repair_instances);
SELECT resolve_payment_with_credit(p)
FROM payment p WHERE event_id IN (SELECT id FROM repair_instances);
ROLLBACK;
