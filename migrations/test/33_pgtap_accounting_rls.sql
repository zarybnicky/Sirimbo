BEGIN;

CREATE SCHEMA IF NOT EXISTS tap;
CREATE EXTENSION IF NOT EXISTS pgtap SCHEMA tap;
SET LOCAL search_path = public, tap;
GRANT USAGE ON SCHEMA tap TO anonymous;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA tap TO anonymous;
SELECT tap.plan(23);

INSERT INTO tenant (id, name) VALUES (-9100, 'Accounting RLS'), (-9101, 'Other tenant');
SELECT set_config('jwt.claims.tenant_id', '-9100', true);
SELECT set_config('jwt.claims.my_person_ids', '{-9100}', true);
INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES (-9100, 'Owner', 'Test', 'unspecified', ''),
       (-9101, 'Other', 'Test', 'unspecified', '');
INSERT INTO accounting_period (id, tenant_id, since, until) OVERRIDING SYSTEM VALUE
VALUES (-9100, -9100, '2020-01-01', '2100-01-01'),
       (-9101, -9101, '2020-01-01', '2100-01-01');
INSERT INTO account (id, tenant_id, person_id, currency) OVERRIDING SYSTEM VALUE
VALUES (-9100, -9100, -9100, 'CZK'), (-9101, -9100, -9101, 'CZK'),
       (-9102, -9100, null, 'CZK'), (-9103, -9101, -9100, 'CZK');
INSERT INTO payment (id, tenant_id, accounting_period_id, status) OVERRIDING SYSTEM VALUE
VALUES (-9100, -9100, -9100, 'unpaid'), (-9101, -9100, -9100, 'unpaid'),
       (-9102, -9100, -9100, 'unpaid'), (-9103, -9101, -9101, 'unpaid');
INSERT INTO payment_debtor (id, tenant_id, payment_id, person_id) OVERRIDING SYSTEM VALUE
VALUES (-9100, -9100, -9100, -9100), (-9101, -9100, -9100, -9101),
       (-9102, -9100, -9101, -9101), (-9103, -9100, -9102, -9101),
       (-9104, -9101, -9103, -9100);
INSERT INTO payment_recipient (id, tenant_id, payment_id, account_id, amount) OVERRIDING SYSTEM VALUE
VALUES (-9100, -9100, -9100, -9101, 200), (-9101, -9100, -9100, -9102, 100),
       (-9102, -9100, -9101, -9100, 200), (-9103, -9100, -9102, -9101, 400),
       (-9104, -9101, -9103, -9103, 500);
INSERT INTO transaction (id, tenant_id, accounting_period_id, payment_id, source, effective_date)
OVERRIDING SYSTEM VALUE
VALUES (-9100, -9100, -9100, -9100, 'auto-credit', now()),
       (-9101, -9100, -9100, -9101, 'auto-credit', now()),
       (-9102, -9100, -9100, null, 'manual-credit', now()),
       (-9103, -9100, -9100, null, 'manual-credit', now()),
       (-9104, -9101, -9101, -9103, 'auto-credit', now());
INSERT INTO posting (id, tenant_id, transaction_id, account_id, amount) OVERRIDING SYSTEM VALUE
VALUES (-9100, -9100, -9100, -9100, -150), (-9101, -9100, -9100, -9101, 150),
       (-9102, -9100, -9102, -9101, 100), (-9103, -9100, -9103, -9100, 100),
       (-9104, -9101, -9104, -9103, 500);

SET LOCAL ROLE member;
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM account), ARRAY[-9100]::bigint[], 'only own accounts');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM payment), ARRAY[-9101,-9100]::bigint[], 'debtor and recipient payments');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM payment_debtor), ARRAY[-9100]::bigint[], 'only own debtor rows');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM payment_recipient), ARRAY[-9102]::bigint[], 'only own recipient rows');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM posting), ARRAY[-9103,-9100]::bigint[], 'only own postings');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM transaction), ARRAY[-9103,-9101,-9100]::bigint[], 'own postings or participating payment, including payment without own postings');
SELECT tap.is((SELECT (payment_debtor_price(d)).amount FROM payment_debtor d), 150::numeric, 'total includes hidden recipients and co-debtor');
SELECT tap.is((SELECT (payment_debtor_price(d)).currency FROM payment_debtor d), 'CZK', 'currency available despite hidden recipient accounts');
SELECT tap.is((SELECT amount FROM payment_debtor_price(ROW(-9101,-9100,-9100,-9101)::payment_debtor)), null::numeric, 'cannot query another debtor by composite argument');
SELECT tap.is((SELECT amount FROM payment_debtor_price(ROW(-9104,-9101,-9103,-9100)::payment_debtor)), null::numeric, 'cannot query another tenant by composite argument');
WITH changed AS (UPDATE payment SET status='paid' RETURNING id)
SELECT tap.is(count(*), 0::bigint, 'member cannot update payments') FROM changed;

RESET ROLE;
INSERT INTO account (id, tenant_id, person_id, currency) OVERRIDING SYSTEM VALUE
VALUES (-9104, -9100, -9100, 'EUR');
SET LOCAL ROLE member;
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM current_account_ids() a(id)), ARRAY[-9104,-9100]::bigint[], 'new currency account visible without refreshing claims');
SELECT set_config('jwt.claims.my_person_ids', '{}', true);
SELECT tap.is((SELECT count(*) FROM payment), 0::bigint, 'empty claims see no payments');
SELECT tap.is((SELECT count(*) FROM transaction), 0::bigint, 'empty claims see no transactions');
SELECT set_config('jwt.claims.my_person_ids', '{-9100,-9101}', true);
SELECT tap.is((SELECT count(*) FROM payment_debtor), 4::bigint, 'all proxied persons are included');
SELECT set_config('jwt.claims.my_person_ids', '{-9100}', true);
SELECT set_config('jwt.claims.tenant_id', '-9101', true);
SELECT tap.is((SELECT array_agg(id) FROM payment), ARRAY[-9103]::bigint[], 'tenant switch scopes the same person to the other tenant');
SELECT set_config('jwt.claims.tenant_id', '-9100', true);

RESET ROLE;
SET LOCAL ROLE trainer;
SELECT tap.is((SELECT count(*) FROM payment_debtor), 1::bigint, 'trainer inherits ownership restrictions');
RESET ROLE;
SET LOCAL ROLE anonymous;
SELECT tap.is((SELECT count(*) FROM payment), 0::bigint, 'anonymous sees no payments even with person claims');
SELECT tap.is((SELECT amount FROM payment_debtor_price(ROW(-9100,-9100,-9100,-9100)::payment_debtor)), null::numeric, 'anonymous cannot calculate totals');
RESET ROLE;
SET LOCAL ROLE administrator;
SELECT tap.is((SELECT count(*) FROM payment), 3::bigint, 'administrator sees all current-tenant payments');
SELECT tap.is((SELECT (payment_debtor_price(d)).amount FROM payment_debtor d WHERE id=-9101), 150::numeric, 'administrator sees other debtor totals');
WITH changed AS (UPDATE payment SET status='paid' RETURNING id)
SELECT tap.is(count(*), 3::bigint, 'administrator retains write access within tenant') FROM changed;

RESET ROLE;
SELECT tap.is((SELECT (payment_debtor_price(d)).amount FROM payment_debtor d WHERE id=-9100), 150::numeric, 'database owner can calculate totals without SET ROLE');
SELECT * FROM tap.finish(true);
ROLLBACK;
