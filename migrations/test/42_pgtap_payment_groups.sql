BEGIN;
SET LOCAL search_path = public, tap;
SELECT tap.plan(30);

INSERT INTO tenant (id, name) VALUES (2, 'Payment group tests') ON CONFLICT (id) DO NOTHING;
SELECT set_config('jwt.claims.tenant_id', '2', true);
INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES (-9800, 'Debtor', 'Payment group', 'unspecified', ''),
       (-9801, 'Recipient', 'Payment group', 'unspecified', '');
INSERT INTO account (id, person_id, currency) OVERRIDING SYSTEM VALUE
VALUES (-9800, -9800, 'CZK'), (-9801, -9801, 'CZK');
INSERT INTO cohort (id, name, color_rgb) OVERRIDING SYSTEM VALUE
VALUES (-9800, 'Payment group tests', '#000000');
INSERT INTO cohort_subscription (id, cohort_id, account_id, price) OVERRIDING SYSTEM VALUE
VALUES (-9800, -9800, -9801, (100, 'CZK')::price);

SET LOCAL ROLE administrator;
CREATE TEMP TABLE saved_payment AS
SELECT * FROM save_payment_group(-9800, ARRAY[(-9800, now() - interval '1 day', 100)::payment_group_item]);
CREATE TEMP VIEW credit_postings AS
SELECT p.* FROM posting p JOIN transaction t ON t.id = p.transaction_id
WHERE t.payment_id = (SELECT id FROM saved_payment) AND t.source = 'auto-credit';

SELECT tap.is((SELECT status::text FROM saved_payment), 'paid', 'new due credit payment resolves immediately');
SELECT tap.ok((SELECT paid_at IS NOT NULL FROM saved_payment), 'credit resolution sets paid_at');
SELECT tap.is((SELECT count(*) FROM transaction WHERE payment_id = (SELECT id FROM saved_payment)), 1::bigint, 'new credit payment has one transaction');
SELECT tap.is((SELECT amount FROM credit_postings WHERE account_id = -9800), -100::numeric, 'new payment debits the person');
SELECT tap.is((SELECT amount FROM credit_postings WHERE account_id = -9801), 100::numeric, 'new payment credits the recipient');

CREATE TEMP TABLE previous_transaction AS
SELECT id FROM transaction WHERE payment_id = (SELECT id FROM saved_payment) AND source = 'auto-credit';
CREATE TEMP TABLE manual_transaction AS
WITH created AS (
  INSERT INTO transaction (payment_id, source)
  SELECT id, 'manual-cash' FROM saved_payment RETURNING id
) SELECT * FROM created;
INSERT INTO posting (transaction_id, account_id, amount)
SELECT id, -9801, 5 FROM manual_transaction;

SELECT tap.is((SELECT id FROM save_payment_group(-9800, ARRAY[(-9800, (SELECT due_at FROM saved_payment), 140)::payment_group_item])), (SELECT id FROM saved_payment), 'editing retains the payment ID');
SELECT tap.is((SELECT count(*) FROM transaction WHERE id = (SELECT id FROM previous_transaction)), 0::bigint, 'old auto-credit transaction is removed');
SELECT tap.is((SELECT count(*) FROM posting WHERE transaction_id = (SELECT id FROM previous_transaction)), 0::bigint, 'old credit postings are removed');
SELECT tap.is((SELECT count(*) FROM transaction WHERE payment_id = (SELECT id FROM saved_payment) AND source = 'auto-credit'), 1::bigint, 'replacement creates one auto-credit transaction');
SELECT tap.is((SELECT amount FROM credit_postings WHERE account_id = -9800), -140::numeric, 'replacement uses the new debit amount');
SELECT tap.is((SELECT amount FROM credit_postings WHERE account_id = -9801), 140::numeric, 'replacement uses the new recipient amount');
SELECT tap.is((SELECT count(*) FROM transaction WHERE id = (SELECT id FROM manual_transaction)), 1::bigint, 'credit edit preserves manual transaction');
SELECT tap.is((SELECT amount FROM posting WHERE transaction_id = (SELECT id FROM manual_transaction)), 5::numeric, 'credit edit preserves manual postings');

TRUNCATE previous_transaction;
INSERT INTO previous_transaction
SELECT id FROM transaction WHERE payment_id = (SELECT id FROM saved_payment) AND source = 'auto-credit';
SELECT tap.is((SELECT id FROM save_payment_group(-9800, ARRAY[(-9800, (SELECT due_at FROM saved_payment), 140)::payment_group_item])), (SELECT id FROM saved_payment), 'unchanged amount still returns the payment');
SELECT tap.is((SELECT id FROM transaction WHERE payment_id = (SELECT id FROM saved_payment) AND source = 'auto-credit'), (SELECT id FROM previous_transaction), 'unchanged amount keeps its credit transaction');
SELECT tap.is((SELECT status::text FROM save_payment_group(-9800, ARRAY[(-9800, (SELECT due_at FROM saved_payment), 0)::payment_group_item])), 'paid', 'explicit zero resolves as a paid credit payment');
SELECT tap.is((SELECT amount FROM credit_postings WHERE account_id = -9800), 0::numeric, 'zero removes the previous debit');
SELECT tap.is((SELECT amount FROM credit_postings WHERE account_id = -9801), 0::numeric, 'zero removes the previous recipient amount');

CREATE TEMP TABLE future_payment AS
SELECT * FROM save_payment_group(-9800, ARRAY[(-9800, now() + interval '1 month', 200)::payment_group_item]);
SELECT tap.is((SELECT status::text FROM future_payment), 'tentative', 'future credit payment remains tentative');
SELECT tap.ok((SELECT paid_at IS NULL FROM future_payment), 'future credit payment has no paid_at');
SELECT tap.is((SELECT count(*) FROM transaction WHERE payment_id = (SELECT id FROM future_payment)), 0::bigint, 'future payment does not use credit yet');
SELECT tap.is((SELECT id FROM save_payment_group(-9800, ARRAY[(-9800, (SELECT due_at FROM future_payment), 250)::payment_group_item])), (SELECT id FROM future_payment), 'future edit retains the payment ID');
SELECT tap.is((SELECT count(*) FROM transaction WHERE payment_id = (SELECT id FROM future_payment)), 0::bigint, 'future edit still does not use credit');
SELECT tap.is((SELECT amount FROM payment_recipient WHERE payment_id = (SELECT id FROM future_payment)), 250::numeric, 'future edit updates the amount');

UPDATE payment SET is_auto_credit_allowed = false, paid_at = now() - interval '7 days'
WHERE id = (SELECT id FROM saved_payment);
CREATE TEMP TABLE non_credit_payment AS
SELECT p.*, t.id AS transaction_id FROM payment p JOIN transaction t ON t.payment_id = p.id
WHERE p.id = (SELECT id FROM saved_payment) AND t.source = 'auto-credit';
SELECT tap.is((SELECT id FROM save_payment_group(-9800, ARRAY[(-9800, (SELECT due_at FROM saved_payment), 320)::payment_group_item])), (SELECT id FROM saved_payment), 'non-credit edit retains the payment ID');
SELECT tap.is((SELECT status FROM payment WHERE id = (SELECT id FROM saved_payment)), (SELECT status FROM non_credit_payment), 'non-credit edit preserves status');
SELECT tap.is((SELECT paid_at FROM payment WHERE id = (SELECT id FROM saved_payment)), (SELECT paid_at FROM non_credit_payment), 'non-credit edit preserves paid_at');
SELECT tap.is((SELECT id FROM transaction WHERE payment_id = (SELECT id FROM saved_payment) AND source = 'auto-credit'), (SELECT transaction_id FROM non_credit_payment), 'payment flag disables rebuilding even in a credit-using tenant');
SELECT tap.is((SELECT count(*) FROM transaction WHERE id = (SELECT id FROM manual_transaction)), 1::bigint, 'non-credit edit preserves manual transaction');
SELECT tap.is((SELECT amount FROM payment_recipient WHERE payment_id = (SELECT id FROM saved_payment)), 320::numeric, 'non-credit edit updates only the amount');

RESET ROLE;
SELECT * FROM tap.finish(true);
ROLLBACK;
