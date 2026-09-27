BEGIN;
CREATE SCHEMA IF NOT EXISTS tap;
CREATE EXTENSION IF NOT EXISTS pgtap SCHEMA tap;
SET LOCAL search_path = public, tap;
GRANT USAGE ON SCHEMA tap TO anonymous;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA tap TO anonymous;
SELECT tap.plan(10);

INSERT INTO tenant (id, name) VALUES (-8500, 'Application write tests');
SELECT set_config('jwt.claims.tenant_id', '-8500', true);
INSERT INTO users (id, tenant_id, u_email, u_pass, u_jmeno, u_prijmeni) OVERRIDING SYSTEM VALUE
VALUES (-8500,-8500,'application@test.invalid','test-password','Application','User');
SELECT set_config('jwt.claims.user_id', '-8500', true);
SET LOCAL ROLE anonymous;
SELECT tap.lives_ok($$INSERT INTO membership_application
  (id, first_name, last_name, gender, nationality, created_by, prefix_title, suffix_title, bio) OVERRIDING SYSTEM VALUE
  VALUES (-8500, 'Test', 'Applicant', 'unspecified', '', -8500, '', '', '')$$,
  'logged-in nonmember can submit an application');
SELECT tap.lives_ok($$UPDATE membership_application SET note='Edited' WHERE id=-8500$$,
  'applicant can edit a pending application');
SELECT tap.throws_ok($$UPDATE membership_application SET status='approved' WHERE id=-8500$$,
  '42501', null, 'applicant cannot approve their application');
SELECT tap.throws_ok($$INSERT INTO membership_application
  (first_name, last_name, gender, nationality, created_by, status)
  VALUES ('Test', 'Applicant', 'unspecified', '', -8500, 'approved')$$,
  '42501', null, 'applicant cannot submit an already approved application');
RESET ROLE;
SET LOCAL ROLE administrator;
SELECT tap.lives_ok($$SELECT confirm_membership_application(-8500)$$,
  'administrator can confirm the application including person and proxy creation');
SELECT tap.is((SELECT status::text FROM membership_application WHERE id=-8500), 'approved',
  'confirmation approves the application');
RESET ROLE;
SET LOCAL ROLE anonymous;
SELECT tap.is((SELECT count(*) FROM membership_application WHERE id=-8500), 1::bigint,
  'applicant can still read the processed application');
WITH changed AS (UPDATE membership_application SET note='Changed after approval' WHERE id=-8500 RETURNING id)
SELECT tap.is(count(*), 0::bigint, 'applicant cannot edit a processed application') FROM changed;
WITH removed AS (DELETE FROM membership_application WHERE id=-8500 RETURNING id)
SELECT tap.is(count(*), 0::bigint, 'applicant cannot delete a processed application') FROM removed;
SELECT set_config('jwt.claims.user_id', '', true);
SELECT tap.is((SELECT count(*) FROM membership_application WHERE id=-8500), 0::bigint,
  'unauthenticated caller cannot see the application');
RESET ROLE;
SELECT * FROM tap.finish(true);
ROLLBACK;
