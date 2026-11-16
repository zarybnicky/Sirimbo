BEGIN;
SET LOCAL search_path = public, tap;
SELECT tap.plan(16);

INSERT INTO tenant (id, name) VALUES (-8500, 'Application write tests'), (-8501, 'Other application club');
SELECT set_config('jwt.claims.tenant_id', '-8500', true);
INSERT INTO users (id, tenant_id, u_email, u_pass, u_jmeno, u_prijmeni) OVERRIDING SYSTEM VALUE
VALUES (-8500,-8500,'application@test.invalid','test-password','Application','User');
INSERT INTO cohort (id, tenant_id, name, color_rgb) OVERRIDING SYSTEM VALUE
VALUES (-8501, -8501, 'Other club cohort', '#000000');
INSERT INTO membership_application (id, tenant_id, first_name, last_name, gender, nationality, created_by)
OVERRIDING SYSTEM VALUE VALUES (-8501, -8501, 'Other', 'Applicant', 'unspecified', '', -8500);
SELECT set_config('jwt.claims.user_id', '-8500', true);
SET LOCAL ROLE anonymous;
SELECT tap.lives_ok($$INSERT INTO membership_application
  (id, first_name, last_name, gender, nationality, created_by) OVERRIDING SYSTEM VALUE
  VALUES (-8500, 'Test', 'Applicant', 'unspecified', '', -8500)$$,
  'logged-in nonmember can submit an application');
WITH changed AS (UPDATE membership_application SET note='Edited' WHERE id=-8500 RETURNING note)
SELECT tap.is((SELECT note FROM changed), 'Edited', 'applicant can edit a pending application');
SELECT tap.throws_ok($$UPDATE membership_application SET status='approved' WHERE id=-8500$$,
  '42501', null, 'applicant cannot approve their application');
SELECT tap.throws_ok($$INSERT INTO membership_application
  (first_name, last_name, gender, nationality, created_by, status)
  VALUES ('Test', 'Applicant', 'unspecified', '', -8500, 'approved')$$,
  '42501', null, 'applicant cannot submit an already approved application');
RESET ROLE;
SELECT set_config('jwt.claims.admin_tenant_ids', '{-8500}', true);
SET LOCAL ROLE administrator;
SELECT tap.throws_ok($$SELECT confirm_membership_application(-8500, cohort_ids => array[-8501]::bigint[])$$,
  '42501', null, 'cannot approve into another club cohort');
SELECT tap.ok(confirm_membership_application(-8501) is null, 'cannot approve another club application');
SELECT tap.throws_ok($$SELECT confirm_membership_application(-8500, false, false, false)$$,
  '42501', 'A new person must have a club relationship', 'cannot approve without a club role');
SELECT tap.throws_ok($$SELECT confirm_membership_application(-8500, null, false, false)$$,
  '42501', 'A new person must have a club relationship', 'null does not bypass the club role requirement');
SELECT tap.is((SELECT status::text FROM membership_application WHERE id=-8500), 'sent',
  'rejected confirmation leaves the application pending');
SELECT tap.lives_ok($$SELECT confirm_membership_application(-8500)$$,
  'administrator can confirm the application including person and proxy creation');
SELECT tap.is((SELECT status::text FROM membership_application WHERE id=-8500), 'approved',
  'confirmation approves the application');
SELECT tap.ok((SELECT prefix_title = '' AND suffix_title = '' AND bio = '' FROM person WHERE first_name = 'Test' AND last_name = 'Applicant'),
  'application column defaults carry into the person');
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
