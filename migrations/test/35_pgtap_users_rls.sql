BEGIN;
CREATE SCHEMA IF NOT EXISTS tap;
CREATE EXTENSION IF NOT EXISTS pgtap SCHEMA tap;
SET LOCAL search_path = public, tap;
GRANT USAGE ON SCHEMA tap TO anonymous;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA tap TO anonymous;
SELECT tap.plan(17);

INSERT INTO tenant (id, name) VALUES (-8300, 'User RLS'), (-8301, 'Other user tenant');
SELECT set_config('jwt.claims.tenant_id', '-8300', true);
INSERT INTO users (id, tenant_id, u_email, u_pass, u_jmeno, u_prijmeni) OVERRIDING SYSTEM VALUE
VALUES (-8300,-8300,'self@test.invalid','test-password','Self','User'),
       (-8301,-8301,'shared@test.invalid','test-password','Shared','User'),
       (-8302,-8300,'unrelated@test.invalid','test-password','Announcement','Author'),
       (-8303,-8300,'expired@test.invalid','test-password','Expired','User');
INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES (-8300,'Shared','Person','unspecified',''),
       (-8301,'Other','Person','unspecified',''),
       (-8302,'Expired','Person','unspecified','');
INSERT INTO user_proxy (id,user_id,person_id,since,until) OVERRIDING SYSTEM VALUE
VALUES (-8300,-8300,-8300,now()-interval '1 day',null),
       (-8301,-8301,-8300,now()-interval '1 day',null),
       (-8302,-8301,-8301,now()-interval '1 day',null),
       (-8303,-8302,-8301,now()-interval '1 day',null),
       (-8304,-8303,-8300,now()-interval '2 days',now()-interval '1 day'),
       (-8305,-8300,-8302,now()-interval '2 days',now()-interval '1 day'),
       (-8306,-8303,-8302,now()-interval '1 day',null);
INSERT INTO announcement (id,tenant_id,author_id,title,body,status) OVERRIDING SYSTEM VALUE
VALUES (-8300,-8300,-8302,'Visible announcement','','published'),
       (-8301,-8300,-8302,'Hidden draft','','draft'),
       (-8302,-8301,-8302,'Other tenant','','published');

SELECT set_config('jwt.claims.user_id', '-8300', true);
SELECT set_config('jwt.claims.my_person_ids', '{-8300,-8302}', true);
SET LOCAL ROLE member;
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM users), ARRAY[-8301,-8300]::bigint[], 'self and active shared-person users, including accounts created in another tenant');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM user_proxy), ARRAY[-8305,-8301,-8300]::bigint[], 'own history and shared active links, not other people managed by the shared user');
SELECT tap.is((SELECT count(*) FROM users WHERE id=-8303), 0::bigint, 'expired links on either side grant no shared access despite stale person claims');
WITH changed AS (UPDATE users SET u_jmeno='Forbidden' WHERE id=-8301 RETURNING id)
SELECT tap.is(count(*), 0::bigint, 'shared-user access is read-only') FROM changed;
SELECT tap.is((get_current_user()).id, -8300::bigint, 'current-user lookup still works');
SELECT tap.is(current_claims()->>'user_id', '-8300', 'current claims still work');
SELECT tap.is((refresh_jwt()).user_id, -8300::bigint, 'JWT refresh still works');
SELECT tap.is((SELECT announcement_author_name(a) FROM announcement a WHERE id=-8300), 'Announcement Author', 'visible announcement retains its byline');
SELECT tap.is((SELECT count(*) FROM users WHERE id=-8302), 0::bigint, 'byline does not expose author account');

RESET ROLE;
SET LOCAL ROLE anonymous;
SELECT tap.is((SELECT count(*) FROM users), 2::bigint, 'logged-in nonmember retains self and shared-user access');
SELECT tap.lives_ok($$SELECT change_password('new-test-password')$$, 'nonmember can change own password');
SELECT set_config('jwt.claims.user_id', '', true);
SELECT tap.is((SELECT count(*) FROM users), 0::bigint, 'unauthenticated caller sees no user accounts');
SELECT tap.is((SELECT count(*) FROM user_proxy), 0::bigint, 'unauthenticated caller sees no proxy links');
SELECT tap.is(((login('self@test.invalid','new-test-password')).usr).id, -8300::bigint, 'password login still works after password change');
SELECT tap.is((SELECT count(*) FROM users), 2::bigint, 'login sets context for nested user reads');

RESET ROLE;
SET LOCAL ROLE administrator;
SELECT tap.is((SELECT count(*) FROM users WHERE id BETWEEN -8303 AND -8300), 4::bigint, 'administrator retains account lookup across tenants');
SELECT tap.is((SELECT count(*) FROM user_proxy WHERE id BETWEEN -8306 AND -8300), 7::bigint, 'administrator retains proxy management visibility');
RESET ROLE;
SELECT * FROM tap.finish(true);
ROLLBACK;
