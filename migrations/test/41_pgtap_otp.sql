BEGIN;
SET LOCAL search_path = public, tap;
SELECT tap.no_plan();

INSERT INTO tenant (id, name) VALUES (-9400, 'OTP source'), (-9401, 'OTP destination');
SELECT set_config('jwt.claims.tenant_id', '-9400', true);
INSERT INTO users (id, u_email, u_pass) VALUES (-9400, 'otp@test.invalid', 'test-password');
INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES (-9400, 'OTP', 'Person', 'unspecified', '');
INSERT INTO user_proxy (user_id, person_id) VALUES (-9400, -9400);
INSERT INTO tenant_administrator (tenant_id, person_id, since)
VALUES (-9400, -9400, now() - interval '1 day');
INSERT INTO event (id, tenant_id, since, until, is_public) OVERRIDING SYSTEM VALUE
VALUES (-9400, -9401, now(), now() + interval '1 day', true);

INSERT INTO otp_token (access_token, user_id, tenant_id, intent, event_id)
VALUES ('00000000-0000-4000-8000-000000009400', -9400, -9401, 'event_handoff', -9400);
INSERT INTO otp_token (access_token, user_id)
VALUES ('00000000-0000-4000-8000-000000009402', -9400);

SET LOCAL ROLE anonymous;
SELECT tap.throws_ok($$SELECT otp_login('00000000-0000-4000-8000-000000009400')$$, '28P01', 'INVALID_CREDENTIALS',
  'handoff cannot be redeemed in the source tenant');
SELECT set_config('jwt.claims.tenant_id', '-9401', true);
CREATE TEMP TABLE redeemed AS SELECT (otp_login('00000000-0000-4000-8000-000000009400')).jwt AS claims;
SELECT tap.ok((SELECT (claims).user_id = -9400 AND (claims).tenant_id = -9401
  AND (claims).my_person_ids = ARRAY[-9400]::bigint[]
  AND (claims).admin_tenant_ids = ARRAY[-9400]::bigint[] FROM redeemed),
  'handoff rebuilds identity for the destination without granting admin there');
SELECT tap.throws_ok($$SELECT otp_login('00000000-0000-4000-8000-000000009400')$$, '28P01', 'INVALID_CREDENTIALS',
  'handoff can only be redeemed once');
SELECT tap.is((SELECT ((otp_login('00000000-0000-4000-8000-000000009402')).jwt).user_id),
  -9400::bigint, 'password-reset OTP still works');
RESET ROLE;
UPDATE otp_token SET used_at=null, expires_at=now()-interval '1 second'
WHERE access_token='00000000-0000-4000-8000-000000009400';
SET LOCAL ROLE anonymous;
SELECT tap.throws_ok($$SELECT otp_login('00000000-0000-4000-8000-000000009400')$$, '28P01', 'INVALID_CREDENTIALS',
  'expired handoffs cannot be redeemed');
RESET ROLE;
UPDATE otp_token SET expires_at=now()+interval '10 minutes', user_id=null
WHERE access_token='00000000-0000-4000-8000-000000009400';
SET LOCAL ROLE anonymous;
SELECT tap.throws_ok($$SELECT otp_login('00000000-0000-4000-8000-000000009400')$$, '28P01', 'INVALID_CREDENTIALS',
  'an OTP without a user cannot be redeemed');
SELECT * FROM tap.finish(true);
ROLLBACK;
