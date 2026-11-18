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
SELECT tap.ok(otp_login('00000000-0000-4000-8000-000000009400') is null,
  'handoff cannot be redeemed in the source tenant');
SELECT set_config('jwt.claims.tenant_id', '-9401', true);
CREATE TEMP TABLE redeemed AS SELECT (otp_login('00000000-0000-4000-8000-000000009400')).jwt AS claims;
SELECT tap.ok((SELECT (claims).user_id = -9400 AND (claims).tenant_id = -9401
  AND (claims).my_person_ids = ARRAY[-9400]::bigint[]
  AND (claims).admin_tenant_ids = ARRAY[-9400]::bigint[] FROM redeemed),
  'handoff rebuilds identity for the destination without granting admin there');
SELECT tap.ok(otp_login('00000000-0000-4000-8000-000000009400') is null,
  'handoff can only be redeemed once');
SELECT tap.is((SELECT ((otp_login('00000000-0000-4000-8000-000000009402')).jwt).user_id),
  -9400::bigint, 'password-reset OTP still works');
RESET ROLE;
UPDATE otp_token SET used_at=null, expires_at=now()-interval '1 second'
WHERE access_token='00000000-0000-4000-8000-000000009400';
SET LOCAL ROLE anonymous;
SELECT tap.ok(otp_login('00000000-0000-4000-8000-000000009400') is null,
  'expired handoffs cannot be redeemed');
RESET ROLE;
UPDATE otp_token SET expires_at=now()+interval '10 minutes', user_id=null
WHERE access_token='00000000-0000-4000-8000-000000009400';
SET LOCAL ROLE anonymous;
SELECT tap.ok(otp_login('00000000-0000-4000-8000-000000009400') is null,
  'an OTP without a user cannot be redeemed');
RESET ROLE;
INSERT INTO otp_token (access_token, user_id, tenant_id, intent)
VALUES ('00000000-0000-4000-8000-000000009403', -9400, -9401, 'tenant_handoff');
SELECT set_config('jwt.claims.tenant_id', '-9400', true);
SET LOCAL ROLE anonymous;
SELECT tap.ok(otp_login('00000000-0000-4000-8000-000000009403') is null,
  'club handoff cannot be redeemed in another tenant');
SELECT set_config('jwt.claims.tenant_id', '-9401', true);
CREATE TEMP TABLE club_redeemed AS SELECT (otp_login('00000000-0000-4000-8000-000000009403')).jwt AS claims;
SELECT tap.ok((SELECT (claims).user_id = -9400 AND (claims).tenant_id = -9401
  AND (claims).admin_tenant_ids = ARRAY[-9400]::bigint[] FROM club_redeemed),
  'club handoff rebuilds destination claims without granting destination admin');
SELECT tap.ok(otp_login('00000000-0000-4000-8000-000000009403') is null,
  'club handoff can only be redeemed once');
RESET ROLE;
UPDATE otp_token SET used_at=null, expires_at=now()-interval '1 second'
WHERE access_token='00000000-0000-4000-8000-000000009403';
SET LOCAL ROLE anonymous;
SELECT tap.ok(otp_login('00000000-0000-4000-8000-000000009403') is null,
  'expired club handoffs cannot be redeemed');
SELECT * FROM tap.finish(true);
ROLLBACK;
