begin;
set local search_path = public, tap;
select tap.plan(16);

insert into tenant (id, name) values (-3900, 'Activity'), (-3901, 'Other activity');
select set_config('jwt.claims.tenant_id', '-3900', true);
insert into person (id, first_name, last_name, gender, nationality, csts_id)
  overriding system value values
  (-3900, 'Active', 'Person', 'unspecified', '', 1999999999),
  (-3901, 'Empty', 'Person', 'unspecified', '', null),
  (-3902, 'Other', 'Person', 'unspecified', '', null);
insert into tenant_membership (tenant_id, person_id, since) values
  (-3900, -3900, now() - interval '1 year'),
  (-3900, -3901, now() - interval '1 year'),
  (-3901, -3902, now() - interval '1 year');
insert into users (id, tenant_id, u_email, u_pass, u_jmeno, u_prijmeni, last_active_at)
  overriding system value values
  (-3900, -3901, 'activity@test.invalid', 'test-password', 'Active', 'User', now() - interval '3 days'),
  (-3901, -3900, 'expired-activity@test.invalid', 'test-password', 'Expired', 'User', now());
insert into user_proxy (user_id, person_id, since, until) values
  (-3900, -3900, now() - interval '1 year', null),
  (-3901, -3900, now() - interval '1 year', now() - interval '1 day');
insert into access_event (tenant_id, external_id, device, kind, code, person_id, occurred_at, allowed) values
  (-3900, 'allowed', 'test', 'MIFARE', 'activity', -3900, now() - interval '2 days', true),
  (-3900, 'denied', 'test', 'MIFARE', 'activity', -3900, now() - interval '1 day', false),
  (-3900, 'future', 'test', 'MIFARE', 'activity', -3900, now() + interval '1 day', true),
  (-3901, 'other', 'test', 'MIFARE', 'activity', -3900, now(), true);
insert into event_instance (id, tenant_id, since, until, type, is_cancelled)
  overriding system value values
  (-3900, -3900, now() - interval '5 days', now() - interval '5 days' + interval '1 hour', 'group', false),
  (-3901, -3900, now() - interval '1 day', now() - interval '1 day' + interval '1 hour', 'lesson', true),
  (-3902, -3900, now() + interval '1 day', now() + interval '1 day 1 hour', 'lesson', false),
  (-3903, -3900, now() - interval '2 days', now() - interval '2 days' + interval '1 hour', 'lesson', false),
  (-3904, -3901, now() - interval '1 day', now() - interval '1 day' + interval '1 hour', 'group', false),
  (-3905, -3900, now() - interval '12 hours', now() - interval '11 hours', 'group', false),
  (-3906, -3900, now() - interval '6 hours', now() - interval '5 hours', 'lesson', false);
insert into event_registration (tenant_id, event_id, person_id, status) values
  (-3900, -3900, -3900, 'attended'), (-3900, -3901, -3900, 'attended'),
  (-3900, -3902, -3900, 'attended'), (-3900, -3903, -3900, 'unknown'),
  (-3901, -3904, -3900, 'attended'),
  (-3900, -3905, -3900, 'unknown');
insert into event_registration (tenant_id, event_id, person_id, status, registration_status)
  values (-3900, -3906, -3900, 'unknown', 'cancelled');
insert into federated.person (federation, external_id) values ('csts', 1999999999);
insert into federated.competitor (federation, external_id, competitor_type) values ('csts', 'activity-test', 'solo');
insert into federated.competitor_component (competitor_id, person_id, role) values ('csts:activity-test', 'csts:1999999999', 'member');
insert into federated.category (id, name, series, discipline, age_group, class)
  overriding system value values (-3900, 'Activity', 'activity-test', 'STT', '', '');
insert into federated.event (id, federation, external_id, start_date)
  overriding system value values (-3900, 'csts', 'activity-test', current_date - 20);
insert into federated.competition (id, federation, external_id, event_id, category_id, start_date)
  overriding system value values
  (-3900, 'csts', 'activity-result', -3900, -3900, current_date - 20),
  (-3901, 'csts', 'activity-future', -3900, -3900, current_date + 1),
  (-3902, 'csts', 'activity-entry', -3900, -3900, current_date - 1);
insert into federated.competition_result (competition_id, competitor_id, ranking)
  values (-3900, 'csts:activity-test', 1), (-3901, 'csts:activity-test', 1);
insert into federated.competition_entry (competition_id, competitor_id)
  values (-3902, 'csts:activity-test');

set local role trainer;
select tap.is((select count(*) from person_last_activity where person_id between -3902 and -3900), 2::bigint, 'trainer sees current-tenant people only');
select tap.is((select web_at from person_last_activity where person_id = -3900), now() - interval '3 days', 'web activity uses active links, including accounts created elsewhere');
select tap.is((select card_entry_at from person_last_activity where person_id = -3900), now() - interval '2 days', 'card activity excludes denied, future, and other-tenant entries');
select tap.is((select training_at from person_last_activity where person_id = -3900), now() - interval '2 days', 'lessons are implicitly attended; unknown groups, cancellations, future and other-tenant events are excluded');
select tap.is((select competition_on from person_last_activity where person_id = -3900), current_date - 20, 'competition uses past results, not entries');
select tap.ok((select web_at is null and card_entry_at is null and training_at is null and competition_on is null from person_last_activity where person_id = -3901), 'missing sources yield null');

reset role;
update users set last_active_at = null where id = -3901;
insert into user_proxy (user_id, person_id) values (-3901, -3901);
insert into access_credential (person_id, label, code) values (-3901, 'Undated', 'undated-activity-card');
insert into event_registration (event_id, person_id, status) values (-3905, -3901, 'unknown');
insert into federated.person (federation, external_id) values ('csts', 1999999998);
update person set csts_id = 1999999998 where id = -3901;
set local role trainer;
select tap.ok((select web_at = 'epoch'::timestamptz and card_entry_at = 'epoch'::timestamptz and training_at = 'epoch'::timestamptz and competition_on = 'epoch'::date from person_last_activity where person_id = -3901), 'sources without dates yield epoch');
select tap.is((select count(*) from users where id = -3900), 0::bigint, 'summary does not grant account access');
select tap.is((select count(*) from user_proxy where person_id = -3900), 0::bigint, 'summary does not grant account-link access');
select tap.is((select count(*) from access_event where person_id = -3900), 0::bigint, 'summary does not grant access to raw entries');
select tap.ok(not has_table_privilege(current_user, 'person_last_activity', 'insert,update,delete'), 'cannot write activity');
select set_config('jwt.claims.tenant_id', '-3901', true);
select tap.is((select count(*) from person_last_activity where person_id = -3900), 0::bigint, 'changing tenant removes access to previous-tenant people');
reset role;
select set_config('jwt.claims.tenant_id', '-3900', true);
set local role administrator;
select tap.is((select count(*) from person_last_activity where person_id = -3900), 1::bigint, 'administrator can read activity');
set local role member;
select tap.is((select count(*) from person_last_activity), 0::bigint, 'members without current persons cannot read activity');
select set_config('jwt.claims.my_person_ids', '{-3901,-3902}', true);
select tap.is((select array_agg(person_id order by person_id) from person_last_activity where person_id between -3902 and -3900), array[-3901]::bigint[], 'member sees self, but not another person or a current person in another tenant');
select set_config('jwt.claims.my_person_ids', '{}', true);
set local role anonymous;
select tap.is((select count(*) from person_last_activity), 0::bigint, 'anonymous cannot read activity');
reset role;
select * from tap.finish(true);
rollback;
