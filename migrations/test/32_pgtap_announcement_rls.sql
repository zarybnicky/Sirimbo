BEGIN;

CREATE SCHEMA IF NOT EXISTS tap;
CREATE EXTENSION IF NOT EXISTS pgtap SCHEMA tap;
DO $$
BEGIN
  execute 'set search_path to ' || current_setting('search_path') || ',tap';
END
$$;

SELECT tap.plan(8);

INSERT INTO tenant (id, name)
VALUES (1200, 'Announcement RLS Test')
ON CONFLICT (id) DO NOTHING;
SELECT set_config('jwt.claims.tenant_id', '1200', true);

INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES
  (220001, 'Member', 'Test', 'unspecified', ''),
  (220002, 'Trainer', 'Test', 'unspecified', '')
ON CONFLICT (id) DO NOTHING;

INSERT INTO users (id, tenant_id, u_pass, u_email, u_jmeno, u_prijmeni) OVERRIDING SYSTEM VALUE
VALUES
  (230001, 1200, repeat('0', 40), 'announcement-trainer@test.invalid', 'Trainer', 'Test'),
  (230002, 1200, repeat('0', 40), 'announcement-other@test.invalid', 'Other', 'Test')
ON CONFLICT (id) DO NOTHING;

INSERT INTO tenant_membership (tenant_id, person_id, since)
VALUES (1200, 220001, now() - interval '1 day')
ON CONFLICT DO NOTHING;
INSERT INTO tenant_trainer (tenant_id, person_id, since)
VALUES (1200, 220002, now() - interval '1 day')
ON CONFLICT DO NOTHING;

INSERT INTO announcement (
  id, tenant_id, author_id, title, body, status, scheduled_since
) OVERRIDING SYSTEM VALUE
VALUES
  (920001, 1200, 230002, 'Everyone', '', 'published', null),
  (920002, 1200, 230002, 'Scheduled', '', 'published', now() + interval '1 day'),
  (920003, 1200, 230002, 'Members', '', 'published', null),
  (920004, 1200, 230002, 'Trainers', '', 'published', null),
  (920005, 1200, 230001, 'Own draft', '', 'draft', null),
  (920006, 1200, 230002, 'Archive', '', 'archived', null),
  (920008, 1200, 230002, 'Other draft', '', 'draft', null)
ON CONFLICT (id) DO NOTHING;

INSERT INTO announcement_audience (tenant_id, announcement_id, audience_role)
VALUES
  (1200, 920003, 'member'),
  (1200, 920004, 'trainer')
ON CONFLICT DO NOTHING;

SELECT tap.is(
  app_private.announcement_status_next(
    now(), now() + interval '1 day', null, 'published'
  ),
  'scheduled'::announcement_status,
  'a future publication becomes scheduled'
);
SELECT tap.is(
  app_private.announcement_status_next(
    now(), null, now() - interval '1 day', 'published'
  ),
  'archived'::announcement_status,
  'an ended publication becomes archived'
);

GRANT USAGE ON SCHEMA tap TO member;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA tap TO member;
SELECT set_config('jwt.claims.my_person_ids', '[220001]', true);
SET LOCAL ROLE member;

SELECT tap.is(
  (SELECT array_agg(id ORDER BY id) FROM announcement WHERE id BETWEEN 920001 AND 920008),
  ARRAY[920001, 920003, 920006]::bigint[],
  'members see current and archived announcements for their audience'
);

RESET ROLE;
SELECT set_config('jwt.claims.user_id', '230001', true);
SELECT set_config('jwt.claims.my_person_ids', '[220002]', true);
SET LOCAL ROLE trainer;

SELECT tap.is(
  (SELECT array_agg(id ORDER BY id) FROM announcement WHERE id BETWEEN 920001 AND 920008),
  ARRAY[920001, 920004, 920005, 920006]::bigint[],
  'trainers additionally see their own unpublished announcements'
);
SELECT tap.lives_ok(
  $$update announcement set title = 'Updated own draft' where id = 920005$$,
  'trainers can update their own announcements'
);
SELECT tap.lives_ok(
  $$
    do $body$
    declare
      changed integer;
    begin
      update announcement set title = 'Blocked update' where id = 920008;
      get diagnostics changed = row_count;
      if changed <> 0 then
        raise exception 'updated % rows', changed;
      end if;
    end
    $body$
  $$,
  'trainers cannot update announcements by another author'
);
SELECT tap.lives_ok(
  $$
    do $body$
    declare
      announcement_id bigint;
    begin
      insert into announcement (title, body, status)
      values ('Trainer announcement', '', 'draft')
      returning id into announcement_id;

      insert into announcement_audience (announcement_id, audience_role)
      values (announcement_id, 'member');
    end
    $body$
  $$,
  'trainers can create and target their own announcements'
);
SELECT tap.is(
  (SELECT author_id FROM announcement WHERE title = 'Trainer announcement'),
  230001::bigint,
  'new trainer announcements receive the current user as author'
);

RESET ROLE;
SELECT tap.finish();

ROLLBACK;
