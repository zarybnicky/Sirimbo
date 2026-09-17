BEGIN;

CREATE SCHEMA IF NOT EXISTS tap;
CREATE EXTENSION IF NOT EXISTS pgtap SCHEMA tap;
DO $$
BEGIN
  execute 'set search_path to ' || current_setting('search_path') || ',tap';
END
$$;

SELECT tap.plan(7);

INSERT INTO tenant (id, name)
VALUES (1300, 'Document RLS Test')
ON CONFLICT (id) DO NOTHING;
SELECT set_config('jwt.claims.tenant_id', '1300', true);

INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES
  (240001, 'Member', 'Doc', 'unspecified', ''),
  (240002, 'Trainer', 'Doc', 'unspecified', '')
ON CONFLICT (id) DO NOTHING;

INSERT INTO users (id, tenant_id, u_pass, u_email, u_jmeno, u_prijmeni) OVERRIDING SYSTEM VALUE
VALUES
  (250001, 1300, repeat('0', 40), 'document-trainer@test.invalid', 'Trainer', 'Doc'),
  (250002, 1300, repeat('0', 40), 'document-member@test.invalid', 'Member', 'Doc')
ON CONFLICT (id) DO NOTHING;

INSERT INTO tenant_membership (tenant_id, person_id, since)
VALUES (1300, 240001, now() - interval '1 day')
ON CONFLICT DO NOTHING;
INSERT INTO tenant_trainer (tenant_id, person_id, since)
VALUES (1300, 240002, now() - interval '1 day')
ON CONFLICT DO NOTHING;

INSERT INTO event_instance (id, tenant_id, since, until, type) OVERRIDING SYSTEM VALUE
VALUES (940001, 1300, now() + interval '1 day', now() + interval '1 day 2 hours', 'group')
ON CONFLICT (id) DO NOTHING;

INSERT INTO document (id, tenant_id, kind, title, event_instance_id, show_to_members)
  OVERRIDING SYSTEM VALUE
VALUES
  (930001, 1300, 'plan', 'Trainer only', 940001, false),
  (930002, 1300, 'plan', 'Shared with members', 940001, true)
ON CONFLICT (id) DO NOTHING;

INSERT INTO document_node (id, tenant_id, document_id, parent_id, ordering)
VALUES
  ('11111111-1111-7111-8111-111111111111', 1300, 930001, null, 1),
  ('22222222-2222-7222-8222-222222222222', 1300, 930002, null, 1),
  ('33333333-3333-7333-8333-333333333333', 1300, 930002,
   '22222222-2222-7222-8222-222222222222', 1)
ON CONFLICT (id) DO NOTHING;

INSERT INTO document_node_tag (tenant_id, node_id, person_id)
VALUES (1300, '22222222-2222-7222-8222-222222222222', 240001)
ON CONFLICT DO NOTHING;

GRANT USAGE ON SCHEMA tap TO member, trainer;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA tap TO member, trainer;

-- Constraints are checked as the owner, before any role switching.
SELECT tap.throws_ok(
  $$
    insert into document (tenant_id, kind, title, event_instance_id, cohort_id)
    values (1300, 'plan', 'Two subjects', 940001, 1)
  $$,
  '23514',
  null,
  'a document cannot have more than one subject'
);

SELECT tap.throws_ok(
  $$
    insert into document_node (id, tenant_id, document_id, parent_id, ordering)
    values ('44444444-4444-7444-8444-444444444444', 1300, 930001,
            '22222222-2222-7222-8222-222222222222', 1)
  $$,
  '23503',
  null,
  'a node cannot be parented to a node in another document'
);

SELECT set_config('jwt.claims.user_id', '250001', true);
SELECT set_config('jwt.claims.my_person_ids', '[240002]', true);
SET LOCAL ROLE trainer;

SELECT tap.is(
  (SELECT array_agg(id ORDER BY id) FROM document WHERE id BETWEEN 930001 AND 930002),
  ARRAY[930001, 930002]::bigint[],
  'trainers see every document in their tenant'
);

SELECT tap.lives_ok(
  $$
    do $body$
    declare
      new_document_id bigint;
      new_node_id uuid := '55555555-5555-7555-8555-555555555555';
    begin
      insert into document (kind, title, event_instance_id)
      values ('plan', 'Trainer plan', 940001)
      returning id into new_document_id;

      insert into document_node (id, document_id, ordering)
      values (new_node_id, new_document_id, 1);

      insert into document_node_tag (node_id, person_id)
      values (new_node_id, 240002);
    end
    $body$
  $$,
  'trainers can create a document, its nodes and their tags'
);

RESET ROLE;
SELECT set_config('jwt.claims.user_id', '250002', true);
SELECT set_config('jwt.claims.my_person_ids', '[240001]', true);
SET LOCAL ROLE member;

SELECT tap.is(
  (SELECT array_agg(id ORDER BY id) FROM document WHERE id BETWEEN 930001 AND 930002),
  ARRAY[930002]::bigint[],
  'members only see documents marked show_to_members'
);

SELECT tap.is(
  (SELECT count(*) FROM document_node
   WHERE document_id BETWEEN 930001 AND 930002),
  2::bigint,
  'members only see the nodes of documents they can read'
);

SELECT tap.lives_ok(
  $$
    do $body$
    declare
      changed integer;
    begin
      update document set title = 'Blocked update' where id = 930001;
      get diagnostics changed = row_count;
      if changed <> 0 then
        raise exception 'updated % rows', changed;
      end if;
    end
    $body$
  $$,
  'members cannot modify documents'
);

RESET ROLE;
SELECT tap.finish();

ROLLBACK;
