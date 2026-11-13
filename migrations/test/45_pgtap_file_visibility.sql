BEGIN;
SET LOCAL search_path = public, tap;
SET LOCAL plan_cache_mode = force_generic_plan;
SELECT tap.no_plan();

INSERT INTO tenant (id, name) VALUES (-9500, 'File visibility'), (-9501, 'Other files');
SELECT set_config('jwt.claims.tenant_id', '-9500', true);
SELECT set_config('jwt.claims.user_id', '', true);
SELECT set_config('jwt.claims.my_person_ids', '{}', true);
SELECT set_config('jwt.claims.my_tenant_ids', '{}', true);
SELECT set_config('jwt.claims.shared.event_ids', '{}', true);

INSERT INTO person (id, first_name, last_name, gender, nationality) OVERRIDING SYSTEM VALUE
VALUES (-9500, 'Cover', 'Attendee', 'unspecified', '');
INSERT INTO file (id, tenant_id, object_key, name, is_public, uploaded_by)
OVERRIDING SYSTEM VALUE
SELECT id, CASE WHEN id = -9510 THEN -9501 ELSE -9500 END,
  'file-visibility-test/' || id, 'cover.webp', id = -9506, null
FROM unnest(ARRAY[-9507,-9506,-9505,-9504,-9503,-9502,-9501,-9500,-9510]) id;
INSERT INTO event (id, tenant_id, parent_id, name, since, until, is_visible, is_public, cover_image_id)
OVERRIDING SYSTEM VALUE VALUES
  (-9500, -9500, null, 'Public cover', now(), now()+interval '1 hour', false, true, -9500),
  (-9501, -9500, null, 'Member cover', now(), now()+interval '1 hour', true, false, -9501),
  (-9502, -9500, null, 'Attendee cover', now(), now()+interval '1 hour', false, false, -9502),
  (-9503, -9500, -9502, 'Child cover', now(), now()+interval '1 hour', false, false, -9503),
  (-9504, -9500, null, 'Private cover', now(), now()+interval '1 hour', false, false, -9504),
  (-9505, -9500, null, 'Cancelled attendee cover', now(), now()+interval '1 hour', false, false, -9505),
  (-9510, -9501, null, 'Foreign public cover', now(), now()+interval '1 hour', false, true, -9510);
INSERT INTO event_registration (id, tenant_id, event_id, person_id, status, registration_status)
OVERRIDING SYSTEM VALUE VALUES
  (-9500, -9500, -9502, -9500, 'unknown', 'active'),
  (-9501, -9500, -9505, -9500, 'unknown', 'cancelled');

SET LOCAL ROLE anonymous;
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM file WHERE id BETWEEN -9510 AND -9500),
  ARRAY[-9506,-9500]::bigint[], 'anonymous sees public files and public event covers only');
SELECT tap.set_eq('SELECT * FROM app_private.accessible_event_ids()',
  'SELECT * FROM app_private.accessible_event_ids(NULL, NULL)',
  'a null candidate list preserves full event visibility');
SELECT tap.is((SELECT count(*) FROM app_private.accessible_event_ids(NULL, '{}'::bigint[])),
  0::bigint, 'an empty candidate list returns no events');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM app_private.accessible_event_ids(
    NULL, ARRAY[-9500,-9500,-9504,-9510,-9599,NULL]::bigint[]) id),
  ARRAY[-9500]::bigint[], 'candidate IDs cannot grant private or foreign access or duplicate events');
SELECT tap.is((SELECT count(*) FROM app_private.accessible_event_ids(
    'private:*'::tsquery, ARRAY[-9500]::bigint[])),
  0::bigint, 'title and candidate filters both apply');

SET LOCAL ROLE member;
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM file WHERE id BETWEEN -9510 AND -9500),
  ARRAY[-9506,-9501,-9500]::bigint[], 'members also see member-visible event covers');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM app_private.accessible_event_ids(
    NULL, ARRAY[-9501,-9502,-9510]::bigint[]) id),
  ARRAY[-9501]::bigint[], 'filtered member access still excludes private and foreign events');

SET LOCAL ROLE anonymous;
SELECT set_config('jwt.claims.my_person_ids', '{-9500}', true);
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM file WHERE id BETWEEN -9510 AND -9500),
  ARRAY[-9506,-9503,-9502,-9500]::bigint[],
  'attendees see parent and child covers, not cancelled registrations or unrelated covers');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM app_private.accessible_event_ids(
    NULL, ARRAY[-9503]::bigint[]) id),
  ARRAY[-9503]::bigint[], 'filtering to a child retains access inherited from a noncandidate parent');
RESET ROLE;
UPDATE event_registration SET registration_status = 'cancelled' WHERE id = -9500;
SET LOCAL ROLE anonymous;
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM file WHERE id BETWEEN -9510 AND -9500),
  ARRAY[-9506,-9500]::bigint[], 'cancellation removes parent and child cover access immediately');

SELECT set_config('jwt.claims.my_person_ids', '{}', true);
SELECT set_config('jwt.claims.shared.event_ids', '{-9504,-9510}', true);
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM file WHERE id BETWEEN -9510 AND -9500),
  ARRAY[-9506,-9504,-9500]::bigint[], 'sharing grants the selected cover without crossing tenants');
SELECT tap.is((SELECT count(*) FROM app_private.accessible_event_ids(NULL, ARRAY[-9510]::bigint[])),
  0::bigint, 'a foreign candidate remains hidden even when its ID is shared');
SELECT set_config('jwt.claims.tenant_id', '-9501', true);
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM file WHERE id BETWEEN -9510 AND -9500),
  ARRAY[-9510]::bigint[], 'switching tenants exposes only that tenant public cover');

SELECT set_config('jwt.claims.tenant_id', '-9500', true);
SELECT set_config('jwt.claims.shared.event_ids', '{}', true);
SET LOCAL ROLE trainer;
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM file WHERE id BETWEEN -9510 AND -9500),
  ARRAY[-9506,-9505,-9504,-9503,-9502,-9501,-9500]::bigint[],
  'trainers see unmanaged event covers without gaining unreferenced or foreign files');
SET LOCAL ROLE administrator;
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM file WHERE id BETWEEN -9510 AND -9500),
  ARRAY[-9507,-9506,-9505,-9504,-9503,-9502,-9501,-9500]::bigint[],
  'administrators retain all local files without gaining foreign covers');
SELECT tap.is((SELECT array_agg(id ORDER BY id) FROM app_private.accessible_event_ids(
    NULL, ARRAY[-9504,-9510]::bigint[]) id),
  ARRAY[-9504]::bigint[], 'filtered administrator event access stays in the request tenant');
RESET ROLE;

SELECT * FROM tap.finish(true);
ROLLBACK;
