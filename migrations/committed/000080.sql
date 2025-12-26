--! Previous: sha1:e0ce45562574c8d99f42e45197535329713d496e
--! Hash: sha1:4b839d8dd233a620605e3bc158d2bc8da56fd249

--! split: 1-current.sql
drop function if exists tenant_account;
CREATE or replace FUNCTION tenant_account(in_currency text, OUT acc account) RETURNS account AS $$
  WITH ins AS (
    INSERT INTO account (tenant_id, person_id, currency)
      VALUES ((select current_tenant_id()), NULL::bigint, COALESCE(in_currency, 'CZK'))
      ON CONFLICT ON CONSTRAINT account_tenant_id_person_id_currency_idx DO NOTHING
      RETURNING *
  )
  SELECT * FROM ins
  UNION ALL
  SELECT account.* FROM account
    WHERE tenant_id = (select current_tenant_id()) AND person_id IS NULL AND currency = COALESCE(in_currency, 'CZK')
  LIMIT 1;
$$ language sql security definer;

grant all on function tenant_account to anonymous;

drop function if exists person_account;
CREATE or replace FUNCTION person_account(p_id bigint, in_currency text, OUT acc account) RETURNS account AS $$
  WITH ins AS (
    INSERT INTO account (tenant_id, person_id, currency)
      VALUES ((select current_tenant_id()), p_id, COALESCE(in_currency, 'CZK'))
      ON CONFLICT ON CONSTRAINT account_tenant_id_person_id_currency_idx DO NOTHING
      RETURNING *
  )
  SELECT * FROM ins
  UNION ALL
  SELECT account.* FROM account
  WHERE tenant_id = (select current_tenant_id()) AND person_id = p_id AND currency = COALESCE(in_currency, 'CZK')
  LIMIT 1;
$$ language sql security definer set search_path = pg_catalog, public, pg_temp;

grant all on function person_account to anonymous;


drop function if exists my_event_instances_for_range;
drop function if exists event_instances_for_range;
CREATE or replace FUNCTION my_event_instances_for_range(
  only_type event_type,
  start_range timestamptz,
  end_range timestamptz DEFAULT NULL,
  trainer_ids bigint[] = null
) RETURNS SETOF event_instance as $$
  select i.*
  from event_instance i
  join event on event_id=event.id
  where event.is_visible
    and i.since <= end_range
    and i.until >= start_range
    and (only_type is null or event.type = only_type)
    and (trainer_ids is null
      OR i.event_id IN (SELECT et.event_id FROM event_trainer et WHERE et.person_id = ANY (trainer_ids))
      OR i.id IN (SELECT eit.instance_id FROM event_instance_trainer eit WHERE eit.person_id = ANY (trainer_ids)))
    and (
      i.event_id IN (
        SELECT r.event_id FROM event_registration r WHERE r.person_id = ANY (current_person_ids()) OR r.couple_id = ANY (current_couple_ids())
      ) OR i.event_id IN (
        SELECT et2.event_id FROM event_trainer et2 WHERE et2.person_id = ANY (current_person_ids())
      ) OR i.id IN (
        SELECT eit2.instance_id FROM event_instance_trainer eit2 WHERE eit2.person_id = ANY (current_person_ids())
      )
    );
$$ stable language sql;
COMMENT ON FUNCTION public.my_event_instances_for_range IS E'@deprecated
@simpleCollections only';
GRANT ALL ON FUNCTION public.my_event_instances_for_range TO anonymous;

CREATE or replace FUNCTION public.event_instances_for_range(
  only_type public.event_type,
  start_range timestamp with time zone,
  end_range timestamp with time zone DEFAULT NULL::timestamp with time zone,
  trainer_ids bigint[] = null,
  only_mine boolean = false
) RETURNS SETOF event_instance as $$
  select i.*
  from event_instance i
  join event on event_id=event.id
  where i.since <= COALESCE(end_range, 'infinity'::timestamptz)
    and i.until >= start_range
    AND i.event_id IN (SELECT e.id FROM public.event e WHERE e.is_visible AND (only_type IS NULL OR e.type = only_type))
    and (trainer_ids is null
      or exists (select 1 from event_trainer where person_id = any (trainer_ids) and event_id = event.id)
      or exists (select 1 from event_instance_trainer where person_id = any (trainer_ids) and instance_id=i.id))
    and (only_mine is FALSE
      or i.event_id IN (
        SELECT r.event_id FROM event_registration r WHERE r.person_id = ANY (current_person_ids()) OR r.couple_id = ANY (current_couple_ids()))
      OR i.event_id IN (
        SELECT et2.event_id FROM event_trainer et2 WHERE et2.person_id = ANY (current_person_ids()))
      OR i.id IN (
        SELECT eit2.instance_id FROM event_instance_trainer eit2 WHERE eit2.person_id = ANY (current_person_ids())));
$$ stable language sql;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;


CREATE INDEX IF NOT EXISTS event_tenant_visible_public_idx ON event (tenant_id, is_public, is_visible);
CREATE INDEX IF NOT EXISTS event_visible_public_tenant_idx ON event (is_public, is_visible, tenant_id);
CREATE INDEX IF NOT EXISTS event_instance_tenant_range_gist ON event_instance USING gist (tenant_id, range);

CREATE INDEX IF NOT EXISTS event_registration_tenant_person_event_idx ON event_registration (tenant_id, person_id, event_id)
  WHERE person_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS event_registration_tenant_couple_event_idx ON event_registration (tenant_id, couple_id, event_id)
  WHERE couple_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS event_trainer_tenant_person_event_idx ON event_trainer (tenant_id, person_id, event_id);
CREATE INDEX IF NOT EXISTS event_instance_trainer_tenant_person_instance_idx ON event_instance_trainer (tenant_id, person_id, instance_id);
