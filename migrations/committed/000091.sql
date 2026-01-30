--! Previous: sha1:d253e25f3d5fd5d9a36a02242b9203dd1ba8d206
--! Hash: sha1:79d881d37b1d0a478150aa8d1ec4aa0f64020334

--! split: 1-current.sql
ALTER TABLE public.event_instance
  ADD COLUMN IF NOT EXISTS name text,
  ADD COLUMN IF NOT EXISTS type event_type,
  ADD COLUMN IF NOT EXISTS location_text text,
  ADD COLUMN IF NOT EXISTS location_id bigint,
  ADD COLUMN IF NOT EXISTS is_visible boolean,
  ADD COLUMN IF NOT EXISTS is_public boolean,
  ADD COLUMN IF NOT EXISTS custom jsonb NOT NULL DEFAULT '{}'::jsonb;

CREATE or replace FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
declare
  payment_id bigint;
begin
  delete from payment where event_instance_id = OLD.id;

  if not new.is_cancelled then
    select (create_event_instance_payment(i)).id into payment_id
    from event_instance i
    where i.type='lesson'
      and i.id = NEW.id
      and not i.is_cancelled
      and i.since < now()
      and not exists (
        select * from payment where event_instance_id=i.id
      );

    update payment set status ='unpaid' where id = payment_id;
    perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
  end if;

  return OLD;
end;
$$;

select verify_function('app_private.tg_event_instance__delete_payment_on_cancellation', 'event_instance');

DROP TRIGGER IF EXISTS _500_delete_on_cancellation on public.event_instance;

CREATE TRIGGER _500_delete_on_cancellation
  AFTER UPDATE OF is_cancelled ON public.event_instance
  FOR EACH ROW
  EXECUTE FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation();


ALTER TABLE event_instance DISABLE TRIGGER ALL;

UPDATE public.event_instance ei
SET
  name = e.name,
  type = e.type,
  location_text = e.location_text,
  location_id = e.location_id,
  is_visible = e.is_visible,
  is_public = e.is_public
FROM public.event e
WHERE e.tenant_id = ei.tenant_id
  AND e.id = ei.event_id
  AND (ei.name IS NULL OR ei.type IS NULL OR ei.location_text IS NULL OR ei.is_visible IS NULL OR ei.is_public IS NULL);

ALTER TABLE event_instance ENABLE TRIGGER ALL;

CREATE OR REPLACE FUNCTION public.tg_event_instance__fill_defaults()
  RETURNS trigger
  LANGUAGE plpgsql
AS $$
DECLARE e public.event;
BEGIN
  SELECT * INTO STRICT e
  FROM public.event
  WHERE tenant_id = NEW.tenant_id
    AND id = NEW.event_id;

  NEW.custom := COALESCE(NEW.custom, '{}'::jsonb);

  IF NEW.name IS NULL THEN NEW.name := e.name; ELSE
    NEW.custom := NEW.custom || jsonb_build_object('name', true);
  END IF;
  IF NEW.type IS NULL THEN NEW.type := e.type; ELSE
    NEW.custom := NEW.custom || jsonb_build_object('type', true);
  END IF;
  IF NEW.location_text IS NULL THEN NEW.location_text := e.location_text; ELSE
    NEW.custom := NEW.custom || jsonb_build_object('location_text', true);
  END IF;
  IF NEW.location_id IS NULL THEN NEW.location_id := e.location_id; ELSE
    NEW.custom := NEW.custom || jsonb_build_object('location_id', true);
  END IF;
  IF NEW.is_visible IS NULL THEN NEW.is_visible := e.is_visible; ELSE
    NEW.custom := NEW.custom || jsonb_build_object('is_visible', true);
  END IF;
  IF NEW.is_public IS NULL THEN NEW.is_public := e.is_public; ELSE
    NEW.custom := NEW.custom || jsonb_build_object('is_public', true);
  END IF;

  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS _800_event_instance__fill_defaults ON event_instance;
CREATE TRIGGER _800_event_instance__fill_defaults
  BEFORE INSERT ON event_instance
  FOR EACH ROW
EXECUTE FUNCTION public.tg_event_instance__fill_defaults();

CREATE OR REPLACE FUNCTION public.tg_event__propagate_to_instances()
  RETURNS trigger
  LANGUAGE plpgsql
AS $$
BEGIN
  IF (ROW(OLD.name, OLD.type, OLD.location_text, OLD.location_id, OLD.is_visible, OLD.is_public)
    IS NOT DISTINCT FROM
    ROW(NEW.name, NEW.type, NEW.location_text, NEW.location_id, NEW.is_visible, NEW.is_public)) THEN
    RETURN NULL;
  END IF;

  UPDATE event_instance ei
  SET
    name          = CASE WHEN (ei.custom ? 'name')          THEN ei.name          ELSE NEW.name          END,
    type          = CASE WHEN (ei.custom ? 'type')          THEN ei.type          ELSE NEW.type          END,
    location_text = CASE WHEN (ei.custom ? 'location_text') THEN ei.location_text ELSE NEW.location_text END,
    location_id   = CASE WHEN (ei.custom ? 'location_id')   THEN ei.location_id   ELSE NEW.location_id   END,
    is_visible    = CASE WHEN (ei.custom ? 'is_visible')    THEN ei.is_visible    ELSE NEW.is_visible    END,
    is_public     = CASE WHEN (ei.custom ? 'is_public')     THEN ei.is_public     ELSE NEW.is_public     END
  WHERE ei.tenant_id = NEW.tenant_id
    AND ei.event_id = NEW.id
    AND (
      ((NOT (ei.custom ? 'name'))          AND ei.name          IS DISTINCT FROM NEW.name) OR
      ((NOT (ei.custom ? 'type'))          AND ei.type          IS DISTINCT FROM NEW.type) OR
      ((NOT (ei.custom ? 'location_text')) AND ei.location_text IS DISTINCT FROM NEW.location_text) OR
      ((NOT (ei.custom ? 'location_id'))   AND ei.location_id   IS DISTINCT FROM NEW.location_id) OR
      ((NOT (ei.custom ? 'is_visible'))    AND ei.is_visible    IS DISTINCT FROM NEW.is_visible) OR
      ((NOT (ei.custom ? 'is_public'))     AND ei.is_public     IS DISTINCT FROM NEW.is_public)
    );

  RETURN NULL;
END;
$$;

DROP TRIGGER IF EXISTS _800_event__propagate_to_instances ON event;
CREATE TRIGGER _800_event__propagate_to_instances
  AFTER UPDATE ON event
  FOR EACH ROW
EXECUTE FUNCTION public.tg_event__propagate_to_instances();

CREATE OR REPLACE FUNCTION public.tg_event_instance__pin_overrides()
  RETURNS trigger
  LANGUAGE plpgsql
AS $$
DECLARE
  e public.event;
  c jsonb;
BEGIN
  c := COALESCE(NEW.custom, '{}'::jsonb);

  IF NEW.event_id IS DISTINCT FROM OLD.event_id THEN
    SELECT *
    INTO STRICT e
    FROM public.event
    WHERE tenant_id = NEW.tenant_id
      AND id = NEW.event_id;

    IF NOT (c ? 'name') THEN NEW.name := e.name; END IF;
    IF NOT (c ? 'type') THEN NEW.type := e.type; END IF;
    IF NOT (c ? 'location_text') THEN NEW.location_text := e.location_text; END IF;
    IF NOT (c ? 'location_id') THEN NEW.location_id := e.location_id; END IF;
    IF NOT (c ? 'is_visible') THEN NEW.is_visible := e.is_visible; END IF;
    IF NOT (c ? 'is_public') THEN NEW.is_public := e.is_public; END IF;

    NEW.custom := c;
    RETURN NEW;
  END IF;

  IF (NEW.name          IS DISTINCT FROM OLD.name) OR
     (NEW.type          IS DISTINCT FROM OLD.type) OR
     (NEW.location_text IS DISTINCT FROM OLD.location_text) OR
     (NEW.location_id   IS DISTINCT FROM OLD.location_id) OR
     (NEW.is_visible    IS DISTINCT FROM OLD.is_visible) OR
     (NEW.is_public     IS DISTINCT FROM OLD.is_public) OR
     (NEW.custom        IS DISTINCT FROM OLD.custom)
  THEN
    SELECT *
    INTO STRICT e
    FROM public.event
    WHERE tenant_id = NEW.tenant_id
      AND id = NEW.event_id;
  END IF;

  IF NEW.name IS DISTINCT FROM OLD.name THEN
    IF NEW.name IS NOT DISTINCT FROM e.name THEN
      c := c - 'name';
    ELSE
      c := c || jsonb_build_object('name', true);
    END IF;
  END IF;

  IF NEW.type IS DISTINCT FROM OLD.type THEN
    IF NEW.type IS NOT DISTINCT FROM e.type THEN
      c := c - 'type';
    ELSE
      c := c || jsonb_build_object('type', true);
    END IF;
  END IF;

  IF NEW.location_text IS DISTINCT FROM OLD.location_text THEN
    IF NEW.location_text IS NOT DISTINCT FROM e.location_text THEN
      c := c - 'location_text';
    ELSE
      c := c || jsonb_build_object('location_text', true);
    END IF;
  END IF;

  IF NEW.location_id IS DISTINCT FROM OLD.location_id THEN
    IF NEW.location_id IS NOT DISTINCT FROM e.location_id THEN
      c := c - 'location_id';
    ELSE
      c := c || jsonb_build_object('location_id', true);
    END IF;
  END IF;

  IF NEW.is_visible IS DISTINCT FROM OLD.is_visible THEN
    IF NEW.is_visible IS NOT DISTINCT FROM e.is_visible THEN
      c := c - 'is_visible';
    ELSE
      c := c || jsonb_build_object('is_visible', true);
    END IF;
  END IF;

  IF NEW.is_public IS DISTINCT FROM OLD.is_public THEN
    IF NEW.is_public IS NOT DISTINCT FROM e.is_public THEN
      c := c - 'is_public';
    ELSE
      c := c || jsonb_build_object('is_public', true);
    END IF;
  END IF;

  NEW.custom := c;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS _800_event_instance__pin_overrides ON public.event_instance;;
CREATE TRIGGER _800_event_instance__pin_overrides
  BEFORE UPDATE OF
    event_id, name, type, location_text, location_id, is_visible, is_public, custom
  ON public.event_instance
  FOR EACH ROW
EXECUTE FUNCTION public.tg_event_instance__pin_overrides();

CREATE or replace FUNCTION public.event_instances_for_range(
  only_type public.event_type,
  start_range timestamp with time zone,
  end_range timestamp with time zone DEFAULT NULL::timestamp with time zone,
  trainer_ids bigint[] = null,
  only_mine boolean = false
) RETURNS SETOF event_instance as $$
  select i.*
  from event_instance i
  where i.tenant_id = current_tenant_id()
    and (only_type IS NULL OR i.type = only_type)
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_trainer where person_id = any (trainer_ids) and event_id = i.event_id)
      or exists (select 1 from event_instance_trainer where person_id = any (trainer_ids) and instance_id = i.id))
    and (only_mine is FALSE or i.event_id in (select r.event_id from event_registration r where r.person_id = any ((select current_person_ids())::bigint[])) or i.event_id in (select r.event_id from event_registration r where r.couple_id = any ((select current_couple_ids())::bigint[])) or i.event_id in (SELECT et2.event_id FROM event_trainer et2 WHERE et2.person_id = ANY ((select current_person_ids())::bigint[]))
      OR i.id IN (
        SELECT eit2.instance_id FROM event_instance_trainer eit2 WHERE eit2.person_id = ANY ((select current_person_ids())::bigint[])));
$$ stable language sql;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;

select app_private.drop_policies('public.event_instance');

CREATE POLICY current_tenant ON public.event_instance AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY admin_same_tenant ON public.event_instance TO administrator USING (true);
CREATE POLICY trainer_same_tenant ON public.event_instance TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
CREATE POLICY member_view ON public.event_instance FOR SELECT TO member USING (is_visible);
CREATE POLICY public_view ON public.event_instance FOR SELECT TO anonymous USING (is_public);


create index if NOT EXISTS event_tenant_visible_public on event (tenant_id) where (is_visible or is_public);

create index if not exists event_instance_tenant_since_idx
  on event_instance (tenant_id, since);

create index if not exists event_instance_tenant_until_idx
  on event_instance (tenant_id, until);
