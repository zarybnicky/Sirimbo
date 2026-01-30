CREATE FUNCTION public.tg_event_instance__pin_overrides() RETURNS trigger
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
