CREATE FUNCTION public.tg_event_instance__fill_defaults() RETURNS trigger
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
