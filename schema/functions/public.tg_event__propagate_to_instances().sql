CREATE FUNCTION public.tg_event__propagate_to_instances() RETURNS trigger
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
