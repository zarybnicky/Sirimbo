CREATE FUNCTION app_private.tg_event_instance__update_parent_range() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
    UPDATE event
    SET since = (SELECT min(since) FROM event_instance WHERE event_id = NEW.event_id),
        until = (SELECT max(until) FROM event_instance WHERE event_id = NEW.event_id)
    WHERE id = NEW.event_id;
  END IF;

  IF TG_OP = 'UPDATE' OR TG_OP = 'DELETE' THEN
    UPDATE event
    SET since = (SELECT min(since) FROM event_instance WHERE event_id = OLD.event_id),
        until = (SELECT max(until) FROM event_instance where event_id = OLD.event_id)
    WHERE id = OLD.event_id;
  END IF;

  RETURN NULL;
end;
$$;
