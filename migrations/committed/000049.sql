--! Previous: sha1:dc11ad358556cd0354cdd57b975907f739950a86
--! Hash: sha1:2195f336fd0e46f2177fbd8ee0ac7fa222e01ef2

--! split: 1-current.sql
CREATE or replace FUNCTION app_private.tg_event_instance__update_parent_range() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
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

select verify_function('app_private.tg_event_instance__update_parent_range', 'event_instance');

DROP TRIGGER IF EXISTS _500_update_parent_range on public.event_instance;

CREATE TRIGGER _500_update_parent_range
  AFTER INSERT OR UPDATE OR DELETE ON public.event_instance
  FOR EACH ROW
  EXECUTE FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation();

UPDATE event
SET since = (SELECT min(since) FROM event_instance WHERE event_id = event.id),
    until = (SELECT max(until) FROM event_instance where event_id = event.id);
