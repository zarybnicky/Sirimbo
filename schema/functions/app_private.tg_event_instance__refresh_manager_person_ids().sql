CREATE FUNCTION app_private.tg_event_instance__refresh_manager_person_ids() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  perform app_private.refresh_event_instance_manager_person_ids(new.id, new.event_id);
  return null;
end;
$$;

REVOKE ALL ON FUNCTION app_private.tg_event_instance__refresh_manager_person_ids() FROM PUBLIC;
