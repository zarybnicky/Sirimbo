CREATE FUNCTION app_private.tg_event__refresh_manager_person_ids() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  perform app_private.refresh_event_manager_person_ids(new.id);
  return null;
end;
$$;
