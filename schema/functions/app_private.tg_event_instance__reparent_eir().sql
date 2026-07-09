CREATE FUNCTION app_private.tg_event_instance__reparent_eir() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  if new.event_id is distinct from old.event_id then
    perform app_private.sync_eir_registrations(array(
      select distinct er.id from event_registration er
      where er.event_id in (old.event_id, new.event_id)));
  end if;
  return null;
end;
$$;
