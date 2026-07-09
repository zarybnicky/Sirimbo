CREATE FUNCTION app_private.tg_event_instance__sync_eir() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  -- @plpgsql_check_options: newtable = changed_rows
  perform app_private.sync_eir_registrations(array(
    select distinct er.id from event_registration er
    join changed_rows ci on ci.event_id = er.event_id));
  return null;
end;
$$;
