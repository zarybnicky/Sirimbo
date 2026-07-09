CREATE FUNCTION app_private.tg_event_registration__sync_eir() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  -- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
  if tg_op = 'DELETE' then
    perform app_private.sync_eir_registrations(array(select distinct id from deleted_rows));
  else
    perform app_private.sync_eir_registrations(array(select distinct id from changed_rows));
  end if;
  return null;
end;
$$;
