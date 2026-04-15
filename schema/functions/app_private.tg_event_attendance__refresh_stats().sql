CREATE FUNCTION app_private.tg_event_attendance__refresh_stats() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if tg_op in ('UPDATE', 'DELETE') then
    perform app_private.refresh_event_instance_stats(old.instance_id);
  end if;
  if tg_op in ('INSERT', 'UPDATE') then
    if tg_op = 'INSERT'
      or new.instance_id is distinct from old.instance_id
      or new.status is distinct from old.status
    then
      perform app_private.refresh_event_instance_stats(new.instance_id);
    end if;
  end if;
  return null;
end;
$$;
