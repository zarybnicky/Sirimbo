CREATE FUNCTION app_private.tg_event_registration__create_attendance() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  insert into event_attendance (registration_id, instance_id, person_id)
  select NEW.id, event_instance.id, app_private.event_registration_person_ids(NEW) from event_instance where event_id = NEW.event_id
  on conflict do nothing;
  return NEW;
end;
$$;
