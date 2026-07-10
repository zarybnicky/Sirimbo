CREATE FUNCTION app_private.tg__set_event_id_from_registration_id() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  select registration.event_id into new.event_id
  from public.event_instance_registration registration
  where registration.id = new.registration_id;
  return new;
end;
$$;
