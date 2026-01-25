CREATE FUNCTION app_private.tg__set_event_id_from_registration_id() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  select i.event_id into new.event_id from public.event_registration i where i.id = new.registration_id;
  return new;
end $$;
