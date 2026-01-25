CREATE FUNCTION app_private.tg__set_event_id_from_instance_id() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  select i.event_id into new.event_id from public.event_instance i where i.id = new.instance_id;
  return new;
end $$;
