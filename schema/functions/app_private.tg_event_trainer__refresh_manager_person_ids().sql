CREATE FUNCTION app_private.tg_event_trainer__refresh_manager_person_ids() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if tg_op = 'DELETE' then
    perform app_private.refresh_event_manager_person_ids(old.event_id);
  elsif tg_op = 'INSERT' then
    perform app_private.refresh_event_manager_person_ids(new.event_id);
  elsif old.event_id is distinct from new.event_id
    or old.person_id is distinct from new.person_id then
    perform app_private.refresh_event_manager_person_ids(old.event_id);
    perform app_private.refresh_event_manager_person_ids(new.event_id);
  end if;
  return null;
end;
$$;
