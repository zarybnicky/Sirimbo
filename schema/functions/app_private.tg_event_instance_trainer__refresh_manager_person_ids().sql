CREATE FUNCTION app_private.tg_event_instance_trainer__refresh_manager_person_ids() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if tg_op = 'DELETE' then
    perform app_private.refresh_event_instance_manager_person_ids(old.instance_id);
  elsif tg_op = 'INSERT' then
    perform app_private.refresh_event_instance_manager_person_ids(new.instance_id);
  elsif old.instance_id is distinct from new.instance_id
    or old.person_id is distinct from new.person_id then
    perform app_private.refresh_event_instance_manager_person_ids(old.instance_id);
    perform app_private.refresh_event_instance_manager_person_ids(new.instance_id);
  end if;
  return null;
end;
$$;

REVOKE ALL ON FUNCTION app_private.tg_event_instance_trainer__refresh_manager_person_ids() FROM PUBLIC;
