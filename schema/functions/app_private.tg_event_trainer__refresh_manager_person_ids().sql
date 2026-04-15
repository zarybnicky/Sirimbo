CREATE FUNCTION app_private.tg_event_trainer__refresh_manager_person_ids() RETURNS trigger
    LANGUAGE plpgsql
    SET search_path TO 'pg_catalog', 'public'
    AS $$
begin
  if tg_op in ('DELETE', 'UPDATE') then
    if tg_op = 'DELETE' or old.event_id is distinct from new.event_id or old.person_id is distinct from new.person_id then
      perform app_private.refresh_event_instance_manager_person_ids(ei.id, ei.event_id)
      from public.event_instance ei
      where ei.event_id = old.event_id;
    end if;
  end if;
  if tg_op in ('INSERT', 'UPDATE') then
    if tg_op = 'INSERT' or old.event_id is distinct from new.event_id or old.person_id is distinct from new.person_id then
      perform app_private.refresh_event_instance_manager_person_ids(ei.id, ei.event_id)
      from public.event_instance ei
      where ei.event_id = new.event_id;
    end if;
  end if;
  return null;
end;
$$;
