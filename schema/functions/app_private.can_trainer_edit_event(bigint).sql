CREATE FUNCTION app_private.can_trainer_edit_event(eid bigint) RETURNS boolean
    LANGUAGE sql STABLE SECURITY DEFINER LEAKPROOF PARALLEL SAFE
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select exists (
    select 1 from event_trainer where eid = event_id and person_id = any (current_person_ids())
  ) or not exists (
    select 1 from event_trainer where eid = event_id
  );
$$;
