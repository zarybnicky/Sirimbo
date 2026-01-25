CREATE FUNCTION app_private.can_trainer_edit_event(eid bigint) RETURNS boolean
    LANGUAGE sql STABLE SECURITY DEFINER LEAKPROOF PARALLEL SAFE
    AS $$
  select exists (
    select 1 from event_trainer where eid = event_id and person_id = any ((select current_person_ids())::bigint[])
  ) or not exists (
    select 1 from event_trainer where eid = event_id
  );
$$;

GRANT ALL ON FUNCTION app_private.can_trainer_edit_event(eid bigint) TO anonymous;
