CREATE FUNCTION app_private.can_trainer_edit_event(p_event_id bigint) RETURNS boolean
    LANGUAGE sql STABLE SECURITY DEFINER LEAKPROOF PARALLEL SAFE
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select exists (
    select 1 from event
    where id = p_event_id
      and (cardinality(manager_person_ids) = 0 or manager_person_ids && (select current_person_ids()))
  );
$$;

GRANT ALL ON FUNCTION app_private.can_trainer_edit_event(p_event_id bigint) TO anonymous;
