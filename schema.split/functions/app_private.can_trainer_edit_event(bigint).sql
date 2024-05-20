CREATE FUNCTION app_private.can_trainer_edit_event(eid bigint) RETURNS boolean
    LANGUAGE sql SECURITY DEFINER
    AS $$
  select (
    select count(person_id) > 0 from event_trainer where eid = event_id and person_id in (select my_person_ids())
  ) or not exists (
    select 1 from event_trainer where eid = event_id
  );
$$;

GRANT ALL ON FUNCTION app_private.can_trainer_edit_event(eid bigint) TO anonymous;


