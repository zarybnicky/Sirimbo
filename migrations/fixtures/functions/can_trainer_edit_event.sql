create or replace function app_private.can_trainer_edit_event(eid bigint) returns boolean language sql as $$
  select exists (
    select 1 from event_trainer where eid = event_id and person_id = any ((select current_person_ids())::bigint[])
  ) or not exists (
    select 1 from event_trainer where eid = event_id
  );
$$ security definer stable leakproof parallel safe;

grant all on function app_private.can_trainer_edit_event to anonymous;
