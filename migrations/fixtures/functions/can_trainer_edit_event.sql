create or replace function app_private.can_trainer_edit_event(p_event_id bigint)
  returns boolean
  language sql stable security definer leakproof parallel safe
  set search_path = pg_catalog, public, pg_temp
as $$
  select exists (
    select 1 from event
    where id = p_event_id
      and (cardinality(manager_person_ids) = 0 or manager_person_ids && (select current_person_ids()))
  );
$$;

grant all on function app_private.can_trainer_edit_event to anonymous;
