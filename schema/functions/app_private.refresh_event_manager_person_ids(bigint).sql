CREATE FUNCTION app_private.refresh_event_manager_person_ids(p_event_id bigint) RETURNS void
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  with recursive desired as (
    select id, app_private.event_manager_person_ids(id) as person_ids
    from event where id = p_event_id
    union all
    select child.id, array(
      select distinct person_id
      from unnest(parent.person_ids || array(
        select person_id from event_trainer where event_id = child.id
      )) person_id
      order by person_id
    )
    from event child
    join desired parent on child.parent_id = parent.id
  )
  update event instance
  set manager_person_ids = desired.person_ids
  from desired
  where instance.id = desired.id
    and instance.id = any(array(select id from desired))
    and instance.manager_person_ids is distinct from desired.person_ids
$$;

GRANT ALL ON FUNCTION app_private.refresh_event_manager_person_ids(p_event_id bigint) TO anonymous;
