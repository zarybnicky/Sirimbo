CREATE FUNCTION app_private.refresh_event_instance_manager_person_ids(p_instance_id bigint, p_event_id bigint) RETURNS boolean
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'pg_temp'
    AS $$
  with recursive affected as (
    select id, event_id from public.event_instance where id = p_instance_id
    union all
    select child.id, child.event_id
    from public.event_instance child
    join affected parent on child.parent_id = parent.id
  ), desired as (
    select id, app_private.event_instance_manager_person_ids(id, event_id) as person_ids
    from affected
  ), changed as (
    update public.event_instance instance
    set manager_person_ids = desired.person_ids
    from desired
    where instance.id = desired.id
      and instance.manager_person_ids is distinct from desired.person_ids
    returning 1
  )
  select exists(select 1 from changed);
$$;

GRANT ALL ON FUNCTION app_private.refresh_event_instance_manager_person_ids(p_instance_id bigint, p_event_id bigint) TO anonymous;
