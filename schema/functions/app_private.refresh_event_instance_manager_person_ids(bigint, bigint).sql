CREATE FUNCTION app_private.refresh_event_instance_manager_person_ids(p_instance_id bigint, p_event_id bigint) RETURNS boolean
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  with v as (
    select app_private.event_instance_manager_person_ids(p_instance_id, p_event_id) as ids
  ),
  u as (
    update public.event_instance ei set manager_person_ids = v.ids from v
    where ei.id = p_instance_id and ei.event_id = p_event_id and ei.manager_person_ids is distinct from v.ids
    returning 1
  )
  select exists(select 1 from u);
$$;

GRANT ALL ON FUNCTION app_private.refresh_event_instance_manager_person_ids(p_instance_id bigint, p_event_id bigint) TO anonymous;
