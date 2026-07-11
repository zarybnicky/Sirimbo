CREATE FUNCTION app_private.event_instance_manager_person_ids(p_instance_id bigint, p_event_id bigint) RETURNS bigint[]
    LANGUAGE sql STABLE PARALLEL SAFE
    AS $$
  with recursive chain as (
    select id, parent_id
    from public.event_instance where id = p_instance_id
    union all
    select parent.id, parent.parent_id
    from public.event_instance parent
    join chain child on child.parent_id = parent.id
  ), managers as (
    select distinct person_id from public.event_instance_trainer
    where instance_id in (select id from chain)
  )
  select coalesce(array_agg(person_id order by person_id), '{}'::bigint[])
  from managers;
$$;
