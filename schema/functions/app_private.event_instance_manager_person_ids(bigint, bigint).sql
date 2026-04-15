CREATE FUNCTION app_private.event_instance_manager_person_ids(p_instance_id bigint, p_event_id bigint) RETURNS bigint[]
    LANGUAGE sql STABLE PARALLEL SAFE
    AS $$
  select coalesce(array_agg(s.person_id order by s.person_id), '{}'::bigint[])
  from (
    select eit.person_id
    from public.event_instance_trainer eit
    where eit.instance_id = p_instance_id
    union
    select et.person_id
    from public.event_trainer et
    where et.event_id = p_event_id
  ) s;
$$;
