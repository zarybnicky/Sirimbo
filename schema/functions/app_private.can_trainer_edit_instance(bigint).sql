CREATE FUNCTION app_private.can_trainer_edit_instance(iid bigint) RETURNS boolean
    LANGUAGE sql STABLE SECURITY DEFINER LEAKPROOF PARALLEL SAFE
    SET search_path TO 'pg_catalog', 'pg_temp'
    AS $$
  with recursive chain as (
    select i.id, i.parent_id from public.event_instance i where i.id = iid
    union all
    select p.id, p.parent_id from public.event_instance p join chain c on p.id = c.parent_id
  ),
  trainers as (
    select person_id from public.event_instance_trainer where instance_id in (select id from chain)
  )
  select exists (
    select 1 from trainers where person_id = any ((select public.current_person_ids())::bigint[])
  ) or not exists (
    select 1 from trainers
  );
$$;

GRANT ALL ON FUNCTION app_private.can_trainer_edit_instance(iid bigint) TO anonymous;
