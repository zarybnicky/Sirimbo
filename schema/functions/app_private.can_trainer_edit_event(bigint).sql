CREATE FUNCTION app_private.can_trainer_edit_event(p_event_id bigint) RETURNS boolean
    LANGUAGE sql STABLE SECURITY DEFINER LEAKPROOF PARALLEL SAFE
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  with recursive chain as (
    select i.id, i.parent_id from event i where i.id = p_event_id
    union all
    select p.id, p.parent_id from event p join chain c on p.id = c.parent_id
  ),
  trainers as (
    select person_id from event_trainer where event_id in (select id from chain)
  )
  select exists (
    select 1 from trainers where person_id = any ((select current_person_ids())::bigint[])
  ) or not exists (
    select 1 from trainers
  );
$$;

GRANT ALL ON FUNCTION app_private.can_trainer_edit_event(p_event_id bigint) TO anonymous;
