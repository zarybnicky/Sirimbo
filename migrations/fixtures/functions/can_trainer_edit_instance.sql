create or replace function app_private.can_trainer_edit_instance(iid bigint)
  returns boolean
  language sql stable security definer leakproof parallel safe
  set search_path = pg_catalog, pg_temp
as $$
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

grant all on function app_private.can_trainer_edit_instance(bigint) to anonymous;
