create or replace function app_private.can_trainer_edit_instance(iid bigint)
  returns boolean
  language sql stable security definer leakproof parallel safe
  set search_path = pg_catalog, public, pg_temp
as $$
  with recursive chain as (
    select i.id, i.parent_id from event i where i.id = iid
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

grant all on function app_private.can_trainer_edit_instance to anonymous;
