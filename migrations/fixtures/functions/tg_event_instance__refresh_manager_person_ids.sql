create or replace function app_private.event_instance_manager_person_ids(p_instance_id bigint, p_event_id bigint)
  returns bigint[] language sql stable parallel safe as $$
  with recursive chain as (
    select id, parent_id, event_id
    from public.event_instance where id = p_instance_id
    union all
    select parent.id, parent.parent_id, parent.event_id
    from public.event_instance parent
    join chain child on child.parent_id = parent.id
  ), managers as (
    select person_id from public.event_instance_trainer
    where instance_id in (select id from chain)
    union
    select person_id from public.event_trainer
    where event_id in (select event_id from chain where event_id is not null)
  )
  select coalesce(array_agg(person_id order by person_id), '{}'::bigint[])
  from managers;
$$;

create or replace function app_private.refresh_event_instance_manager_person_ids(p_instance_id bigint, p_event_id bigint)
  returns boolean language sql security definer
  set search_path = pg_catalog, pg_temp as $$
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

create or replace function app_private.tg_event_instance__refresh_manager_person_ids()
  returns trigger language plpgsql as $$
begin
  perform app_private.refresh_event_instance_manager_person_ids(new.id, new.event_id);
  return null;
end;
$$;
select verify_function('app_private.tg_event_instance__refresh_manager_person_ids', 'public.event_instance');
revoke execute on function app_private.tg_event_instance__refresh_manager_person_ids()
  from public, anonymous;

drop trigger if exists _500_refresh_manager_person_ids_from_parent on public.event_instance;
create trigger _500_refresh_manager_person_ids_from_parent
  after insert or update of parent_id on public.event_instance
  for each row execute function app_private.tg_event_instance__refresh_manager_person_ids();
