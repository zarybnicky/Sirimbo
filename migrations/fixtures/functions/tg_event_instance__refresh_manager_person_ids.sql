create or replace function app_private.event_instance_manager_person_ids(p_instance_id bigint)
  returns bigint[] language sql stable parallel safe as $$
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

create or replace function app_private.refresh_event_instance_manager_person_ids(p_instance_id bigint)
  returns boolean language sql security definer
  set search_path = pg_catalog, pg_temp as $$
  with recursive affected as (
    select id from public.event_instance where id = p_instance_id
    union all
    select child.id
    from public.event_instance child
    join affected parent on child.parent_id = parent.id
  ), desired as (
    select id, app_private.event_instance_manager_person_ids(id) as person_ids
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

revoke execute on function app_private.event_instance_manager_person_ids(bigint)
  from public, anonymous;
grant execute on function app_private.refresh_event_instance_manager_person_ids(bigint)
  to anonymous;

create or replace function app_private.tg_event_instance__refresh_manager_person_ids()
  returns trigger language plpgsql as $$
begin
  perform app_private.refresh_event_instance_manager_person_ids(new.id);
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

create or replace function app_private.tg_event_instance_trainer__refresh_manager_person_ids()
  returns trigger language plpgsql as $$
begin
  if tg_op = 'DELETE' then
    perform app_private.refresh_event_instance_manager_person_ids(old.instance_id);
  elsif tg_op = 'INSERT' then
    perform app_private.refresh_event_instance_manager_person_ids(new.instance_id);
  elsif old.instance_id is distinct from new.instance_id
    or old.person_id is distinct from new.person_id then
    perform app_private.refresh_event_instance_manager_person_ids(old.instance_id);
    perform app_private.refresh_event_instance_manager_person_ids(new.instance_id);
  end if;
  return null;
end;
$$;
revoke execute on function app_private.tg_event_instance_trainer__refresh_manager_person_ids()
  from public, anonymous;

drop trigger if exists _500_refresh_manager_person_ids on public.event_instance_trainer;
create trigger _500_refresh_manager_person_ids
  after insert or delete or update of instance_id, person_id
  on public.event_instance_trainer
  for each row execute function app_private.tg_event_instance_trainer__refresh_manager_person_ids();
