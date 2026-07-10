--! include functions/update_event_instance_details.sql

alter function app_private.sync_eir_registrations(bigint[])
  set search_path = pg_catalog, public, pg_temp;

with camps as (
  select event.tenant_id, event.id as event_id,
    min(instance.since) as since, max(instance.until) as until
  from public.event event
  join public.event_instance instance on instance.event_id = event.id
  where event.type = 'camp'
    and instance.parent_id is null
  group by event.tenant_id, event.id
  having count(*) > 1
), parents as (
  insert into public.event_instance (tenant_id, event_id, since, until)
  select tenant_id, event_id, since, until from camps
  returning id, event_id
)
update public.event_instance child
set parent_id = parent.id
from parents parent
where child.event_id = parent.event_id
  and child.id <> parent.id
  and child.parent_id is null;

do $$
begin
  if exists (
    select 1 from pg_catalog.pg_constraint
    where conrelid = 'public.event_lesson_demand'::regclass
      and conname = 'event_lesson_demand_tenant_id_registration_id_event_id_fkey'
  ) then
    if exists (
      select demand.id
      from public.event_lesson_demand demand
      join public.event_registration registration
        on registration.id = demand.registration_id
      left join public.event_instance_registration instance_registration
        on instance_registration.legacy_registration_id = registration.id
       and instance_registration.parent_registration_id is null
      left join public.event_instance instance
        on instance.id = instance_registration.instance_id
       and instance.parent_id is null
      group by demand.id
      having count(instance.id) <> 1
    ) then
      raise exception 'event lesson demand does not map to one root instance registration';
    end if;

    alter table public.event_lesson_demand
      drop constraint event_lesson_demand_tenant_id_registration_id_event_id_fkey;
    drop trigger if exists _100_event_id on public.event_lesson_demand;
    alter table public.event_lesson_demand alter column event_id drop not null;
    alter table public.event_lesson_demand disable trigger _100_timestamps;

    with mapping as (
      select demand.id as demand_id, min(instance_registration.id) as registration_id
      from public.event_lesson_demand demand
      join public.event_registration registration
        on registration.id = demand.registration_id
      join public.event_instance_registration instance_registration
        on instance_registration.legacy_registration_id = registration.id
       and instance_registration.parent_registration_id is null
      join public.event_instance instance
        on instance.id = instance_registration.instance_id
       and instance.parent_id is null
      group by demand.id
    )
    update public.event_lesson_demand demand
    set registration_id = mapping.registration_id
    from mapping
    where demand.id = mapping.demand_id;

    alter table public.event_lesson_demand enable trigger _100_timestamps;

    alter table public.event_lesson_demand
      add constraint event_lesson_demand_registration_id_fkey
      foreign key (registration_id)
      references public.event_instance_registration(id)
      on update cascade on delete cascade;
  end if;
end;
$$;

--! include functions/tg__set_event_id_from_registration_id.sql

drop trigger if exists _100_event_id on public.event_lesson_demand;
create trigger _100_event_id
  before insert or update of registration_id on public.event_lesson_demand
  for each row execute function app_private.tg__set_event_id_from_registration_id();

--! include policies/event_lesson_demand.sql
--! include functions/event_registration_lesson_demands.sql
--! include functions/set_lesson_demand.sql
--! include functions/event_remaining_x.sql
--! include functions/detach_event_instance.sql
