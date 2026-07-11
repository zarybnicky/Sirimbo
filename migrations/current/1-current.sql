alter table public.event_instance_trainer
  add column if not exists lessons_offered integer default 0;

comment on column public.event_instance_trainer.lessons_offered is
  'Maximum lesson requests for this trainer on this instance; NULL means unlimited.';

do $$
begin
  if exists (
    select 1
    from pg_catalog.pg_constraint
    where conname = 'event_lesson_demand_trainer_id_fkey'
      and conrelid = 'public.event_lesson_demand'::regclass
      and confrelid = 'public.event_trainer'::regclass
  ) then
    if exists (
      select 1
      from public.event_lesson_demand demand
      left join public.event_instance_registration registration
        on registration.id = demand.registration_id
      left join public.event_instance instance
        on instance.id = registration.instance_id
      left join public.event_trainer trainer
        on trainer.id = demand.trainer_id
      where registration.id is null
        or instance.id is null
        or trainer.id is null
        or registration.parent_registration_id is not null
        or demand.tenant_id is distinct from registration.tenant_id
        or demand.tenant_id is distinct from instance.tenant_id
        or demand.tenant_id is distinct from trainer.tenant_id
        or registration.event_id is distinct from instance.event_id
        or registration.event_id is distinct from trainer.event_id
    ) then
      raise exception 'event lesson demand has no matching instance registration and event trainer';
    end if;

    if exists (
      with unique_roots as (
        select event_id, min(id) as instance_id
        from public.event_instance
        where event_id is not null and parent_id is null
        group by event_id
        having count(*) = 1
      ), required as (
        select registration.instance_id, trainer.person_id
        from public.event_lesson_demand demand
        join public.event_instance_registration registration
          on registration.id = demand.registration_id
        join public.event_trainer trainer on trainer.id = demand.trainer_id
        union
        select root.instance_id, trainer.person_id
        from unique_roots root
        join public.event_trainer trainer on trainer.event_id = root.event_id
        where trainer.lessons_offered is distinct from 0
      )
      select 1
      from required
      where exists (
        select 1 from public.event_instance_trainer
        where instance_id = required.instance_id
      ) and not exists (
        select 1 from public.event_instance_trainer
        where instance_id = required.instance_id
          and person_id = required.person_id
      )
    ) then
      raise exception 'lesson trainer is missing from an instance trainer override';
    end if;

    if exists (
      select 1
      from public.event_lesson_demand demand
      join public.event_instance_registration registration
        on registration.id = demand.registration_id
      join public.event_trainer trainer on trainer.id = demand.trainer_id
      group by demand.registration_id, registration.instance_id, trainer.person_id
      having count(*) > 1
    ) then
      raise exception 'event lesson demand trainer mapping is not unique';
    end if;

    alter table public.event_instance_trainer disable trigger _100_timestamps;

    with unique_roots as (
      select event_id, min(id) as instance_id
      from public.event_instance
      where event_id is not null and parent_id is null
      group by event_id
      having count(*) = 1
    ), target_instances as (
      select distinct registration.instance_id, instance.event_id
      from public.event_lesson_demand demand
      join public.event_instance_registration registration
        on registration.id = demand.registration_id
      join public.event_instance instance on instance.id = registration.instance_id
      union
      select root.instance_id, root.event_id
      from unique_roots root
      where exists (
        select 1 from public.event_trainer trainer
        where trainer.event_id = root.event_id
          and trainer.lessons_offered is distinct from 0
      )
    )
    insert into public.event_instance_trainer (
      tenant_id, instance_id, person_id, lessons_offered, created_at, updated_at
    )
    select trainer.tenant_id, target.instance_id, trainer.person_id,
      trainer.lessons_offered, trainer.created_at, trainer.updated_at
    from target_instances target
    join public.event_trainer trainer on trainer.event_id = target.event_id
    where not exists (
      select 1 from public.event_instance_trainer existing
      where existing.instance_id = target.instance_id
    )
    on conflict (instance_id, person_id) do nothing;

    with target_instances as (
      select distinct registration.instance_id
      from public.event_lesson_demand demand
      join public.event_instance_registration registration
        on registration.id = demand.registration_id
      union
      select min(instance.id)
      from public.event_instance instance
      join public.event_trainer trainer on trainer.event_id = instance.event_id
      where instance.parent_id is null
        and trainer.lessons_offered is distinct from 0
      group by instance.event_id
      having count(distinct instance.id) = 1
    )
    update public.event_instance_trainer instance_trainer
    set lessons_offered = trainer.lessons_offered
    from public.event_instance instance, public.event_trainer trainer
    where instance_trainer.instance_id in (select instance_id from target_instances)
      and instance.id = instance_trainer.instance_id
      and trainer.event_id = instance.event_id
      and trainer.person_id = instance_trainer.person_id
      and instance_trainer.lessons_offered is distinct from trainer.lessons_offered;

    alter table public.event_instance_trainer enable trigger _100_timestamps;

    alter table public.event_lesson_demand
      drop constraint eld_unique_registration_trainer_key,
      drop constraint event_lesson_demand_trainer_id_fkey;
    alter table public.event_lesson_demand disable trigger _100_timestamps;

    with mapping as (
      select demand.id as demand_id, instance_trainer.id as trainer_id
      from public.event_lesson_demand demand
      join public.event_instance_registration registration
        on registration.id = demand.registration_id
      join public.event_trainer trainer on trainer.id = demand.trainer_id
      join public.event_instance_trainer instance_trainer
        on instance_trainer.instance_id = registration.instance_id
       and instance_trainer.person_id = trainer.person_id
    )
    update public.event_lesson_demand demand
    set trainer_id = mapping.trainer_id
    from mapping
    where demand.id = mapping.demand_id;

    alter table public.event_lesson_demand enable trigger _100_timestamps;

    if exists (
      select 1
      from public.event_lesson_demand demand
      join public.event_instance_registration registration
        on registration.id = demand.registration_id
      left join public.event_instance_trainer trainer
        on trainer.id = demand.trainer_id
      where trainer.id is null
        or trainer.tenant_id is distinct from demand.tenant_id
        or trainer.instance_id is distinct from registration.instance_id
    ) then
      raise exception 'event lesson demand trainer migration failed';
    end if;

    alter table public.event_lesson_demand
      add constraint eld_unique_registration_trainer_key
        unique (registration_id, trainer_id),
      add constraint event_lesson_demand_trainer_id_fkey
        foreign key (trainer_id)
        references public.event_instance_trainer(id)
        on update cascade on delete cascade;
  end if;
end;
$$;

--! include functions/set_event_instance_registration.sql
--! include functions/set_lesson_demand.sql
--! include functions/register_to_event_many.sql
--! include functions/event_remaining_x.sql
--! include functions/detach_event_instance.sql
--! include functions/update_event_instance_details.sql
--! include functions/upsert_event.sql
