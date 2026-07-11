--! Previous: sha1:8d07717c264de7529f2348927aef77890f085d64
--! Hash: sha1:4e3a400f98a649a2e67ba7f1aeb85fc25657b266

--! split: 1-current.sql
comment on table public.event_instance is E'@omit create\n@simpleCollections only';
comment on column public.event_instance.event_id is '@omit';
comment on table public.event_series is
  E'@behavior -query:resource:list -query:resource:connection';
comment on table public.event is '@omit';
comment on table public.event_registration is '@omit';
comment on table public.event_target_cohort is '@omit';
comment on table public.event_trainer is '@omit';

alter table public.event_instance
  drop constraint if exists event_instance_tenant_id_id_event_id_ux;
alter table public.event_registration
  drop constraint if exists event_registration_tenant_id_id_event_id_ux;

drop trigger if exists _800_event_instance__fill_defaults on public.event_instance;
drop trigger if exists _800_event_instance__pin_overrides on public.event_instance;
drop function if exists public.tg_event_instance__fill_defaults();
drop function if exists public.tg_event_instance__pin_overrides();

drop trigger if exists _150_eir_legacy_created_at
  on public.event_instance_registration;
drop function if exists app_private.tg_eir__legacy_created_at();

drop trigger if exists _100_event_id on public.event_instance_trainer;
drop trigger if exists _100_event_id on public.event_lesson_demand;
drop trigger if exists _500_refresh_manager_person_ids
  on public.event_instance_trainer;
drop function if exists app_private.tg__set_event_id_from_instance_id();
drop function if exists app_private.tg__set_event_id_from_registration_id();
drop function if exists app_private.tg_event_instance_trainer__refresh_manager_person_ids();
drop function if exists app_private.refresh_event_instance_manager_person_ids(bigint, bigint);
drop function if exists app_private.event_instance_manager_person_ids(bigint, bigint);

alter table public.event_instance_registration drop column if exists event_id;
alter table public.event_instance_trainer drop column if exists event_id;
alter table public.event_lesson_demand drop column if exists event_id;
alter table public.event_instance drop column if exists custom;

select app_private.drop_policies('public.event');
select app_private.drop_policies('public.event_registration');
select app_private.drop_policies('public.event_target_cohort');
select app_private.drop_policies('public.event_trainer');

revoke all on table public.event from anonymous;
revoke all on table public.event_registration from anonymous;
revoke all on table public.event_target_cohort from anonymous;
revoke all on table public.event_trainer from anonymous;
revoke all on sequence public.event_id_seq from anonymous;
revoke all on sequence public.event_registration_id_seq from anonymous;
revoke all on sequence public.event_target_cohort_id_seq from anonymous;
revoke all on sequence public.event_trainer_id_seq from anonymous;

drop function if exists public.detach_event_instance(bigint, text);
drop function if exists public.delete_event_instance(bigint);
drop function if exists public.quick_create_events(public.quick_event_input[]);
do $$
begin
  if to_regtype('public.event_type_input') is not null
    and to_regtype('public.event_instance_type_input') is not null
    and to_regtype('public.event_trainer_type_input') is not null then
    execute 'drop function if exists public.upsert_event(
      public.event_type_input,
      public.event_instance_type_input[],
      public.event_trainer_type_input[]
    )';
  end if;
end
$$;

drop function if exists public.couple_event_instances(public.couple);
drop function if exists public.event_instance_my_registrations(public.event_instance);
drop function if exists public.event_instance_registrations(public.event_instance);
drop function if exists public.event_is_registration_open(public.event);
drop function if exists public.event_my_registrations(public.event);
drop function if exists public.event_registrants(public.event);
drop function if exists public.event_registration_event_lesson_demands_by_registration_id(
  public.event_registration
);
drop function if exists public.event_remaining_lessons(public.event);
drop function if exists public.event_remaining_person_spots(public.event);
drop function if exists public.event_trainer_lessons_remaining(public.event_trainer);
drop function if exists public.event_trainer_name(public.event_trainer);
drop function if exists app_private.can_trainer_edit_event(bigint);
drop function if exists app_private.merge_couples(bigint, bigint);

drop type if exists public.event_instance_type_input;
drop type if exists public.event_instance_trainer_type_input;
drop type if exists public.event_trainer_type_input;
drop type if exists public.event_type_input;

drop trigger if exists _800_event__propagate_to_instances on public.event;
drop function if exists public.tg_event__propagate_to_instances();

--! Included functions/quick_create_event_instances.sql
drop function if exists public.quick_create_event_instances(
  public.quick_event_input[], bigint
);

create or replace function quick_create_event_instances(
  events quick_event_input[],
  parent_id bigint default null,
  p_is_visible boolean default true,
  p_is_public boolean default false,
  p_is_locked boolean default false,
  p_enable_notes boolean default false
) returns setof event_instance
  language plpgsql
as $$
declare
  quick_event quick_event_input;
  created_instance event_instance;
begin
  foreach quick_event in array coalesce(events, '{}'::quick_event_input[]) loop
    insert into event_instance (
      parent_id, since, until, name, type, location_id, location_text,
      capacity, capacity_unit, is_visible, is_public, is_locked, enable_notes,
      description, summary
    ) values (
      parent_id, quick_event.since, quick_event.until, '',
      coalesce(quick_event.type, 'lesson'), quick_event.location_id,
      coalesce(quick_event.location_text, ''),
      case when coalesce(quick_event.type, 'lesson') = 'lesson' then 2 else 0 end,
      'people', p_is_visible, p_is_public, p_is_locked, p_enable_notes, '', ''
    )
    returning * into created_instance;

    with roots as (
      insert into event_instance_registration (
        instance_id, person_id, couple_id, source, status
      )
      select created_instance.id, registration.person_id, registration.couple_id,
        'manager',
        case when registration.person_id is not null then 'unknown'::attendance_type end
      from unnest(quick_event.registrations) registration
      returning id, couple_id
    )
    insert into event_instance_registration (
      instance_id, parent_registration_id, person_id, status
    )
    select created_instance.id, root.id, person.person_id, 'unknown'
    from roots root
    join couple couple on couple.id = root.couple_id
    cross join lateral unnest(array[couple.man_id, couple.woman_id]) person(person_id)
    where root.couple_id is not null;

    insert into event_instance_trainer (instance_id, person_id)
    select distinct created_instance.id, trainer.person_id
    from unnest(quick_event.trainer_person_ids) trainer(person_id)
    where trainer.person_id is not null;

    return next created_instance;
  end loop;
end;
$$;

select verify_function('quick_create_event_instances');
grant execute on function quick_create_event_instances to anonymous;
--! EndIncluded functions/quick_create_event_instances.sql
--! Included functions/tg_event_instance__refresh_manager_person_ids.sql
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
--! EndIncluded functions/tg_event_instance__refresh_manager_person_ids.sql
--! Included functions/update_event_instance_details.sql
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[]
);
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[], public.quick_event_registration_input[]
);
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[], public.quick_event_registration_input[],
  integer, public.event_capacity_unit, boolean
);
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[], public.quick_event_registration_input[],
  integer, public.event_capacity_unit, boolean, integer[]
);
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[], public.quick_event_registration_input[],
  integer, public.event_capacity_unit, boolean, integer[], bigint[]
);

create or replace function public.update_event_instance_details(
  p_instance_id bigint,
  p_since timestamptz,
  p_until timestamptz,
  p_name text,
  p_type public.event_type,
  p_location_id bigint,
  p_location_text text,
  p_is_visible boolean,
  p_is_public boolean,
  p_is_cancelled boolean,
  p_trainer_person_ids bigint[] default null,
  p_registrations public.quick_event_registration_input[] default null,
  p_capacity integer default null,
  p_capacity_unit public.event_capacity_unit default null,
  p_is_locked boolean default null,
  p_trainer_lessons_offered integer[] default null,
  p_cohort_ids bigint[] default null,
  p_enable_notes boolean default null
) returns public.event_instance
  language plpgsql
as $$
declare
  updated_instance public.event_instance;
begin
  if p_registrations is not null then
    perform registration.id
    from public.event_instance_registration registration
    where registration.instance_id = p_instance_id
    order by registration.id
    for update;
  end if;

  update public.event_instance
  set
    since = p_since,
    until = p_until,
    name = p_name,
    type = p_type,
    location_id = p_location_id,
    location_text = coalesce(p_location_text, ''),
    is_visible = coalesce(p_is_visible, is_visible),
    is_public = coalesce(p_is_public, is_public),
    capacity = coalesce(p_capacity, capacity),
    capacity_unit = coalesce(p_capacity_unit, capacity_unit),
    is_locked = coalesce(p_is_locked, is_locked),
    enable_notes = coalesce(p_enable_notes, enable_notes),
    is_cancelled = p_is_cancelled
  where id = p_instance_id
  returning * into updated_instance;

  if not found then
    raise exception 'event instance % not found', p_instance_id;
  end if;

  if p_registrations is not null then
    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(p_registrations) registration
    ), roots as (
      select existing.id
      from public.event_instance_registration existing
      where existing.instance_id = p_instance_id
        and existing.parent_registration_id is null
        and not exists (
          select 1
          from desired
          where desired.person_id is not distinct from existing.person_id
            and desired.couple_id is not distinct from existing.couple_id
        )
    )
    update public.event_instance_registration registration
    set registration_status = 'cancelled',
        target_cohort_id = null,
        source = case when registration.id = roots.id
          then 'manager'::public.event_registration_source end
    from roots
    where registration.registration_status <> 'cancelled'
      and (
        registration.id = roots.id
        or registration.parent_registration_id = roots.id
      );

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(p_registrations) registration
    ), roots as (
      select existing.id
      from public.event_instance_registration existing
      join desired
        on desired.person_id is not distinct from existing.person_id
        and desired.couple_id is not distinct from existing.couple_id
      where existing.instance_id = p_instance_id
        and existing.parent_registration_id is null
    )
    update public.event_instance_registration registration
    set registration_status = 'active',
        target_cohort_id = null,
        source = case when registration.id = roots.id
          then 'manager'::public.event_registration_source end
    from roots
    where registration.registration_status <> 'active'
      and (
        registration.id = roots.id
        or registration.parent_registration_id = roots.id
      );

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(p_registrations) registration
    ), roots as (
      insert into public.event_instance_registration (
        instance_id, person_id, couple_id, source, status
      )
      select
        p_instance_id,
        desired.person_id,
        desired.couple_id,
        'manager',
        case when desired.person_id is not null then 'unknown'::public.attendance_type end
      from desired
      where not exists (
        select 1
        from public.event_instance_registration existing
        where existing.instance_id = p_instance_id
          and existing.parent_registration_id is null
          and existing.person_id is not distinct from desired.person_id
          and existing.couple_id is not distinct from desired.couple_id
      )
      returning id, couple_id
    )
    insert into public.event_instance_registration (
      instance_id, parent_registration_id, person_id, status
    )
    select p_instance_id, roots.id, person.person_id, 'unknown'
    from roots
    join public.couple couple on couple.id = roots.couple_id
    cross join lateral unnest(array[couple.man_id, couple.woman_id]) person(person_id);
  end if;

  if p_cohort_ids is not null then
    insert into public.event_instance_target_cohort (tenant_id, instance_id, cohort_id)
    select distinct updated_instance.tenant_id, p_instance_id, desired.cohort_id
    from unnest(p_cohort_ids) desired(cohort_id)
    where desired.cohort_id is not null
    on conflict (instance_id, cohort_id) do nothing;

    delete from public.event_instance_target_cohort target
    where target.instance_id = p_instance_id
      and not exists (
        select 1
        from unnest(p_cohort_ids) desired(cohort_id)
        where desired.cohort_id = target.cohort_id
      );
  end if;

  if p_trainer_person_ids is not null then
    if p_trainer_lessons_offered is not null
      and cardinality(p_trainer_lessons_offered) <> cardinality(p_trainer_person_ids) then
      raise exception 'trainer lesson offers must match trainers';
    end if;

    delete from public.event_instance_trainer
    where instance_id = p_instance_id
      and not exists (
        select 1 from unnest(p_trainer_person_ids) person(id)
        where person.id = event_instance_trainer.person_id
      );

    insert into public.event_instance_trainer (instance_id, person_id, lessons_offered)
    select distinct on (input.person_id)
      p_instance_id,
      input.person_id,
      case when p_trainer_lessons_offered is null then 0
        else input.lessons_offered end
    from (
      select p_trainer_person_ids[i] person_id,
        p_trainer_lessons_offered[i] lessons_offered
      from generate_subscripts(p_trainer_person_ids, 1) item(i)
    ) input
    where input.person_id is not null
    order by input.person_id
    on conflict (instance_id, person_id) do update
    set lessons_offered = case when p_trainer_lessons_offered is null
      then event_instance_trainer.lessons_offered
      else excluded.lessons_offered end;
  end if;

  if p_registrations is not null or p_cohort_ids is not null then
    select * into updated_instance
    from public.event_instance
    where id = p_instance_id;
  end if;

  return updated_instance;
end;
$$;

select verify_function('public.update_event_instance_details');
grant all on function public.update_event_instance_details to anonymous;
--! EndIncluded functions/update_event_instance_details.sql
