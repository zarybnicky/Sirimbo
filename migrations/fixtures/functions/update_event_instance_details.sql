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
  p_trainer_lessons_offered integer[] default null
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
    set registration_status = 'cancelled'
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
    set registration_status = 'active'
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
        instance_id, person_id, couple_id, status
      )
      select
        p_instance_id,
        desired.person_id,
        desired.couple_id,
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

  if p_registrations is not null then
    select * into updated_instance
    from public.event_instance
    where id = p_instance_id;
  end if;

  return updated_instance;
end;
$$;

select verify_function('public.update_event_instance_details');
grant all on function public.update_event_instance_details to anonymous;
