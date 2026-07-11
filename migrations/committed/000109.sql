--! Previous: sha1:630dd471760bdc4a418d643c95da3682b38a498c
--! Hash: sha1:cf3c111cf5631b429111cc918fb3328c4d2d2186

--! split: 1-current.sql
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

--! Included functions/set_event_instance_registration.sql
create or replace function public.set_event_instance_registration(
  p_instance_id bigint,
  p_person_id bigint,
  p_couple_id bigint,
  p_is_registered boolean
) returns public.event_instance_registration
  language plpgsql
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
declare
  target_instance event_instance;
  registration event_instance_registration;
  required_capacity integer;
  remaining_capacity integer;
begin
  if p_is_registered is null or num_nonnulls(p_person_id, p_couple_id) <> 1 then
    raise exception 'INVALID_REGISTRANT' using errcode = '22023';
  end if;

  if (p_person_id is not null
      and p_person_id <> all(coalesce(current_person_ids(), '{}'::bigint[])))
    or (p_couple_id is not null
      and p_couple_id <> all(coalesce(current_couple_ids(), '{}'::bigint[]))) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  select * into target_instance
  from event_instance
  where id = p_instance_id
    and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;
  if target_instance.event_id is not null or target_instance.is_locked then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  select * into registration
  from event_instance_registration
  where instance_id = p_instance_id
    and parent_registration_id is null
    and legacy_registration_id is null
    and person_id is not distinct from p_person_id
    and couple_id is not distinct from p_couple_id;

  if not p_is_registered then
    if registration is null then
      raise exception 'REGISTRATION_NOT_FOUND' using errcode = '22023';
    end if;

    update event_instance_registration
    set registration_status = 'cancelled'
    where id = registration.id or parent_registration_id = registration.id;

    select * into registration from event_instance_registration where id = registration.id;
    return registration;
  end if;

  if target_instance.is_cancelled
    or target_instance.until <= now()
    or not (
      coalesce(target_instance.is_public, false)
      or (
        coalesce(target_instance.is_visible, false)
        and current_tenant_id() = any(my_tenants_array())
      )
    ) then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  if registration is not null and registration.registration_status = 'active' then
    return registration;
  end if;

  remaining_capacity := event_instance_remaining_person_spots(target_instance);
  required_capacity := case
    when target_instance.capacity_unit = 'people' and p_couple_id is not null then 2
    else 1
  end;
  if remaining_capacity is not null and remaining_capacity < required_capacity then
    raise exception 'CAPACITY_EXCEEDED' using errcode = '22023';
  end if;

  if registration is not null then
    update event_instance_registration
    set registration_status = 'active'
    where id = registration.id or parent_registration_id = registration.id;
  else
    insert into event_instance_registration (
      instance_id, person_id, couple_id, status
    ) values (
      p_instance_id, p_person_id, p_couple_id,
      case when p_person_id is not null then 'unknown'::attendance_type end
    ) returning * into registration;

    insert into event_instance_registration (
      instance_id, parent_registration_id, person_id, status
    )
    select p_instance_id, registration.id, person.person_id, 'unknown'
    from couple
    cross join lateral unnest(array[couple.man_id, couple.woman_id]) person(person_id)
    where couple.id = p_couple_id;
  end if;

  select * into registration from event_instance_registration where id = registration.id;
  return registration;
end;
$$;

select verify_function('public.set_event_instance_registration');
grant execute on function public.set_event_instance_registration(bigint, bigint, bigint, boolean)
  to anonymous;
--! EndIncluded functions/set_event_instance_registration.sql
--! Included functions/set_lesson_demand.sql
drop function if exists set_lesson_demand(bigint, bigint, integer);

CREATE FUNCTION set_lesson_demand(instance_registration_id bigint, instance_trainer_id bigint, lesson_count integer) RETURNS event_lesson_demand
  LANGUAGE plpgsql STRICT SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
declare
  registration event_instance_registration;
  instance event_instance;
  trainer event_instance_trainer;
  lesson_demand event_lesson_demand;
  other_lessons bigint;
begin
  select * into registration
  from event_instance_registration
  where id = $1 and parent_registration_id is null;

  if not found then
    raise exception 'REGISTRATION_NOT_FOUND' using errcode = '28000';
  end if;

  select * into instance
  from event_instance
  where id = registration.instance_id;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '28000';
  end if;
  if instance.is_locked then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  select * into trainer
  from event_instance_trainer
  where id = $2 and instance_id = instance.id
  for update;

  if not found then
    raise exception 'TRAINER_NOT_FOUND' using errcode = '28000';
  end if;

  if $3 = 0 then
    delete from event_lesson_demand eld where eld.registration_id = registration.id and eld.trainer_id = $2;
    return null;
  end if;
  if trainer.lessons_offered = 0 then
    raise exception 'LESSONS_NOT_OFFERED' using errcode = '28000';
  end if;
  if trainer.lessons_offered is not null then
    select coalesce(sum(eld.lesson_count), 0) into other_lessons
    from event_lesson_demand eld
    join event_instance_registration other_registration
      on other_registration.id = eld.registration_id
     and other_registration.registration_status = 'active'
    where eld.trainer_id = $2 and eld.registration_id <> registration.id;

    if $3 > trainer.lessons_offered - other_lessons then
      raise exception 'LESSON_LIMIT_EXCEEDED' using errcode = '22023';
    end if;
  end if;

  INSERT INTO event_lesson_demand (registration_id, trainer_id, lesson_count)
  values (registration.id, $2, $3)
  on conflict on constraint eld_unique_registration_trainer_key do update set lesson_count = $3
  returning * into lesson_demand;

  return lesson_demand;
end;
$$;

select verify_function('set_lesson_demand');
GRANT ALL ON FUNCTION set_lesson_demand(bigint, bigint, integer) TO anonymous;
--! EndIncluded functions/set_lesson_demand.sql
--! Included functions/register_to_event_many.sql
drop function if exists register_to_event_many(register_to_event_type[]);

CREATE FUNCTION register_to_event_many(event_registrations register_to_event_type[]) RETURNS SETOF event_registration
    LANGUAGE plpgsql
    SET search_path TO pg_catalog, public, pg_temp
    AS $$
declare
  event event;
  created_ids bigint[] := array[]::bigint[];
  registration register_to_event_type;
  created event_registration;
  demand event_lesson_demand;
  instance_registration_id bigint;
begin
  foreach registration in array event_registrations loop
    select * into event from event where id = registration.event_id;

    if event is null then
      raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
    end if;
    if event.is_locked = true then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;
    if registration.person_id <> all (current_person_ids()) and registration.couple_id <> all (current_couple_ids()) then
      raise exception 'ACCESS_DENIED' using errcode = '42501';
    end if;

    insert into event_registration (event_id, person_id, couple_id, note)
    values (registration.event_id, registration.person_id, registration.couple_id, registration.note)
    returning * into created;
    created_ids := created_ids || created.id;
    if coalesce(cardinality(registration.lessons), 0) > 0 then
      foreach demand in array registration.lessons loop
        select instance_registration.id into instance_registration_id
        from event_instance_trainer trainer
        join event_instance_registration instance_registration
          on instance_registration.instance_id = trainer.instance_id
         and instance_registration.legacy_registration_id = created.id
         and instance_registration.parent_registration_id is null
        where trainer.id = demand.trainer_id;

        if not found then
          raise exception 'TRAINER_NOT_FOUND' using errcode = '28000';
        end if;

        perform set_lesson_demand(instance_registration_id, demand.trainer_id, demand.lesson_count);
      end loop;
    end if;
  end loop;
  return query select * from event_registration where id = any (created_ids);
end;
$$ security definer;

select verify_function('public.register_to_event_many');
GRANT ALL ON FUNCTION public.register_to_event_many TO anonymous;
--! EndIncluded functions/register_to_event_many.sql
--! Included functions/event_remaining_x.sql
CREATE or replace FUNCTION event_trainer_lessons_remaining(e event_trainer) RETURNS integer AS $$
  select case
    when e.lessons_offered is null then null
    else e.lessons_offered - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_demand demand
      join event_instance_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      join event_instance_trainer trainer on trainer.id = demand.trainer_id
      join event_instance instance on instance.id = trainer.instance_id
      where instance.event_id = e.event_id and trainer.person_id = e.person_id
    )
  end;
$$ LANGUAGE sql STABLE;

CREATE or replace FUNCTION event_instance_trainer_lessons_remaining(e event_instance_trainer) RETURNS integer AS $$
  select case
    when e.lessons_offered is null then null
    else e.lessons_offered - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_demand demand
      join event_instance_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      where demand.trainer_id = e.id
    )
  end;
$$ LANGUAGE sql STABLE;

CREATE or replace FUNCTION event_remaining_person_spots(e event) RETURNS integer AS $$
  select e.capacity - (
    select coalesce(sum(case when couple_id is not null then 2 else 1 end), 0)
    from event_registration where event_id = e.id
  ) - (
    select coalesce(count(id), 0)
    from event_external_registration where event_id = e.id
  );
$$ LANGUAGE sql STABLE security definer;

CREATE or replace FUNCTION event_remaining_lessons(e event) RETURNS integer AS $$
  select case
    when exists (
      select 1 from event_trainer et
      where et.event_id = e.id and et.lessons_offered is null
    ) then null
    else (
      select coalesce(sum(lessons_offered), 0) from event_trainer et where et.event_id = e.id
    ) - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_demand demand
      join event_instance_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      where demand.event_id = e.id
    )
  end;
$$ LANGUAGE sql STABLE security definer;

GRANT ALL ON FUNCTION event_trainer_lessons_remaining(e event_trainer) TO anonymous;
GRANT ALL ON FUNCTION event_instance_trainer_lessons_remaining(e event_instance_trainer) TO anonymous;
GRANT ALL ON FUNCTION event_remaining_person_spots(e event) TO anonymous;
GRANT ALL ON FUNCTION event_remaining_lessons(e event) TO anonymous;
--! EndIncluded functions/event_remaining_x.sql
--! Included functions/detach_event_instance.sql
CREATE OR REPLACE FUNCTION public.detach_event_instance(
  p_instance_id     bigint,
  p_new_event_name  text   DEFAULT NULL
)
  RETURNS public.event
  LANGUAGE plpgsql
  SECURITY DEFINER
  SET search_path = pg_catalog, pg_temp
AS $$
DECLARE
  v_old_event_id bigint;
  v_new_event_id bigint;
  v_old_att jsonb;
  v_old_demands jsonb;
BEGIN
  -- Lock the instance and fetch old event_id
  SELECT ei.event_id
  INTO v_old_event_id
  FROM public.event_instance ei
  WHERE ei.id        = p_instance_id
    FOR UPDATE;

  IF v_old_event_id IS NULL THEN
    RAISE EXCEPTION 'detach_event_instance: instance % not found', p_instance_id;
  END IF;

  -- Also lock the parent event row (prevents concurrent edits during cloning)
  PERFORM 1
  FROM public.event e
  WHERE e.id = v_old_event_id
    FOR UPDATE;

  -- Snapshot attendance state for this instance so it can be restored after
  -- the legacy registration bridge is rebuilt for the cloned event.
  SELECT coalesce(jsonb_agg(jsonb_build_object(
    'reg_person_id', er.person_id,
    'reg_couple_id', er.couple_id,
    'attendee_person_id', eir.person_id,
    'registration_status', eir.registration_status,
    'status', eir.status,
    'attendance_note', eir.attendance_note,
    'attendance_created_at', eir.attendance_created_at,
    'attendance_updated_at', eir.attendance_updated_at
  )), '[]'::jsonb)
  INTO v_old_att
  FROM public.event_instance_registration eir
  JOIN public.event_registration er ON er.tenant_id = eir.tenant_id AND er.id = eir.legacy_registration_id AND er.event_id = v_old_event_id
  WHERE eir.instance_id = p_instance_id;

  SELECT coalesce(jsonb_agg(jsonb_build_object(
    'reg_person_id', er.person_id,
    'reg_couple_id', er.couple_id,
    'trainer_id', trainer.id,
    'lesson_count', demand.lesson_count,
    'created_at', demand.created_at
  )), '[]'::jsonb)
  INTO v_old_demands
  FROM public.event_lesson_demand demand
  JOIN public.event_instance_registration eir ON eir.id = demand.registration_id
  JOIN public.event_registration er ON er.id = eir.legacy_registration_id
  JOIN public.event_instance_trainer trainer ON trainer.id = demand.trainer_id
  WHERE eir.instance_id = p_instance_id
    AND eir.parent_registration_id IS NULL;

  -- Reparenting synchronizes the new registrations before sweeping the old
  -- bridge rows, which would collide on the instance/person unit key.
  DELETE FROM public.event_instance_registration eir
  WHERE eir.instance_id = p_instance_id
    AND eir.event_id = v_old_event_id
    AND eir.legacy_registration_id IS NOT NULL;

  -- Clone the event (explicit column list, but short and stable in compact.sql)
  INSERT INTO public.event (
    name, location_text, description, capacity, files_legacy,
    updated_at, is_locked, is_visible, summary, is_public, enable_notes, tenant_id,
    type, location_id, created_at
  )
  SELECT
    COALESCE(p_new_event_name, e.name), e.location_text, e.description, e.capacity, e.files_legacy,
    now(), e.is_locked, e.is_visible, e.summary, e.is_public, e.enable_notes, e.tenant_id,
    e.type, e.location_id, now()
  FROM public.event e
  WHERE e.id = v_old_event_id
  RETURNING id INTO v_new_event_id;

  -- Copy event_target_cohort (natural key: cohort_id)
  INSERT INTO public.event_target_cohort (event_id, cohort_id, created_at, updated_at)
  SELECT v_new_event_id, etc.cohort_id, now(), now()
  FROM public.event_target_cohort etc
  WHERE etc.event_id = v_old_event_id
  ON CONFLICT (event_id, cohort_id) DO NOTHING;

  -- Copy event_trainer (natural key: person_id)
  INSERT INTO public.event_trainer (event_id, person_id, created_at, updated_at, lessons_offered)
  SELECT v_new_event_id, et.person_id, now(), now(), et.lessons_offered
  FROM public.event_trainer et
  WHERE et.event_id  = v_old_event_id
  ON CONFLICT (event_id, person_id) DO NOTHING;

  -- Re-point the instance and its denormalized trainer event IDs.
  UPDATE public.event_instance ei
  SET event_id   = v_new_event_id
  WHERE ei.id        = p_instance_id
    AND ei.event_id  = v_old_event_id;

  UPDATE public.event_instance_trainer
  SET event_id = v_new_event_id
  WHERE instance_id = p_instance_id;

  -- Copy registrations: map target_cohort_id by cohort_id (old_tc -> new_tc)
  INSERT INTO public.event_registration (
    event_id, target_cohort_id, couple_id, person_id, note, created_at
  )
  SELECT v_new_event_id, new_tc.id, er.couple_id, er.person_id, er.note, er.created_at
  FROM public.event_registration er
  LEFT JOIN public.event_target_cohort old_tc ON old_tc.tenant_id = er.tenant_id AND old_tc.id = er.target_cohort_id
  LEFT JOIN public.event_target_cohort new_tc ON new_tc.tenant_id = er.tenant_id AND new_tc.event_id  = v_new_event_id AND new_tc.cohort_id = old_tc.cohort_id
  WHERE er.event_id  = v_old_event_id
  ON CONFLICT (event_id, person_id, couple_id) DO NOTHING;

  INSERT INTO public.event_lesson_demand (
    trainer_id, registration_id, lesson_count, created_at, event_id
  )
  SELECT trainer.id, new_eir.id, old.lesson_count, old.created_at, v_new_event_id
  FROM jsonb_to_recordset(v_old_demands) AS old(
    reg_person_id bigint,
    reg_couple_id bigint,
    trainer_id bigint,
    lesson_count integer,
    created_at timestamptz
  )
  JOIN public.event_instance_trainer trainer
    ON trainer.id = old.trainer_id
   AND trainer.instance_id = p_instance_id
  JOIN public.event_registration new_reg
    ON new_reg.event_id = v_new_event_id
   AND new_reg.person_id IS NOT DISTINCT FROM old.reg_person_id
   AND new_reg.couple_id IS NOT DISTINCT FROM old.reg_couple_id
  JOIN public.event_instance_registration new_eir
    ON new_eir.instance_id = p_instance_id
   AND new_eir.legacy_registration_id = new_reg.id
   AND new_eir.parent_registration_id IS NULL
  ON CONFLICT (registration_id, trainer_id) DO NOTHING;

  -- Restore per-person attendance on the newly generated bridge rows.
  UPDATE public.event_instance_registration eir
  SET registration_status = oa.registration_status,
      status = oa.status,
      attendance_note = oa.attendance_note
  FROM jsonb_to_recordset(v_old_att) AS oa(
    reg_person_id bigint,
    reg_couple_id bigint,
    attendee_person_id bigint,
    registration_status public.event_instance_registration_status,
    status public.attendance_type,
    attendance_note text
  )
  JOIN public.event_registration new_reg ON new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM oa.reg_person_id AND new_reg.couple_id IS NOT DISTINCT FROM oa.reg_couple_id
  WHERE eir.instance_id     = p_instance_id
    AND eir.legacy_registration_id = new_reg.id
    AND eir.person_id IS NOT DISTINCT FROM oa.attendee_person_id;

  -- Restoring status/note correctly counts as an update for the new bridge row,
  -- so restore the original attendance audit lifecycle in a separate statement.
  UPDATE public.event_instance_registration eir
  SET attendance_created_at = oa.attendance_created_at,
      attendance_updated_at = oa.attendance_updated_at
  FROM jsonb_to_recordset(v_old_att) AS oa(
    reg_person_id bigint,
    reg_couple_id bigint,
    attendee_person_id bigint,
    attendance_created_at timestamptz,
    attendance_updated_at timestamptz
  )
  JOIN public.event_registration new_reg ON new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM oa.reg_person_id AND new_reg.couple_id IS NOT DISTINCT FROM oa.reg_couple_id
  WHERE eir.instance_id = p_instance_id
    AND eir.legacy_registration_id = new_reg.id
    AND eir.person_id IS NOT DISTINCT FROM oa.attendee_person_id;

  return (select event from public.event where id = v_new_event_id);
END;
$$;

select verify_function('public.detach_event_instance');
grant execute on function detach_event_instance to anonymous;
--! EndIncluded functions/detach_event_instance.sql
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
      and registration.legacy_registration_id is null
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
    if updated_instance.event_id is not null then
      raise exception 'event-backed instance % registrations must be edited through event_registration', p_instance_id;
    end if;

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(p_registrations) registration
    ), roots as (
      select existing.id
      from public.event_instance_registration existing
      where existing.instance_id = p_instance_id
        and existing.parent_registration_id is null
        and existing.legacy_registration_id is null
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
        and existing.legacy_registration_id is null
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
          and existing.legacy_registration_id is null
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
--! EndIncluded functions/update_event_instance_details.sql
--! Included functions/upsert_event.sql
drop function if exists upsert_event;

drop type if exists event_type_input;
drop type if exists event_instance_type_input;
drop type if exists event_instance_trainer_type_input;
drop type if exists event_trainer_type_input;
drop type if exists event_target_cohort_type_input;
drop type if exists event_registration_type_input;

CREATE TYPE public.event_type_input AS (
  id bigint,
  name text,
  summary text,
  description text,
  type public.event_type,
  location_id bigint,
  location_text text,
  capacity integer,
  is_visible boolean,
  is_public boolean,
  is_locked boolean,
  enable_notes boolean
);

CREATE TYPE public.event_instance_trainer_type_input AS (
  id bigint,
  person_id bigint,
  lessons_offered integer
);

CREATE TYPE public.event_instance_type_input AS (
  id bigint,
  since timestamp with time zone,
  until timestamp with time zone,
  is_cancelled boolean,
  trainers event_instance_trainer_type_input[]
);

CREATE TYPE public.event_trainer_type_input AS (
  id bigint,
  person_id bigint,
  lessons_offered integer
);

CREATE TYPE public.event_target_cohort_type_input AS (
  id bigint,
  cohort_id bigint
);

CREATE TYPE public.event_registration_type_input AS (
  id bigint,
  person_id bigint,
  couple_id bigint
);

CREATE OR REPLACE FUNCTION upsert_event(
  info event_type_input,
  event_instances event_instance_type_input[],
  trainers event_trainer_type_input[],
  cohorts event_target_cohort_type_input[],
  registrations event_registration_type_input[]
) RETURNS event LANGUAGE plpgsql AS $$
declare
  instance event_instance_type_input;
  trainer event_trainer_type_input;
  instance_trainer event_instance_trainer_type_input;
  cohort event_target_cohort_type_input;
  registration event_registration_type_input;
  v_event event;
  v_instance_id bigint;
begin
  if info.id is null then

    insert into event (
      name,
      summary,
      description,
      type,
      location_id,
      location_text,
      capacity,
      is_visible,
      is_public,
      is_locked,
      enable_notes
    )
    values (
      info.name,
      info.summary,
      info.description,
      info.type,
      info.location_id,
      info.location_text,
      info.capacity,
      info.is_visible,
      info.is_public,
      info.is_locked,
      info.enable_notes
    )
    returning * into v_event;
  else
    update event set
      name=info.name,
      summary=info.summary,
      description=info.description,
      type=info.type,
      location_id=info.location_id,
      location_text=info.location_text,
      capacity=info.capacity,
      is_visible=info.is_visible,
      is_public=info.is_public,
      is_locked=info.is_locked,
      enable_notes=info.enable_notes
    where id=info.id
    returning * into v_event;

    if not found then
      raise exception 'event % not found', info.id;
    end if;
  end if;

  foreach instance in array coalesce(event_instances, '{}'::event_instance_type_input[]) loop
    if instance.id is null then
      insert into event_instance (event_id, since, until, is_cancelled)
      values (v_event.id, instance.since, instance.until, instance.is_cancelled)
      returning id into v_instance_id;
    elsif instance.since is null and instance.until is null then
      delete from event_instance where id=instance.id;
      v_instance_id := null;
    else
      update event_instance
      set since=instance.since, until=instance.until, is_cancelled=instance.is_cancelled
      where id=instance.id
      returning id into v_instance_id;
    end if;

    if v_instance_id is not null then
      foreach instance_trainer in array coalesce(instance.trainers, '{}'::event_instance_trainer_type_input[]) loop
        if instance_trainer.id is null then
          insert into event_instance_trainer (instance_id, person_id, lessons_offered)
          values (v_instance_id, instance_trainer.person_id, instance_trainer.lessons_offered)
          on conflict (instance_id, person_id) do update
            set lessons_offered = excluded.lessons_offered;
        elsif instance_trainer.person_id is null then
          delete from event_instance_trainer where id=instance_trainer.id;
        else
          update event_instance_trainer
          set person_id = instance_trainer.person_id,
              lessons_offered = instance_trainer.lessons_offered
          where id = instance_trainer.id and instance_id = v_instance_id;
        end if;
      end loop;
    end if;
  end loop;

  foreach trainer in array coalesce(trainers, '{}'::event_trainer_type_input[]) loop
    if trainer.id is null then
      insert into event_trainer (event_id, person_id, lessons_offered)
      values (v_event.id, trainer.person_id, trainer.lessons_offered)
      on conflict (event_id, person_id) do update
        set lessons_offered = trainer.lessons_offered;
    elsif trainer.person_id is null then
      delete from event_trainer where id=trainer.id;
    else
      update event_trainer set lessons_offered=trainer.lessons_offered where id=trainer.id;
    end if;
  end loop;

  foreach cohort in array coalesce(cohorts, '{}'::event_target_cohort_type_input[]) loop
    if cohort.id is null then
      insert into event_target_cohort (event_id, cohort_id)
      values (v_event.id, cohort.cohort_id)
      on conflict (event_id, cohort_id) do nothing;
    elsif cohort.cohort_id is null then
      delete from event_target_cohort where id=cohort.id;
    end if;
  end loop;

  foreach registration in array coalesce(registrations, '{}'::event_registration_type_input[]) loop
    if registration.id is null then
      insert into event_registration (event_id, person_id, couple_id)
      values (v_event.id, registration.person_id, registration.couple_id)
      on conflict (event_id, person_id, couple_id) do nothing;
    elsif registration.person_id is null and registration.couple_id is null then
      delete from event_registration where id=registration.id;
    else
      update event_registration
      set person_id=registration.person_id, couple_id=registration.couple_id
      where id=registration.id;
    end if;
  end loop;

  return v_event;
end;
$$;

select verify_function('public.upsert_event');

COMMENT ON FUNCTION public.upsert_event is null;
GRANT ALL ON FUNCTION public.upsert_event TO anonymous;
--! EndIncluded functions/upsert_event.sql
