--! Previous: sha1:50184d5b63499e7ac945f05c8c5b480259cd3255
--! Hash: sha1:630dd471760bdc4a418d643c95da3682b38a498c

--! split: 1-current.sql
--! Included functions/update_event_instance_details.sql
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[]
);
drop function if exists public.update_event_instance_details(
  bigint, timestamptz, timestamptz, text, public.event_type, bigint, text,
  boolean, boolean, boolean, bigint[], public.quick_event_registration_input[]
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
  p_is_locked boolean default null
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
    delete from public.event_instance_trainer
    where instance_id = p_instance_id;

    insert into public.event_instance_trainer (instance_id, person_id)
    select distinct p_instance_id, input.person_id
    from unnest(p_trainer_person_ids) as input(person_id)
    where input.person_id is not null
    on conflict (instance_id, person_id) do nothing;
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

--! Included functions/tg__set_event_id_from_registration_id.sql
create or replace function app_private.tg__set_event_id_from_registration_id()
  returns trigger
  language plpgsql
as $$
begin
  select registration.event_id into new.event_id
  from public.event_instance_registration registration
  where registration.id = new.registration_id;
  return new;
end;
$$;

select verify_function(
  'app_private.tg__set_event_id_from_registration_id',
  'public.event_lesson_demand'
);
--! EndIncluded functions/tg__set_event_id_from_registration_id.sql

drop trigger if exists _100_event_id on public.event_lesson_demand;
create trigger _100_event_id
  before insert or update of registration_id on public.event_lesson_demand
  for each row execute function app_private.tg__set_event_id_from_registration_id();

--! Included policies/event_lesson_demand.sql
select app_private.drop_policies('public.event_lesson_demand');
CREATE POLICY admin_all ON public.event_lesson_demand TO administrator USING (true);
CREATE POLICY view_visible_instance ON public.event_lesson_demand FOR SELECT USING (
  registration_id IN (SELECT id FROM public.event_instance_registration)
);
GRANT ALL ON public.event_lesson_demand TO anonymous;
--! EndIncluded policies/event_lesson_demand.sql
--! Included functions/event_registration_lesson_demands.sql
create or replace function public.event_registration_event_lesson_demands_by_registration_id(
  registration public.event_registration
) returns setof public.event_lesson_demand
  language sql stable
as $$
  select demand.*
  from public.event_lesson_demand demand
  join public.event_instance_registration instance_registration
    on instance_registration.id = demand.registration_id
  where instance_registration.legacy_registration_id = registration.id;
$$;

comment on function public.event_registration_event_lesson_demands_by_registration_id(public.event_registration)
  is E'@simpleCollections only\n@filterable\n@sortable';
grant execute on function public.event_registration_event_lesson_demands_by_registration_id(public.event_registration)
  to anonymous;
--! EndIncluded functions/event_registration_lesson_demands.sql
--! Included functions/set_lesson_demand.sql
CREATE or replace FUNCTION set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer) RETURNS event_lesson_demand
  LANGUAGE plpgsql STRICT SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
declare
  v_event event;
  v_trainer event_trainer;
  legacy_registration event_registration;
  registration event_instance_registration;
  registration_count bigint;
  instance_registration_id bigint;
  lesson_demand event_lesson_demand;
  other_lessons bigint;
begin
  select * into legacy_registration from event_registration where id = $1;
  select * into v_event from event where id = legacy_registration.event_id;
  select * into v_trainer from event_trainer where id = $2 and event_id = legacy_registration.event_id;

  if v_event is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if v_event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if v_trainer is null then
    raise exception 'TRAINER_NOT_FOUND' using errcode = '28000';
  end if;

  select count(*), min(eir.id)
  into registration_count, instance_registration_id
  from event_instance_registration eir
  join event_instance instance on instance.id = eir.instance_id
  where eir.legacy_registration_id = legacy_registration.id
    and eir.parent_registration_id is null
    and instance.parent_id is null;

  if registration_count <> 1 then
    raise exception 'LESSON_DEMAND_INSTANCE_AMBIGUOUS' using errcode = '22023';
  end if;

  select * into registration
  from event_instance_registration
  where id = instance_registration_id;

  if $3 = 0 then
    delete from event_lesson_demand eld where eld.registration_id = registration.id and eld.trainer_id = $2;
    return null;
  end if;
  if v_trainer.lessons_offered = 0 then
    raise exception 'LESSONS_NOT_OFFERED' using errcode = '28000';
  end if;
  if v_trainer.lessons_offered is not null then
    select coalesce(sum(eld.lesson_count), 0) into other_lessons
    from event_lesson_demand eld
    where eld.trainer_id = $2 and eld.registration_id <> registration.id;

    if $3 > v_trainer.lessons_offered - other_lessons then
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
GRANT ALL ON FUNCTION set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer) TO anonymous;
--! EndIncluded functions/set_lesson_demand.sql
--! Included functions/event_remaining_x.sql
CREATE or replace FUNCTION event_trainer_lessons_remaining(e event_trainer) RETURNS integer AS $$
  select case
    when e.lessons_offered is null then null
    else e.lessons_offered - (
      select coalesce(sum(lesson_count), 0)
      from event_lesson_demand where trainer_id = e.id
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
      select coalesce(sum(lesson_count), 0) from event_lesson_demand eld where eld.event_id = e.id
    )
  end;
$$ LANGUAGE sql STABLE security definer;

GRANT ALL ON FUNCTION event_trainer_lessons_remaining(e event_trainer) TO anonymous;
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
    'trainer_person_id', trainer.person_id,
    'lesson_count', demand.lesson_count,
    'created_at', demand.created_at
  )), '[]'::jsonb)
  INTO v_old_demands
  FROM public.event_lesson_demand demand
  JOIN public.event_instance_registration eir ON eir.id = demand.registration_id
  JOIN public.event_registration er ON er.id = eir.legacy_registration_id
  JOIN public.event_trainer trainer ON trainer.id = demand.trainer_id
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

  -- Re-point the instance (event_instance_trainer + any other (tenant_id, instance_id, event_id) dependents follow via FK cascade)
  UPDATE public.event_instance ei
  SET event_id   = v_new_event_id
  WHERE ei.id        = p_instance_id
    AND ei.event_id  = v_old_event_id;

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
  SELECT new_tr.id, new_eir.id, old.lesson_count, old.created_at, v_new_event_id
  FROM jsonb_to_recordset(v_old_demands) AS old(
    reg_person_id bigint,
    reg_couple_id bigint,
    trainer_person_id bigint,
    lesson_count integer,
    created_at timestamptz
  )
  JOIN public.event_trainer new_tr
    ON new_tr.event_id = v_new_event_id
   AND new_tr.person_id = old.trainer_person_id
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
