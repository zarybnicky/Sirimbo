--! Previous: sha1:ff9b9c33fe9be804f56e3c1383b6d8cd5a7df8f8
--! Hash: sha1:975be5b05d8bf602949c1570d4a6a4f17d2e86e5

--! split: 1-current.sql
do $$
begin
  if not exists (
    select 1
    from pg_catalog.pg_type t
    join pg_catalog.pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public'
      and t.typname = 'event_instance_registration_status'
  ) then
    create type public.event_instance_registration_status as enum ('active', 'cancelled');
  end if;
end;
$$;

alter table public.event_instance_registration
  add column if not exists attendance_note text,
  add column if not exists registration_status public.event_instance_registration_status default 'active';

update public.event_instance_registration
set registration_status = 'active'
where registration_status is null;

alter table public.event_instance_registration
  alter column registration_status set default 'active',
  alter column registration_status set not null;

create or replace function app_private.tg_eir__attendance_timestamps()
  returns trigger
  language plpgsql
  set search_path = pg_catalog, pg_temp
as $$
begin
  if new.person_id is null then
    new.attendance_created_at := null;
    new.attendance_updated_at := null;
  elsif tg_op = 'INSERT' or new.person_id is distinct from old.person_id then
    new.attendance_created_at := new.created_at;
    new.attendance_updated_at := new.updated_at;
  else
    new.attendance_created_at := old.attendance_created_at;
    new.attendance_updated_at := case
      when new.status is not distinct from old.status
        and new.attendance_note is not distinct from old.attendance_note
        then old.attendance_updated_at
      when old.attendance_updated_at >= new.updated_at
        then old.attendance_updated_at + interval '1 millisecond'
      else new.updated_at
    end;
  end if;
  return new;
end;
$$;
select verify_function('app_private.tg_eir__attendance_timestamps', 'public.event_instance_registration');
revoke execute on function app_private.tg_eir__attendance_timestamps() from public, anonymous;

drop trigger if exists _200_eir_attendance_timestamps on public.event_instance_registration;
create trigger _200_eir_attendance_timestamps
  before insert or update of person_id, status, attendance_note
  on public.event_instance_registration
  for each row execute function app_private.tg_eir__attendance_timestamps();

--! Included functions/event_instance_stats.sql
create or replace function app_private.refresh_event_instance_stats(p_instance_id bigint)
  returns void
  language sql volatile security definer
  set search_path = pg_catalog, pg_temp
as $$
  select 1 from public.event_instance where id = p_instance_id for no key update;

  update public.event_instance instance
  set stats = actual.stats
  from (
    select jsonb_build_object(
      'TOTAL', count(*) filter (where status <> 'cancelled')::int,
      'UNKNOWN', count(*) filter (where status = 'unknown')::int,
      'ATTENDED', count(*) filter (where status = 'attended')::int,
      'NOT_EXCUSED', count(*) filter (where status = 'not-excused')::int
    ) as stats
    from public.event_instance_registration
    where instance_id = p_instance_id
      and person_id is not null
      and registration_status = 'active'
  ) actual
  where instance.id = p_instance_id and instance.stats is distinct from actual.stats;
$$;
revoke execute on function app_private.refresh_event_instance_stats(bigint)
  from public, anonymous;

create or replace function app_private.tg_eir__refresh_stats_stmt()
  returns trigger
  language plpgsql security definer
  set search_path = pg_catalog, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
begin
  if tg_op = 'INSERT' then
    perform app_private.refresh_event_instance_stats(affected.instance_id)
    from (
      select distinct instance_id from changed_rows
      where person_id is not null order by instance_id
    ) affected;
  elsif tg_op = 'DELETE' then
    perform app_private.refresh_event_instance_stats(affected.instance_id)
    from (
      select distinct instance_id from deleted_rows
      where person_id is not null order by instance_id
    ) affected;
  else
    perform app_private.refresh_event_instance_stats(affected.instance_id)
    from (
      select distinct affected.instance_id
      from deleted_rows old_row
      join changed_rows new_row using (id)
      cross join lateral (values
        (old_row.instance_id, old_row.person_id),
        (new_row.instance_id, new_row.person_id)
      ) affected(instance_id, person_id)
      where row(old_row.instance_id, old_row.person_id, old_row.status, old_row.registration_status)
        is distinct from
        row(new_row.instance_id, new_row.person_id, new_row.status, new_row.registration_status)
        and affected.person_id is not null
      order by affected.instance_id
    ) affected;
  end if;
  return null;
end;
$$;
select verify_function(
  'app_private.tg_eir__refresh_stats_stmt',
  'public.event_instance_registration'
);
revoke execute on function app_private.tg_eir__refresh_stats_stmt()
  from public, anonymous;
--! EndIncluded functions/event_instance_stats.sql

create or replace view public.event_attendance with (security_invoker = true) as
select
  id,
  tenant_id,
  instance_id,
  person_id,
  status,
  attendance_note as note,
  legacy_registration_id as registration_id,
  event_id,
  attendance_created_at as created_at,
  attendance_updated_at as updated_at
from public.event_instance_registration
where person_id is not null;

drop function if exists update_attendance;
drop function if exists detach_event_instance(bigint, bigint, text);
drop function if exists detach_event_instance(bigint, text);
--! Included functions/update_attendance.sql
create or replace function update_attendance(eir_id bigint, status attendance_type, note text)
  returns event_instance_registration
  language sql as $$
  update event_instance_registration eir
  set status = $2, attendance_note = $3
  where eir.id = $1 and eir.person_id is not null and eir.registration_status = 'active'
  returning eir.*;
$$;
--! EndIncluded functions/update_attendance.sql
--! Included functions/update_event_attendance.sql
CREATE or replace FUNCTION update_event_attendance(instance_id bigint, person_id bigint, status attendance_type, note text) RETURNS event_attendance
    LANGUAGE sql
    AS $$
  update event_instance_registration eir
  set status = $3, attendance_note = $4
  where eir.id = (
    select candidate.id
    from event_instance_registration candidate
    where candidate.instance_id = $1
      and candidate.person_id = $2
      and candidate.legacy_registration_id is not null
    order by candidate.id
    limit 1
  )
  returning eir.id, eir.tenant_id, eir.instance_id, eir.person_id, eir.status,
    eir.attendance_note, eir.legacy_registration_id, eir.event_id,
    eir.attendance_created_at, eir.attendance_updated_at;
$$;

GRANT ALL ON FUNCTION update_event_attendance(instance_id bigint, person_id bigint, status attendance_type, note text) TO anonymous;
--! EndIncluded functions/update_event_attendance.sql
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

  -- Copy lesson demand:
  -- - trainer maps by person_id (old event_trainer -> new event_trainer)
  -- - registration maps by (person_id, couple_id) to new event_registration
  INSERT INTO public.event_lesson_demand (
    trainer_id, registration_id, lesson_count, created_at, event_id
  )
  SELECT new_tr.id, new_reg.id, d.lesson_count, d.created_at, v_new_event_id
  FROM public.event_lesson_demand d
  JOIN public.event_trainer old_tr ON old_tr.id = d.trainer_id AND old_tr.event_id  = v_old_event_id
  JOIN public.event_trainer new_tr ON new_tr.event_id  = v_new_event_id AND new_tr.person_id = old_tr.person_id
  JOIN public.event_registration old_reg ON old_reg.id = d.registration_id AND old_reg.event_id  = v_old_event_id
  JOIN public.event_registration new_reg ON new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM old_reg.person_id AND new_reg.couple_id IS NOT DISTINCT FROM old_reg.couple_id
  WHERE d.event_id = v_old_event_id
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

alter table public.event_instance
  drop constraint if exists event_instance_parent_id_fkey,
  add constraint event_instance_parent_id_fkey
    foreign key (parent_id) references public.event_instance (id)
    on update cascade on delete no action;

--! Included functions/tg_event_instance__validate_parent.sql
create or replace function app_private.tg_event_instance__validate_parent()
  returns trigger
  language plpgsql
  security definer
  set search_path = pg_catalog, pg_temp
as $$
begin
  if tg_op = 'UPDATE'
    and new.tenant_id is distinct from old.tenant_id
    and exists (
      select 1 from public.event_instance child where child.parent_id = new.id
    ) then
    raise exception 'An event instance with children cannot change tenant'
      using errcode = '23514';
  end if;

  if new.parent_id is null then return new; end if;

  perform 1
  from public.event_instance parent
  where parent.id = new.parent_id
    and parent.tenant_id = new.tenant_id
  for no key update;

  if not found or exists (
    with recursive ancestors as (
      select id, parent_id
      from public.event_instance where id = new.parent_id
      union
      select parent.id, parent.parent_id
      from public.event_instance parent
      join ancestors child on parent.id = child.parent_id
    )
    select 1 from ancestors where id = new.id
  ) then
    raise exception 'Event instance parent must be in the same tenant and not create a cycle'
      using errcode = '23514';
  end if;

  return new;
end;
$$;
select verify_function('app_private.tg_event_instance__validate_parent', 'public.event_instance');
revoke execute on function app_private.tg_event_instance__validate_parent()
  from public, anonymous;

drop trigger if exists _200_validate_parent on public.event_instance;
create trigger _200_validate_parent
  before insert or update of tenant_id, parent_id on public.event_instance
  for each row execute function app_private.tg_event_instance__validate_parent();
--! EndIncluded functions/tg_event_instance__validate_parent.sql
--! Included functions/tg_event_instance__refresh_manager_person_ids.sql
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
--! EndIncluded functions/tg_event_instance__refresh_manager_person_ids.sql
select app_private.refresh_event_instance_manager_person_ids(id, event_id)
from public.event_instance
where parent_id is null;

--! Included policies/event_instance.sql
select app_private.drop_policies('public.event_instance');

CREATE POLICY current_tenant ON public.event_instance AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY admin_same_tenant ON public.event_instance TO administrator USING (true);
CREATE POLICY trainer_select ON public.event_instance FOR SELECT TO trainer
  USING (app_private.can_trainer_edit_instance(id));
CREATE POLICY trainer_insert ON public.event_instance FOR INSERT TO trainer
  WITH CHECK (parent_id is null or app_private.can_trainer_edit_instance(parent_id));
CREATE POLICY trainer_update ON public.event_instance FOR UPDATE TO trainer
  USING (app_private.can_trainer_edit_instance(id))
  WITH CHECK (
    app_private.can_trainer_edit_instance(id)
    or (parent_id is not null and app_private.can_trainer_edit_instance(parent_id))
  );
CREATE POLICY trainer_delete ON public.event_instance FOR DELETE TO trainer
  USING (app_private.can_trainer_edit_instance(id));
CREATE POLICY member_view ON public.event_instance FOR SELECT TO member USING (is_visible);
CREATE POLICY public_view ON public.event_instance FOR SELECT TO anonymous USING (is_public);
--! EndIncluded policies/event_instance.sql

update public.event_instance set capacity_unit = 'people'
where (
  select column_default like '%registrations%'
  from information_schema.columns
  where table_schema = 'public'
    and table_name = 'event_instance'
    and column_name = 'capacity_unit'
);
alter table public.event_instance
  alter column capacity_unit set default 'people',
  alter column capacity_unit set not null;

create or replace function public.event_instance_remaining_person_spots(
  inst public.event_instance
) returns integer
  language sql
  stable
  security definer
  set search_path = pg_catalog, pg_temp
as $$
  select case
    when inst.capacity is null or inst.capacity <= 0 then null
    else inst.capacity - case inst.capacity_unit
        when 'people' then (
          select count(*)::integer
          from public.event_instance_registration person_row
          where person_row.instance_id = inst.id
            and person_row.person_id is not null
            and person_row.registration_status = 'active'
            and person_row.status <> 'cancelled'
        )
        when 'registrations' then (
          select count(*)::integer
          from public.event_instance_registration root
          where root.instance_id = inst.id
            and root.parent_registration_id is null
            and root.registration_status = 'active'
        )
      end - (
      select count(*)::integer
      from public.event_external_registration external_registration
      where external_registration.event_id = inst.event_id
    )
  end;
$$;
grant execute on function public.event_instance_remaining_person_spots(public.event_instance)
  to anonymous;

-- Native schedule lessons are authored without a legacy event shell.
--! Included functions/quick_create_event_instances.sql
create or replace function quick_create_event_instances(
  events quick_event_input[],
  parent_id bigint default null
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
      'people', true, false, false, false, '', ''
    )
    returning * into created_instance;

    with roots as (
      insert into event_instance_registration (
        instance_id, person_id, couple_id, status
      )
      select created_instance.id, registration.person_id, registration.couple_id,
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
--! Included functions/move_event_instance.sql
create or replace function public.move_event_instance(
  id bigint,
  since timestamptz,
  until timestamptz,
  trainer_person_id bigint default null,
  location_id bigint default null,
  location_text text default null
) returns public.event_instance
  language plpgsql
as $$
declare
  inst public.event_instance;
begin
  update public.event_instance
  set since = move_event_instance.since,
      until = move_event_instance.until,
      location_id = case
        when move_event_instance.location_id is not null then move_event_instance.location_id
        when move_event_instance.location_text is not null then null
        else event_instance.location_id
      end,
      location_text = case
        when move_event_instance.location_id is not null then ''
        else coalesce(move_event_instance.location_text, event_instance.location_text)
      end
  where event_instance.id = move_event_instance.id
  returning * into inst;

  if not found then
    raise exception 'Event instance % not found', move_event_instance.id;
  end if;

  if trainer_person_id is not null then
    update public.event_instance_trainer
    set person_id = trainer_person_id
    where instance_id = inst.id
      and 1 = (select count(*) from public.event_instance_trainer where instance_id = inst.id);

    if not found
      and not exists (select 1 from public.event_instance_trainer where instance_id = inst.id)
      and (inst.event_id is null or (select count(*) from public.event_trainer where event_id = inst.event_id) <= 1)
    then
      insert into public.event_instance_trainer (instance_id, person_id)
      values (inst.id, trainer_person_id);
    end if;
  end if;

  return (select instance from public.event_instance instance where instance.id = inst.id);
end;
$$;

select verify_function('public.move_event_instance');
grant execute on function public.move_event_instance(
  bigint, timestamptz, timestamptz, bigint, bigint, text
) to anonymous;
--! EndIncluded functions/move_event_instance.sql
--! Included functions/event_instances_for_range.sql
CREATE or replace FUNCTION public.event_instances_for_range(
  only_type public.event_type,
  start_range timestamp with time zone,
  end_range timestamp with time zone DEFAULT NULL::timestamp with time zone,
  trainer_ids bigint[] = null,
  participant_ids bigint[] = null,
  only_mine boolean = false,
  parent_id bigint = null
) RETURNS SETOF event_instance as $$
  select i.*
  from event_instance i
  where i.tenant_id = current_tenant_id()
    and (only_type IS NULL OR i.type = only_type)
    and i.parent_id is not distinct from $7
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_trainer where event_id = i.event_id and person_id = any (trainer_ids))
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any (participant_ids) and registration_status = 'active' and status <> 'cancelled'))
    and (only_mine is false
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any ((select current_person_ids())::bigint[]) and registration_status = 'active' and status <> 'cancelled')
      or i.manager_person_ids && ((select current_person_ids())::bigint[]));
$$ stable language sql;

COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;
--! EndIncluded functions/event_instances_for_range.sql
--! Included functions/event_overlaps_reports.sql
create or replace function public.event_overlaps_attendee_report(
  p_since timestamptz,
  p_until timestamptz
)
returns setof public.event_overlaps_conflict
language sql
stable
as $$
  with target_range as (
    select tstzrange(
      coalesce(p_since, '-infinity'::timestamptz),
      coalesce(p_until, 'infinity'::timestamptz),
      '[]'
    ) as range
  ),
  instances as (
    select
      ea.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      ei.event_id,
      ei.name as event_name
    from public.event_instance_registration ea
    join public.event_instance ei on ei.id = ea.instance_id
    join public.person p on p.id = ea.person_id
    join target_range tr on true
    where
      ei.tenant_id = public.current_tenant_id()
      and ea.person_id is not null
      and ea.registration_status = 'active'
      and not ei.is_cancelled
      and ea.status <> 'cancelled'
      and ei.range && tr.range
  )
  select
    i1.person_id,
    i1.person_name,
    i1.instance_id as first_instance_id,
    i1.event_id as first_event_id,
    i1.event_name as first_event_name,
    i1.since as first_since,
    i1.until as first_until,
    i2.instance_id as second_instance_id,
    i2.event_id as second_event_id,
    i2.event_name as second_event_name,
    i2.since as second_since,
    i2.until as second_until,
    tstzrange(
      greatest(i1.since, i2.since),
      least(i1.until, i2.until),
      '[]'
    ) as overlap_range
  from instances i1
  join instances i2 on i1.person_id = i2.person_id
    and i1.instance_id < i2.instance_id
    and i1.range && i2.range
    and greatest(i1.since, i2.since) < least(i1.until, i2.until);
$$;

comment on function public.event_overlaps_attendee_report is '@simpleCollections only';
grant all on function public.event_overlaps_attendee_report to anonymous;

create or replace function public.event_overlaps_trainer_report(
  p_since timestamptz,
  p_until timestamptz
)
returns setof public.event_overlaps_conflict
language sql
stable
as $$
  with target_range as (
    select tstzrange(
      coalesce(p_since, '-infinity'::timestamptz),
      coalesce(p_until, 'infinity'::timestamptz),
      '[]'
    ) as range
  ),
  trainer_instances as (
    select
      trainer.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      ei.event_id,
      ei.name as event_name
    from public.event_instance ei
    cross join lateral app_private.event_instance_trainers_at(ei, ei.since) trainer
    join public.person p on p.id = trainer.person_id
    join target_range tr on true
    where
      ei.tenant_id = public.current_tenant_id()
      and not ei.is_cancelled
      and ei.range && tr.range
  )
  select
    ti1.person_id,
    ti1.person_name,
    ti1.instance_id as first_instance_id,
    ti1.event_id as first_event_id,
    ti1.event_name as first_event_name,
    ti1.since as first_since,
    ti1.until as first_until,
    ti2.instance_id as second_instance_id,
    ti2.event_id as second_event_id,
    ti2.event_name as second_event_name,
    ti2.since as second_since,
    ti2.until as second_until,
    tstzrange(
      greatest(ti1.since, ti2.since),
      least(ti1.until, ti2.until),
      '[]'
    ) as overlap_range
  from trainer_instances ti1
  join trainer_instances ti2 on ti1.person_id = ti2.person_id
    and ti1.instance_id < ti2.instance_id
    and ti1.range && ti2.range
    and greatest(ti1.since, ti2.since) < least(ti1.until, ti2.until);
$$;

comment on function public.event_overlaps_trainer_report is '@simpleCollections only';
grant all on function public.event_overlaps_trainer_report to anonymous;
--! EndIncluded functions/event_overlaps_reports.sql
--! Included functions/scoreboard.sql
drop view if exists scoreboard;
drop function if exists scoreboard_entries;

create or replace function scoreboard_entries(
  since date,
  until date,
  cohort_id bigint default null
)
returns setof scoreboard_record
language sql
stable
as $$
  with membership as (
    select cm.person_id, cm.cohort_id
    from current_cohort_membership cm
    where (cm.cohort_id = scoreboard_entries.cohort_id or scoreboard_entries.cohort_id is null)
  ),
  instances as (
    select
      i.id as instance_id,
      i.since,
      i.until,
      i.type as event_type
    from event_instance i
    where not i.is_cancelled
      and i.since >= scoreboard_entries.since::timestamptz
      and i.until  < scoreboard_entries.until::timestamptz
      and i.type <> 'reservation'
  ),
  member_people as (
    select distinct person_id
    from membership
  ),
  attendance as materialized (
    select
      ea.person_id,
      scoreboard_entries.cohort_id as cohort_id,
      case when inst.event_type = 'lesson' then 1 else 0 end as lesson_score,
      case when inst.event_type = 'group' then floor((extract(epoch from (inst.until - inst.since)) / 60)::numeric / 45::numeric) else 0 end as group_score,
      case when inst.event_type = 'camp'  then 3 + 2 * ((extract(epoch from (inst.until - inst.since)) > 86400)::int) else 0 end as event_score,
      date_trunc('day', inst.since)::date as day
    from instances inst
    join event_instance_registration ea
      on ea.instance_id = inst.instance_id and ea.person_id is not null
    left join event_instance_registration registration
      on registration.id = ea.parent_registration_id
    left join event_target_cohort tc
      on tc.id = coalesce(ea.target_cohort_id, registration.target_cohort_id)
    where
      ea.registration_status = 'active'
      and ea.status <> 'cancelled'
      and (ea.status = 'attended' or inst.event_type = 'lesson')
      and ea.person_id = any (select person_id from member_people)
      and (scoreboard_entries.cohort_id is null or tc.cohort_id = scoreboard_entries.cohort_id)
  ),
  per_day as (
    select
      person_id,
      cohort_id,
      least(sum(lesson_score), 4) as lesson_score,
      sum(group_score) as group_score,
      sum(event_score) as event_score
    from attendance
    group by person_id, cohort_id, day
  ),
  aggregated as (
    select
      person_id,
      cohort_id,
      coalesce(sum(lesson_score), 0)::bigint as lesson_total_score,
      coalesce(sum(group_score), 0)::bigint as group_total_score,
      coalesce(sum(event_score), 0)::bigint as event_total_score
    from per_day
    group by person_id, cohort_id
  ),
  manual as (
    select
      sma.person_id,
      case
        when scoreboard_entries.cohort_id is null then null
        else coalesce(sma.cohort_id, scoreboard_entries.cohort_id)
      end as cohort_id,
      sum(sma.points)::bigint as manual_total_score
    from scoreboard_manual_adjustment sma
    where sma.tenant_id = current_tenant_id()
      and sma.awarded_at >= scoreboard_entries.since
      and sma.awarded_at < scoreboard_entries.until
      and (
        scoreboard_entries.cohort_id is null or
        sma.cohort_id is null or
        sma.cohort_id = scoreboard_entries.cohort_id
      )
      and exists (
        select 1
        from membership mem
        where mem.person_id = sma.person_id
          and (
            (scoreboard_entries.cohort_id is null and sma.cohort_id is null)
            or mem.cohort_id is not distinct from coalesce(sma.cohort_id, scoreboard_entries.cohort_id)
          )
      )
    group by 1, 2
  ),
  totals as (
    select
      coalesce(a.person_id, m.person_id) as person_id,
      coalesce(a.cohort_id, m.cohort_id) as cohort_id,
      coalesce(a.lesson_total_score, 0)::bigint as lesson_total_score,
      coalesce(a.group_total_score, 0)::bigint as group_total_score,
      coalesce(a.event_total_score, 0)::bigint as event_total_score,
      coalesce(m.manual_total_score, 0)::bigint as manual_total_score,
      (
        coalesce(a.lesson_total_score, 0)::bigint +
        coalesce(a.group_total_score, 0)::bigint +
        coalesce(a.event_total_score, 0)::bigint +
        coalesce(m.manual_total_score, 0)::bigint
      ) as total_score
    from aggregated a
    full join manual m using (person_id, cohort_id)
  )
  select
    person_id,
    cohort_id,
    lesson_total_score,
    group_total_score,
    event_total_score,
    manual_total_score,
    total_score,
    rank() over (order by total_score desc, person_id) as ranking
  from totals
  order by total_score desc, person_id;
$$;

comment on function scoreboard_entries is '@simpleCollections only';

grant all on function scoreboard_entries to anonymous;
--! EndIncluded functions/scoreboard.sql
--! Included functions/trainer_group_attendance_completion.sql
create or replace function public.trainer_group_attendance_completion(
  since timestamp with time zone default null,
  until timestamp with time zone default null
) returns setof public.trainer_group_attendance_completion
  language sql stable as $_$
  with filtered_instances as (
    select ei.id
    from event_instance ei
    where ei.tenant_id = current_tenant_id()
      and not ei.is_cancelled
      and ei.type = 'group'
      and coalesce(ei.until, ei.since) < coalesce($2, now())
      and ($1 is null or coalesce(ei.since, ei.until) >= $1)
      and ($2 is null or coalesce(ei.until, ei.since) < $2)
  ),
  trainer_instances as (
    select effective_trainer.person_id, fi.id as instance_id
    from filtered_instances fi
    join event_instance instance on instance.id = fi.id
    cross join lateral app_private.event_instance_trainers_at(
      instance,
      instance.since
    ) effective_trainer
  ),
  attendance_stats as (
    select
      ti.person_id,
      ti.instance_id,
      coalesce(stats.attendance_count, 0) as attendance_count,
      coalesce(stats.unknown_count, 0) as unknown_count
    from trainer_instances ti
    left join lateral (
      select
        count(*) as attendance_count,
        count(*) filter (where eir.status = 'unknown') as unknown_count
      from event_instance_registration eir
      where eir.instance_id = ti.instance_id
        and eir.person_id is not null
        and eir.registration_status = 'active'
        and eir.status <> 'cancelled'
    ) stats on true
  ),
  per_trainer as (
    select
      person_id,
      count(*) as total_instances,
      count(*) filter (where attendance_count > 0 and unknown_count = 0) as filled_instances,
      count(*) filter (
        where attendance_count > 0
          and unknown_count > 0
          and unknown_count < attendance_count
      ) as partially_filled_instances,
      count(*) filter (where attendance_count = 0 or unknown_count = attendance_count) as unfilled_instances,
      coalesce(sum(attendance_count), 0) as total_attendances,
      coalesce(sum(unknown_count), 0) as pending_attendances
    from attendance_stats
    group by person_id
  )
  select
    person_id,
    total_instances,
    filled_instances,
    partially_filled_instances,
    unfilled_instances,
    case
      when total_instances > 0 then (filled_instances + partially_filled_instances)::double precision / total_instances
      else null
    end as filled_ratio,
    total_attendances,
    pending_attendances
  from per_trainer
  order by filled_ratio asc nulls last, person_id;
$_$;

comment on function public.trainer_group_attendance_completion(timestamp with time zone, timestamp with time zone) is '@simpleCollections only';
grant all on function public.trainer_group_attendance_completion(timestamp with time zone, timestamp with time zone) to anonymous;
--! EndIncluded functions/trainer_group_attendance_completion.sql
