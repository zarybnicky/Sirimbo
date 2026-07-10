--! Previous: sha1:5878f729d46c72506f664b9537640d3c52793ce6
--! Hash: sha1:ff9b9c33fe9be804f56e3c1383b6d8cd5a7df8f8

--! split: 1-current.sql
alter table public.event_instance_registration
  add column if not exists attendance_created_at timestamptz,
  add column if not exists attendance_updated_at timestamptz;

-- Every legacy attendance must have a bridge person row before its source
-- table is removed; otherwise its audit timestamps cannot be preserved.
do $$
begin
  if exists (
    select 1
    from pg_catalog.pg_class c
    join pg_catalog.pg_namespace n on n.oid = c.relnamespace
    where n.nspname = 'public' and c.relname = 'event_attendance' and c.relkind = 'r'
  ) then
    if exists (
      select 1
      from public.event_attendance ea
      where not exists (
        select 1
        from public.event_instance_registration eir
        where eir.tenant_id = ea.tenant_id
          and eir.legacy_registration_id = ea.registration_id
          and eir.instance_id = ea.instance_id
          and eir.person_id = ea.person_id
      )
    ) or exists (
      select 1
      from public.event_instance_registration eir
      where eir.person_id is not null
        and eir.legacy_registration_id is not null
        and not exists (
          select 1
          from public.event_attendance ea
          where ea.tenant_id = eir.tenant_id
            and ea.registration_id = eir.legacy_registration_id
            and ea.instance_id = eir.instance_id
            and ea.person_id = eir.person_id
        )
    ) then
      raise exception 'Cannot preserve event attendance audit timestamps: legacy attendance and EIR bridge rows do not match';
    end if;
  end if;
end;
$$;

-- The generic EIR timestamps also change when registration metadata changes.
-- Preserve the independent attendance lifecycle before replacing the table.
alter table public.event_instance_registration disable trigger _100_timestamps;

update public.event_instance_registration eir
set attendance_created_at = coalesce(eir.attendance_created_at, ea.created_at),
    attendance_updated_at = coalesce(eir.attendance_updated_at, ea.updated_at)
from public.event_attendance ea
where eir.tenant_id = ea.tenant_id
  and eir.legacy_registration_id = ea.registration_id
  and eir.instance_id = ea.instance_id
  and eir.person_id = ea.person_id
  and (eir.attendance_created_at is null or eir.attendance_updated_at is null);

update public.event_instance_registration
set attendance_created_at = coalesce(attendance_created_at, created_at),
    attendance_updated_at = coalesce(attendance_updated_at, updated_at)
where person_id is not null
  and (attendance_created_at is null or attendance_updated_at is null);

alter table public.event_instance_registration enable trigger _100_timestamps;

create or replace function app_private.tg_eir__attendance_timestamps()
  returns trigger
  language plpgsql
  set search_path = pg_catalog, pg_temp
as $$
begin
  if new.person_id is null then
    new.attendance_created_at := null;
    new.attendance_updated_at := null;
  elsif tg_op = 'INSERT' then
    new.attendance_created_at := new.created_at;
    new.attendance_updated_at := new.updated_at;
  elsif old.person_id is null or new.person_id is distinct from old.person_id then
    new.attendance_created_at := new.created_at;
    new.attendance_updated_at := new.updated_at;
  else
    new.attendance_created_at := old.attendance_created_at;
    if new.status is distinct from old.status or new.note is distinct from old.note then
      new.attendance_updated_at := case
        when old.attendance_updated_at >= new.updated_at
          then old.attendance_updated_at + interval '1 millisecond'
        else new.updated_at
      end;
    else
      new.attendance_updated_at := old.attendance_updated_at;
    end if;
  end if;
  return new;
end;
$$;
select verify_function('app_private.tg_eir__attendance_timestamps', 'public.event_instance_registration');
revoke execute on function app_private.tg_eir__attendance_timestamps() from public, anonymous;

drop trigger if exists _200_eir_attendance_timestamps on public.event_instance_registration;
create trigger _200_eir_attendance_timestamps
  before insert or update of person_id, status, note
  on public.event_instance_registration
  for each row execute function app_private.tg_eir__attendance_timestamps();

do $$
begin
  if not exists (
    select 1 from pg_catalog.pg_constraint
    where conname = 'event_instance_registration_attendance_state'
      and conrelid = 'public.event_instance_registration'::regclass
  ) then
    alter table public.event_instance_registration
      add constraint event_instance_registration_attendance_state check (
        person_id is null or (
          status is not null
          and attendance_created_at is not null
          and attendance_updated_at is not null
        )
      );
  end if;
end;
$$;

create or replace function app_private.refresh_event_instance_stats(p_instance_id bigint)
  returns void
  language plpgsql volatile security definer
  set search_path = pg_catalog, pg_temp
as $$
declare
  v_stats jsonb;
begin
  perform 1 from public.event_instance ei where ei.id = p_instance_id for no key update;
  if not found then
    return;
  end if;

  select jsonb_build_object(
    'TOTAL', count(*) filter (where eir.status <> 'cancelled')::int,
    'UNKNOWN', count(*) filter (where eir.status = 'unknown')::int,
    'ATTENDED', count(*) filter (where eir.status = 'attended')::int,
    'NOT_EXCUSED', count(*) filter (where eir.status = 'not-excused')::int
  )
  into v_stats
  from public.event_instance_registration eir
  where eir.instance_id = p_instance_id
    and eir.person_id is not null;

  update public.event_instance ei
  set stats = v_stats
  where ei.id = p_instance_id
    and ei.stats is distinct from v_stats;
end;
$$;
select verify_function('app_private.refresh_event_instance_stats');
revoke execute on function app_private.refresh_event_instance_stats(bigint) from public, anonymous;

create or replace function app_private.tg_eir__refresh_stats_stmt()
  returns trigger
  language plpgsql security definer
  set search_path = pg_catalog, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
declare
  v_instance_id bigint;
begin
  if tg_op = 'INSERT' then
    for v_instance_id in
      select distinct r.instance_id
      from changed_rows r
      where r.person_id is not null
      order by r.instance_id
    loop
      perform app_private.refresh_event_instance_stats(v_instance_id);
    end loop;
  elsif tg_op = 'DELETE' then
    for v_instance_id in
      select distinct r.instance_id
      from deleted_rows r
      where r.person_id is not null
      order by r.instance_id
    loop
      perform app_private.refresh_event_instance_stats(v_instance_id);
    end loop;
  else
    for v_instance_id in
      with relevant_changes as (
        select
          o.instance_id as old_instance_id,
          o.person_id as old_person_id,
          n.instance_id as new_instance_id,
          n.person_id as new_person_id
        from deleted_rows o
        join changed_rows n using (id)
        where row(o.instance_id, o.person_id, o.status)
          is distinct from row(n.instance_id, n.person_id, n.status)
      )
      select distinct affected.instance_id
      from relevant_changes changed
      cross join lateral (
        values
          (changed.old_instance_id, changed.old_person_id),
          (changed.new_instance_id, changed.new_person_id)
      ) affected(instance_id, person_id)
      where affected.person_id is not null
      order by affected.instance_id
    loop
      perform app_private.refresh_event_instance_stats(v_instance_id);
    end loop;
  end if;
  return null;
end;
$$;

drop trigger if exists _500_eir_refresh_stats_ins on public.event_instance_registration;
drop trigger if exists _500_eir_refresh_stats_del on public.event_instance_registration;
drop trigger if exists _500_eir_refresh_stats_upd on public.event_instance_registration;

create trigger _500_eir_refresh_stats_ins
  after insert on public.event_instance_registration
  referencing new table as changed_rows for each statement
  execute function app_private.tg_eir__refresh_stats_stmt();
create trigger _500_eir_refresh_stats_del
  after delete on public.event_instance_registration
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_eir__refresh_stats_stmt();
create trigger _500_eir_refresh_stats_upd
  after update on public.event_instance_registration
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_eir__refresh_stats_stmt();

drop function if exists app_private.tg_eir__refresh_stats_row();
select verify_function('app_private.tg_eir__refresh_stats_stmt', 'public.event_instance_registration');
revoke execute on function app_private.tg_eir__refresh_stats_stmt() from public, anonymous;

with expected as (
  select
    ei.id,
    jsonb_build_object(
      'TOTAL', count(*) filter (where eir.status <> 'cancelled')::int,
      'UNKNOWN', count(*) filter (where eir.status = 'unknown')::int,
      'ATTENDED', count(*) filter (where eir.status = 'attended')::int,
      'NOT_EXCUSED', count(*) filter (where eir.status = 'not-excused')::int
    ) as stats
  from public.event_instance ei
  left join public.event_instance_registration eir
    on eir.instance_id = ei.id and eir.person_id is not null
  group by ei.id
)
update public.event_instance ei
set stats = expected.stats
from expected
where ei.id = expected.id
  and ei.stats is distinct from expected.stats;

drop policy if exists trainer_same_tenant on public.event_instance_registration;
drop policy if exists trainer_insert on public.event_instance_registration;
drop policy if exists trainer_update on public.event_instance_registration;

create policy trainer_insert on public.event_instance_registration
  for insert to trainer
  with check (app_private.can_trainer_edit_instance(instance_id));
create policy trainer_update on public.event_instance_registration
  for update to trainer
  using (app_private.can_trainer_edit_instance(instance_id))
  with check (app_private.can_trainer_edit_instance(instance_id));

drop trigger if exists _500_create_attendance on event_registration;
drop trigger if exists _500_create_attendance on event_instance;
drop function if exists app_private.tg_event_registration__create_attendance();
drop function if exists app_private.tg_event_instance__create_attendance();

drop function if exists public.update_event_attendance(bigint, bigint, attendance_type, text);
drop function if exists public.person_recent_attendance;
drop function if exists public.person_weekly_attendance;
drop function if exists public.event_instance_attendance_summary;

do $$
begin
  if exists (
    select 1
    from pg_catalog.pg_class c
    join pg_catalog.pg_namespace n on n.oid = c.relnamespace
    where n.nspname = 'public' and c.relname = 'event_attendance' and c.relkind = 'r'
  ) then
    drop table public.event_attendance;
  end if;
end;
$$;

drop function if exists app_private.tg_event_attendance__refresh_stats();
drop function if exists app_private.tg_event_attendance__propagate_status();

create or replace view event_attendance with (security_invoker = true) as
  select
    id, tenant_id, instance_id, person_id, status, note,
    legacy_registration_id as registration_id, event_id,
    attendance_created_at as created_at,
    attendance_updated_at as updated_at
  from event_instance_registration
  where person_id is not null;

comment on view event_attendance is $$
@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only
@primaryKey id
@foreignKey (tenant_id) references tenant (id)
@foreignKey (instance_id) references event_instance (id)|@fieldName instance|@foreignFieldName eventAttendancesByInstanceId
@foreignKey (registration_id) references event_registration (id)|@fieldName registration|@foreignFieldName eventAttendancesByRegistrationId|@behavior -delete
@foreignKey (person_id) references person (id)
$$;
comment on column event_attendance.event_id is '@omit';
comment on column event_attendance.tenant_id is '@notNull';
comment on column event_attendance.instance_id is '@notNull';
comment on column event_attendance.person_id is '@notNull';
comment on column event_attendance.status is '@notNull';
comment on column event_attendance.created_at is '@notNull';
comment on column event_attendance.updated_at is '@notNull';
grant select on event_attendance to anonymous;

comment on table event_instance_registration is E'@omit create,update,delete';
comment on column event_instance_registration.legacy_registration_id is E'@omit';

comment on view activity_timeline_item is $$
@primaryKey id
@interface mode:single type:kind
@type EVENT_ATTENDANCE name:ActivityEventAttendance attributes:event_attendance_id,event_instance_id
@type COMPETITION_BRIEF name:ActivityCompetitionBrief attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,check_in_end,category,dances,participants,competition_type,competition_event_external_id,competition_external_id
@type COMPETITION_RESULT name:ActivityCompetitionResult attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,dances,participants,ranking,ranking_to,point_gain,is_final,competition_type,competition_event_external_id,competition_external_id
@type JUDGING name:ActivityJudging attributes:federation,federated_person_id,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,competition_type,competition_event_external_id,competition_external_id
@type BIRTHDAY name:ActivityBirthday
@foreignKey (person_id) references person (id)|@fieldName person|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_attendance_id) references event_attendance (id)|@fieldName eventAttendance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_attendance_id) references event_instance_registration (id)|@fieldName eventInstanceRegistration|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_instance_id) references event_instance (id)|@fieldName eventInstance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@behavior -query:resource:list -query:resource:connection -query:resource:single
$$;

--! Included functions/update_attendance.sql
create or replace function update_attendance(instance_id bigint, person_id bigint, status attendance_type, note text)
  returns event_instance_registration
  language plpgsql as $$
declare
  eir_row event_instance_registration;
begin
  update event_instance_registration eir
    set status = $3,
        note = $4
  where eir.instance_id = $1 and eir.person_id = $2
  returning eir.* into eir_row;
  return eir_row;
end
$$;

select verify_function('update_attendance');
--! EndIncluded functions/update_attendance.sql
--! Included functions/update_event_attendance.sql
CREATE or replace FUNCTION update_event_attendance(instance_id bigint, person_id bigint, status attendance_type, note text) RETURNS event_attendance
    LANGUAGE plpgsql
    AS $$
declare
  att event_attendance;
  reg event_registration;
  v_id bigint;
begin
  select event_registration.* into reg
  from event_registration
  join event_instance on event_registration.event_id=event_instance.event_id
  left join couple on couple_id=couple.id
  where event_instance.id=$1 and $2 in (event_registration.person_id, man_id, woman_id);

  update event_instance_registration eir
    set status = $3, note = $4
  where eir.legacy_registration_id = reg.id and eir.instance_id = $1 and eir.person_id = $2
  returning eir.id into v_id;

  select * into att from event_attendance where id = v_id;
  return att;
end
$$;

select verify_function('update_event_attendance');

GRANT ALL ON FUNCTION update_event_attendance(instance_id bigint, person_id bigint, status attendance_type, note text) TO anonymous;
--! EndIncluded functions/update_event_attendance.sql
--! Included functions/detach_event_instance.sql
CREATE OR REPLACE FUNCTION public.detach_event_instance(
  p_instance_id     bigint,
  p_tenant_id       bigint DEFAULT public.current_tenant_id(),
  p_new_event_name  text   DEFAULT NULL
)
  RETURNS event
  LANGUAGE plpgsql
AS $$
DECLARE
  v_old_event_id bigint;
  v_new_event_id bigint;
  v_new_event event;
  v_old_att jsonb;
BEGIN
  -- Lock the instance and fetch old event_id
  SELECT ei.event_id
  INTO v_old_event_id
  FROM public.event_instance ei
  WHERE ei.tenant_id = p_tenant_id
    AND ei.id        = p_instance_id
    FOR UPDATE;

  IF v_old_event_id IS NULL THEN
    RAISE EXCEPTION 'detach_event_instance: instance % not found in tenant %', p_instance_id, p_tenant_id;
  END IF;

  -- Also lock the parent event row (prevents concurrent edits during cloning)
  PERFORM 1
  FROM public.event e
  WHERE e.tenant_id = p_tenant_id
    AND e.id        = v_old_event_id
    FOR UPDATE;

  -- Snapshot attendance state for this instance so it can be restored after
  -- the legacy registration bridge is rebuilt for the cloned event.
  SELECT coalesce(jsonb_agg(jsonb_build_object(
    'reg_person_id', er.person_id,
    'reg_couple_id', er.couple_id,
    'attendee_person_id', eir.person_id,
    'status', eir.status,
    'note', eir.note,
    'attendance_created_at', eir.attendance_created_at,
    'attendance_updated_at', eir.attendance_updated_at
  )), '[]'::jsonb)
  INTO v_old_att
  FROM public.event_instance_registration eir
  JOIN public.event_registration er ON er.tenant_id = eir.tenant_id AND er.id = eir.legacy_registration_id AND er.event_id = v_old_event_id
  WHERE eir.tenant_id   = p_tenant_id
    AND eir.instance_id = p_instance_id
    AND eir.person_id   IS NOT NULL;

  -- Reparenting synchronizes the new registrations before sweeping the old
  -- bridge rows, which would collide on the instance/person unit key.
  DELETE FROM public.event_instance_registration eir
  WHERE eir.tenant_id = p_tenant_id
    AND eir.instance_id = p_instance_id
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
  WHERE e.tenant_id = p_tenant_id
    AND e.id        = v_old_event_id
  RETURNING id INTO v_new_event_id;

  -- Copy event_target_cohort (natural key: cohort_id)
  INSERT INTO public.event_target_cohort (tenant_id, event_id, cohort_id, created_at, updated_at)
  SELECT p_tenant_id, v_new_event_id, etc.cohort_id, now(), now()
  FROM public.event_target_cohort etc
  WHERE etc.tenant_id = p_tenant_id
    AND etc.event_id  = v_old_event_id
  ON CONFLICT (event_id, cohort_id) DO NOTHING;

  -- Copy event_trainer (natural key: person_id)
  INSERT INTO public.event_trainer (tenant_id, event_id, person_id, created_at, updated_at, lessons_offered)
  SELECT p_tenant_id, v_new_event_id, et.person_id, now(), now(), et.lessons_offered
  FROM public.event_trainer et
  WHERE et.tenant_id = p_tenant_id
    AND et.event_id  = v_old_event_id
  ON CONFLICT (event_id, person_id) DO NOTHING;

  -- Re-point the instance (event_instance_trainer + any other (tenant_id, instance_id, event_id) dependents follow via FK cascade)
  UPDATE public.event_instance ei
  SET event_id   = v_new_event_id
  WHERE ei.tenant_id = p_tenant_id
    AND ei.id        = p_instance_id
    AND ei.event_id  = v_old_event_id;

  -- Copy registrations: map target_cohort_id by cohort_id (old_tc -> new_tc)
  INSERT INTO public.event_registration (
    tenant_id, event_id, target_cohort_id, couple_id, person_id, note, created_at
  )
  SELECT p_tenant_id, v_new_event_id, new_tc.id, er.couple_id, er.person_id, er.note, er.created_at
  FROM public.event_registration er
  LEFT JOIN public.event_target_cohort old_tc ON old_tc.tenant_id = er.tenant_id AND old_tc.id = er.target_cohort_id
  LEFT JOIN public.event_target_cohort new_tc ON new_tc.tenant_id = er.tenant_id AND new_tc.event_id  = v_new_event_id AND new_tc.cohort_id = old_tc.cohort_id
  WHERE er.tenant_id = p_tenant_id
    AND er.event_id  = v_old_event_id
  ON CONFLICT (event_id, person_id, couple_id) DO NOTHING;

  -- Copy lesson demand:
  -- - trainer maps by person_id (old event_trainer -> new event_trainer)
  -- - registration maps by (person_id, couple_id) to new event_registration
  INSERT INTO public.event_lesson_demand (
    tenant_id, trainer_id, registration_id, lesson_count, created_at, event_id
  )
  SELECT p_tenant_id, new_tr.id, new_reg.id, d.lesson_count, d.created_at, v_new_event_id
  FROM public.event_lesson_demand d
  JOIN public.event_trainer old_tr ON old_tr.tenant_id = p_tenant_id AND old_tr.id = d.trainer_id AND old_tr.event_id  = v_old_event_id
  JOIN public.event_trainer new_tr ON new_tr.tenant_id = p_tenant_id AND new_tr.event_id  = v_new_event_id AND new_tr.person_id = old_tr.person_id
  JOIN public.event_registration old_reg ON old_reg.tenant_id = p_tenant_id AND old_reg.id = d.registration_id AND old_reg.event_id  = v_old_event_id
  JOIN public.event_registration new_reg ON new_reg.tenant_id = p_tenant_id AND new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM old_reg.person_id AND new_reg.couple_id IS NOT DISTINCT FROM old_reg.couple_id
  WHERE d.tenant_id = p_tenant_id
    AND d.event_id  = v_old_event_id
  ON CONFLICT (registration_id, trainer_id) DO NOTHING;

  -- Restore attendance status/note on the newly generated attendance rows for the detached instance.
  UPDATE public.event_instance_registration eir
  SET status = oa.status, note = oa.note
  FROM jsonb_to_recordset(v_old_att) AS oa(
    reg_person_id bigint,
    reg_couple_id bigint,
    attendee_person_id bigint,
    status public.attendance_type,
    note text,
    attendance_created_at timestamptz,
    attendance_updated_at timestamptz
  )
  JOIN public.event_registration new_reg ON new_reg.tenant_id = p_tenant_id AND new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM oa.reg_person_id AND new_reg.couple_id IS NOT DISTINCT FROM oa.reg_couple_id
  WHERE eir.tenant_id       = p_tenant_id
    AND eir.instance_id     = p_instance_id
    AND eir.legacy_registration_id = new_reg.id
    AND eir.person_id       = oa.attendee_person_id;

  -- Restoring status/note correctly counts as an update for the new bridge row,
  -- so restore the original attendance audit lifecycle in a separate statement.
  UPDATE public.event_instance_registration eir
  SET attendance_created_at = oa.attendance_created_at,
      attendance_updated_at = oa.attendance_updated_at
  FROM jsonb_to_recordset(v_old_att) AS oa(
    reg_person_id bigint,
    reg_couple_id bigint,
    attendee_person_id bigint,
    status public.attendance_type,
    note text,
    attendance_created_at timestamptz,
    attendance_updated_at timestamptz
  )
  JOIN public.event_registration new_reg ON new_reg.tenant_id = p_tenant_id AND new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM oa.reg_person_id AND new_reg.couple_id IS NOT DISTINCT FROM oa.reg_couple_id
  WHERE eir.tenant_id = p_tenant_id
    AND eir.instance_id = p_instance_id
    AND eir.legacy_registration_id = new_reg.id
    AND eir.person_id = oa.attendee_person_id;

  select * into v_new_event from public.event where id = v_new_event_id;
  RETURN v_new_event;
END;
$$;

grant all on function detach_event_instance to trainer;
select verify_function('detach_event_instance');
--! EndIncluded functions/detach_event_instance.sql
--! Included functions/tg_cohort_membership__on_status.sql
CREATE or replace FUNCTION app_private.tg_cohort_membership__on_status()
  RETURNS trigger LANGUAGE plpgsql
  security definer
  SET search_path = pg_catalog, public, app_private
  AS $$
begin
  -- Only react to status transitions (or INSERT).
  IF TG_OP <> 'INSERT' AND NEW.status IS NOT DISTINCT FROM OLD.status THEN
    RETURN NEW;
  END IF;

  if NEW.status = 'expired' then
    WITH affected AS (
      SELECT DISTINCT er.id AS registration_id
      FROM event_target_cohort etc
      JOIN event_registration er ON er.event_id = etc.event_id
      WHERE etc.cohort_id = NEW.cohort_id
        AND (er.person_id = NEW.person_id OR (
          er.couple_id IS NOT NULL AND EXISTS (
            SELECT 1 FROM couple c WHERE c.id = er.couple_id AND NEW.person_id IN (c.man_id, c.woman_id))
          )
        )
    ),
    future_att AS (
      -- Capture future attendance rows with their pre-update status
      SELECT eir.id, eir.legacy_registration_id AS registration_id,
             (eir.status IN ('unknown', 'not-excused', 'cancelled')) AS will_be_cancelled
      FROM event_instance_registration eir
      JOIN event_instance ei ON ei.id = eir.instance_id
      JOIN affected a ON a.registration_id = eir.legacy_registration_id
      WHERE eir.person_id IS NOT NULL
        AND ei.since > NEW.until
    ),
    upd AS (
      UPDATE event_instance_registration eir
        SET status = 'cancelled'
        FROM future_att fa
        WHERE eir.id = fa.id
          AND eir.status IN ('unknown', 'not-excused')
        RETURNING eir.legacy_registration_id AS registration_id
    ),
    deletable AS (
      -- Delete registrations where:
      --   (a) all future attendances will be cancelled (pre-update check avoids CTE
      --       visibility issue with upd), AND
      --   (b) there are no non-cancelled past/present attendance rows
      SELECT fa.registration_id
      FROM future_att fa
      GROUP BY fa.registration_id
      HAVING count(*) > 0
         AND bool_and(fa.will_be_cancelled)
         AND NOT EXISTS (
           SELECT 1 FROM event_instance_registration eir2
           JOIN event_instance ei2 ON ei2.id = eir2.instance_id
           WHERE eir2.legacy_registration_id = fa.registration_id
             AND eir2.person_id IS NOT NULL
             AND ei2.since <= NEW.until
             AND eir2.status NOT IN ('unknown', 'cancelled')
         )
    )
    DELETE FROM event_registration er
      USING deletable d
    WHERE er.id = d.registration_id;
  elsif NEW.status = 'active' then
    perform app_private.register_new_cohort_member_to_events(NEW);
    -- TODO: add payments
  end if;
  return NEW;
end;
$$;

grant all on function app_private.tg_cohort_membership__on_status to trainer;
select verify_function('app_private.tg_cohort_membership__on_status', 'cohort_membership');

DROP TRIGGER IF EXISTS _500_on_status ON public.cohort_membership;
CREATE TRIGGER _500_on_status AFTER INSERT OR UPDATE ON public.cohort_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg_cohort_membership__on_status();
--! EndIncluded functions/tg_cohort_membership__on_status.sql
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
    and ($7 is null or $7 = i.parent_id)
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_trainer where event_id = i.event_id and person_id = any (trainer_ids))
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any (participant_ids) and status <> 'cancelled'))
    and (only_mine is false
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any ((select current_person_ids())::bigint[]) and status <> 'cancelled')
      or exists (select 1 from event_trainer where event_id = i.event_id and person_id = any ((select current_person_ids())::bigint[]))
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any ((select current_person_ids())::bigint[])));
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
      e.id as event_id,
      e.name as event_name,
      ea.status
    from public.event_instance_registration ea
    join public.event_instance ei on ei.id = ea.instance_id
    join public.event e on e.id = ei.event_id
    join public.person p on p.id = ea.person_id
    join target_range tr on true
    where
      ei.tenant_id = public.current_tenant_id()
      and ea.person_id is not null
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
      eit.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      e.id as event_id,
      e.name as event_name
    from public.event_instance_trainer eit
    join public.event_instance ei on ei.id = eit.instance_id
    join public.event e on e.id = ei.event_id
    join public.person p on p.id = eit.person_id
    join target_range tr on true
    where
      ei.tenant_id = public.current_tenant_id()
      and not ei.is_cancelled
      and ei.range && tr.range
    union all
    select
      et.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      e.id as event_id,
      e.name as event_name
    from public.event_trainer et
    join public.event_instance ei on ei.event_id = et.event_id
    join public.event e on e.id = ei.event_id
    join public.person p on p.id = et.person_id
    join target_range tr on true
    where
      ei.tenant_id = public.current_tenant_id()
      and not ei.is_cancelled
      and ei.range && tr.range
      and not exists (
        select 1
        from public.event_instance_trainer eit
        where eit.instance_id = ei.id
          and eit.person_id = et.person_id
      )
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
      e.id as event_id,
      e.type as event_type
    from event_instance i join event e on e.id = i.event_id
    where not i.is_cancelled
      and i.since >= scoreboard_entries.since::timestamptz
      and i.until  < scoreboard_entries.until::timestamptz
      and e.type <> 'reservation'
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
    join lateral (select ea.person_id, ea.legacy_registration_id as registration_id, ea.status from event_instance_registration ea where ea.instance_id = inst.instance_id and ea.person_id is not null) ea on true
    join event_registration er on er.id = ea.registration_id
    left join lateral (
      select tc.cohort_id from event_target_cohort tc where tc.id = er.target_cohort_id
    ) tc on true
    where
      (ea.status = 'attended' or inst.event_type = 'lesson')
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
--! Included functions/event_registration_last_attended.sql
create or replace function public.event_registration_last_attended(reg public.event_registration)
  returns timestamp with time zone
  language sql stable as $$
  select max(event_instance.since)
  from public.event_instance_registration eir
  join public.event_instance on event_instance.id = eir.instance_id
  where eir.legacy_registration_id = reg.id
    and eir.person_id is not null
    and eir.status = 'attended'
$$;

grant all on function public.event_registration_last_attended(public.event_registration) to anonymous;
--! EndIncluded functions/event_registration_last_attended.sql
--! Included functions/event_instance_registration_last_attended.sql
create or replace function public.event_instance_registration_last_attended(reg public.event_instance_registration)
  returns timestamp with time zone
  language sql stable as $$
  select max(ei.since)
  from public.event_instance_registration r
  join public.event_instance ei on ei.id = r.instance_id
  where reg.legacy_registration_id is not null
    and r.legacy_registration_id = reg.legacy_registration_id
    and r.person_id is not null
    and r.status = 'attended'
$$;

grant all on function public.event_instance_registration_last_attended(public.event_instance_registration) to anonymous;
--! EndIncluded functions/event_instance_registration_last_attended.sql
--! Included functions/trainer_group_attendance_completion.sql
create or replace function public.trainer_group_attendance_completion(
  since timestamp with time zone default null,
  until timestamp with time zone default null
) returns setof public.trainer_group_attendance_completion
  language sql stable as $_$
  with filtered_instances as (
    select ei.id, ei.event_id
    from event_instance ei
    join event e on e.id = ei.event_id
    where ei.tenant_id = current_tenant_id()
      and not ei.is_cancelled
      and e.type = 'group'
      and coalesce(ei.until, ei.since) < coalesce($2, now())
      and ($1 is null or coalesce(ei.since, ei.until) >= $1)
      and ($2 is null or coalesce(ei.until, ei.since) < $2)
  ),
  trainer_instances as (
    select distinct trainer.person_id, trainer.instance_id
    from filtered_instances fi
    cross join lateral (
      select eit.person_id, fi.id as instance_id
      from event_instance_trainer eit
      where eit.instance_id = fi.id
      union
      select et.person_id, fi.id as instance_id
      from event_trainer et
      where et.event_id = fi.event_id
      and not exists (select 1 from event_instance_trainer where instance_id=fi.id)
    ) trainer
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
--! Included functions/can_trainer_edit_instance.sql
create or replace function app_private.can_trainer_edit_instance(iid bigint)
  returns boolean
  language sql stable security definer leakproof parallel safe
  set search_path = pg_catalog, pg_temp
as $$
  with recursive chain as (
    select i.id, i.parent_id, i.event_id from public.event_instance i where i.id = iid
    union all
    select p.id, p.parent_id, p.event_id from public.event_instance p join chain c on p.id = c.parent_id
  ),
  trainers as (
    select person_id from public.event_instance_trainer where instance_id in (select id from chain)
    union
    select person_id from public.event_trainer where event_id in (select event_id from chain where event_id is not null)
  )
  select exists (
    select 1 from trainers where person_id = any ((select public.current_person_ids())::bigint[])
  ) or not exists (
    select 1 from trainers
  );
$$;

grant all on function app_private.can_trainer_edit_instance(bigint) to anonymous;
--! EndIncluded functions/can_trainer_edit_instance.sql
