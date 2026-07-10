
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

--! include functions/update_event_attendance.sql
--! include functions/detach_event_instance.sql
--! include functions/tg_cohort_membership__on_status.sql
--! include functions/event_instances_for_range.sql
--! include functions/event_overlaps_reports.sql
--! include functions/scoreboard.sql
--! include functions/event_registration_last_attended.sql
--! include functions/trainer_group_attendance_completion.sql
--! include functions/can_trainer_edit_instance.sql
