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

--! include functions/event_instance_stats.sql

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
--! include functions/update_attendance.sql
--! include functions/update_event_attendance.sql
--! include functions/detach_event_instance.sql

alter table public.event_instance
  drop constraint if exists event_instance_parent_id_fkey,
  add constraint event_instance_parent_id_fkey
    foreign key (parent_id) references public.event_instance (id)
    on update cascade on delete no action;

--! include functions/tg_event_instance__validate_parent.sql
--! include functions/tg_event_instance__refresh_manager_person_ids.sql
select app_private.refresh_event_instance_manager_person_ids(id, event_id)
from public.event_instance
where parent_id is null;

--! include policies/event_instance.sql

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
--! include functions/quick_create_event_instances.sql
--! include functions/move_event_instance.sql
--! include functions/event_instances_for_range.sql
--! include functions/event_overlaps_reports.sql
--! include functions/scoreboard.sql
--! include functions/trainer_group_attendance_completion.sql
