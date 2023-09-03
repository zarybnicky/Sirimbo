
drop function if exists person_primary_address;
drop table if exists person_address;
drop table if exists person_email;
drop table if exists person_phone;

grant all on function is_current_tenant_member to anonymous;
comment on table event is E'@omit create';

drop function if exists create_event;
create or replace function create_event(
  inout info event,
  instances event_instance[],
  trainers event_trainer[],
  cohorts event_target_cohort[],
  registrations event_registration[]
) language plpgsql as $$
begin
  insert into event (name, summary, description, type, location_text, capacity, is_visible, is_public, is_locked, enable_notes)
  select info.name, info.summary, info.description, info.type, info.location_text, info.capacity, info.is_visible, info.is_public, info.is_locked, info.enable_notes
  returning * into info;

  insert into event_instance (event_id, since, until)
  select info.id, since, until from unnest(instances) i;

  insert into event_trainer (event_id, person_id, lessons_offered)
  select info.id, person_id, coalesce(lessons_offered, 0) from unnest(trainers) i;

  insert into event_target_cohort (event_id, cohort_id)
  select info.id, cohort_id from unnest(cohorts) i;

  insert into event_registration (event_id, person_id, couple_id, is_confirmed)
  select info.id, person_id, couple_id, true from unnest(registrations) i;
end;
$$;
select verify_function('create_event');
grant all on function create_event to anonymous;
comment on function create_event is E'@arg0variant create
@arg1variant patch
@arg2variant patch
@arg3variant patch
@arg4variant patch
';

create or replace function tenant_couples(t tenant) returns setof couple language sql stable as $$
  select distinct couple.*
  from couple
  join tenant_membership on man_id=person_id or woman_id=person_id
  where now() <@ couple.active_range and now() <@ tenant_membership.active_range and tenant_id=t.id
  order by couple.active_range asc;
$$;
grant all on function tenant_couples to anonymous;
comment on function tenant_couples is '@simpleCollections only';

do $$
begin
if exists (select 1 from pg_sequences where sequencename='akce_a_id_seq') then
  ALTER TABLE public.event ALTER id DROP DEFAULT;
  DROP SEQUENCE public.akce_a_id_seq;
  ALTER TABLE public.event ALTER id ADD GENERATED ALWAYS AS IDENTITY;
  perform setval(pg_get_serial_sequence('event', 'id'), (select max(id) from event));
end if;
end
$$;

do $$ begin
  if not exists (SELECT 1 fROM pg_type JOIN pg_enum ON pg_type.oid = pg_enum.enumtypid WHERE typname = 'event_type' and enumlabel = 'group') then
    alter type event_type add value 'group';
  end if;
end $$;

drop function if exists event_instances_for_range;
CREATE or replace FUNCTION public.event_instances_for_range(only_mine boolean, only_type event_type, start_range timestamptz, end_range timestamptz default null) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $$
  select event_instance.* from event_instance join event on event_id=event.id
  where event.is_visible = true and tstzrange(start_range, end_range, '[]') && range and (only_type is null or event.type = only_type)
  and case only_mine
    when false then true
    else exists (select 1 from event_registration where event_id=event.id and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()))) end
  order by range asc;
$$;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;

create or replace function move_event_instance(id bigint, since timestamptz, until timestamptz, trainer_person_id bigint) returns event_instance language plpgsql as $$
declare
  v_id alias for move_event_instance.id;
  inst event_instance;
begin
  select * from event_instance into inst where event_instance.id = move_event_instance.id;
  if trainer_person_id is not null then
    if (select count(*) = 1 from event_instance_trainer where instance_id = inst.id) then
      update event_instance_trainer set person_id = trainer_person_id where instance_id = inst.id;
    elsif (select count(*) = 1 from event_trainer where event_id = inst.event_id) then
      update event_trainer set person_id = trainer_person_id where event_id = inst.event_id;
    end if;
  end if;
  update event_instance set since=move_event_instance.since, until=move_event_instance.until where event_instance.id=inst.id
  returning * into inst;
  return inst;
end;
$$;
select verify_function('move_event_instance');
grant all on function move_event_instance to anonymous;

ALTER TABLE event ALTER COLUMN capacity TYPE integer USING capacity::integer;

comment on function couple_attendances is E'@simpleCollections only
@filterable
@sortable
@deprecated';

CREATE or replace FUNCTION public.couple_event_instances(p couple) RETURNS SETOF public.event_instance LANGUAGE sql STABLE AS $$
  select distinct event_instance.*
  from event_instance
  join event_registration on event_instance.event_id=event_registration.event_id
  where couple_id = p.man_id;
$$;
GRANT ALL ON FUNCTION public.couple_event_instances(couple) TO anonymous;
comment on function couple_event_instances is E'@simpleCollections only
@filterable
@sortable';

alter table person_invitation add column if not exists email citext not null;
alter table users alter column u_jmeno drop not null;
alter table users alter column u_prijmeni drop not null;
alter table users alter column u_nationality drop not null;

create or replace function public.create_person(inout p person, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamptz) language plpgsql as $$
begin
  insert into person overriding user value select p.* returning * into p;
  if is_member = true then
    insert into tenant_membership (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_trainer = true then
    insert into tenant_trainer (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_admin = true then
    insert into tenant_administrator (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if send_invitation = true and p.email is not null and p.email <> '' then
    insert into person_invitation (person_id, tenant_id, email) values (p.id, current_tenant_id(), p.email);
  end if;
end
$$;

drop trigger if exists _500_notify_admin on users;
drop function if exists app_private.tg_users__notify_admin();
drop policy if exists register_anonymous on users;
drop policy if exists my_tenant on users;
