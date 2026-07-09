create table if not exists event_instance_registration (
  id bigint primary key generated always as identity,
  tenant_id bigint not null references tenant (id) on update cascade on delete cascade default public.current_tenant_id(),
  instance_id bigint not null references event_instance (id) on update cascade on delete cascade,
  event_id bigint null references event (id) on update cascade on delete cascade,
  parent_registration_id bigint null references event_instance_registration (id) on update cascade on delete cascade default null,
  couple_id bigint null references couple (id) on update cascade on delete cascade default null,
  person_id bigint null references person (id) on update cascade on delete cascade default null,
  target_cohort_id bigint null references event_target_cohort (id) on update cascade on delete restrict default null,
  legacy_registration_id bigint null default null,
  status attendance_type null default null,
  note text null default null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  constraint event_instance_registration_shape check (
    case
      when parent_registration_id is null
        then (couple_id is not null and person_id is null)
          or (couple_id is null and person_id is not null)
      else person_id is not null and couple_id is null
    end
  )
);

comment on table event_instance_registration is E'@omit';

drop trigger if exists _100_timestamps on event_instance_registration;
create trigger _100_timestamps before insert or update on event_instance_registration
  for each row execute procedure app_private.tg__timestamps();

grant all on table event_instance_registration to anonymous;
alter table event_instance_registration enable row level security;

create index if not exists event_instance_registration_tenant_id_idx on event_instance_registration (tenant_id);
create index if not exists event_instance_registration_instance_id_idx on event_instance_registration (instance_id);
create index if not exists event_instance_registration_event_id_idx on event_instance_registration (event_id);
create index if not exists event_instance_registration_parent_id_idx on event_instance_registration (parent_registration_id);
create index if not exists event_instance_registration_couple_id_idx on event_instance_registration (couple_id);
create index if not exists event_instance_registration_person_id_idx on event_instance_registration (person_id);
create index if not exists event_instance_registration_target_cohort_id_idx on event_instance_registration (target_cohort_id);
create index if not exists event_instance_registration_legacy_registration_id_idx on event_instance_registration (legacy_registration_id);

create unique index if not exists event_instance_registration_bridge_key
  on event_instance_registration (legacy_registration_id, instance_id, (coalesce(person_id, -1)));

select app_private.drop_policies('public.event_instance_registration');

create policy current_tenant on event_instance_registration as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_instance_registration to administrator using (true);
create policy trainer_same_tenant on event_instance_registration to trainer
  using (app_private.can_trainer_edit_instance(instance_id)) with check (true);
create policy view_visible_instance on event_instance_registration for select
  using (instance_id = any (select id from event_instance));
create policy insert_my on event_instance_registration for insert with check (
  person_id = any ((select current_person_ids())::bigint[])
  or couple_id = any ((select current_couple_ids())::bigint[])
);
create policy delete_my on event_instance_registration for delete using (
  person_id = any ((select current_person_ids())::bigint[])
  or couple_id = any ((select current_couple_ids())::bigint[])
);

create or replace function app_private.sync_eir_registrations(reg_ids bigint[])
  returns void language plpgsql security definer as $$
begin
  -- couple unit rows: one per couple registration x each instance of its event
  insert into event_instance_registration
    (legacy_registration_id, tenant_id, instance_id, event_id, couple_id, target_cohort_id, note)
  select er.id, er.tenant_id, ei.id, er.event_id, er.couple_id, er.target_cohort_id, er.note
  from event_registration er
  join event_instance ei on ei.event_id = er.event_id
  where er.couple_id is not null and er.id = any (reg_ids)
  on conflict (legacy_registration_id, instance_id, (coalesce(person_id, -1))) do update
    set event_id = excluded.event_id,
        couple_id = excluded.couple_id,
        target_cohort_id = excluded.target_cohort_id,
        note = excluded.note;

  -- couple person rows: registration's people x instances, linked to the unit.
  -- New rows start 'unknown'; on conflict status/note are left as-is (native).
  insert into event_instance_registration
    (legacy_registration_id, parent_registration_id, tenant_id, instance_id, event_id, person_id, status)
  select er.id, u.id, er.tenant_id, ei.id, er.event_id, pid, 'unknown'
  from event_registration er
  join event_instance ei on ei.event_id = er.event_id
  join event_instance_registration u
    on u.legacy_registration_id = er.id and u.instance_id = ei.id and u.person_id is null
  cross join lateral app_private.event_registration_person_ids(er) as pid
  where er.couple_id is not null and er.id = any (reg_ids)
  on conflict (legacy_registration_id, instance_id, (coalesce(person_id, -1))) do update
    set parent_registration_id = excluded.parent_registration_id,
        event_id = excluded.event_id;

  -- solo rows: combined unit/person per instance
  insert into event_instance_registration
    (legacy_registration_id, tenant_id, instance_id, event_id, person_id, status, note, target_cohort_id)
  select er.id, er.tenant_id, ei.id, er.event_id, er.person_id, 'unknown', er.note, er.target_cohort_id
  from event_registration er
  join event_instance ei on ei.event_id = er.event_id
  where er.couple_id is null and er.id = any (reg_ids)
  on conflict (legacy_registration_id, instance_id, (coalesce(person_id, -1))) do update
    set event_id = excluded.event_id,
        note = excluded.note,
        target_cohort_id = excluded.target_cohort_id;

  -- orphan sweep: rows no longer in the registration x instances product
  -- (registration cancelled, instance removed, couple recomposed).
  delete from event_instance_registration e
  where e.legacy_registration_id = any (reg_ids)
    and not exists (
      select 1 from event_registration er
      join event_instance ei on ei.event_id = er.event_id
      where er.id = e.legacy_registration_id and ei.id = e.instance_id
        and (e.person_id is null
          or e.person_id in (select app_private.event_registration_person_ids(er)))
    );
end;
$$;
select verify_function('app_private.sync_eir_registrations');
grant all on function app_private.sync_eir_registrations to anonymous;

create or replace function app_private.tg_event_registration__sync_eir() returns trigger
  language plpgsql security definer as $$
begin
  -- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
  if tg_op = 'DELETE' then
    perform app_private.sync_eir_registrations(array(select distinct id from deleted_rows));
  else
    perform app_private.sync_eir_registrations(array(select distinct id from changed_rows));
  end if;
  return null;
end;
$$;
select verify_function('app_private.tg_event_registration__sync_eir', 'public.event_registration');

drop trigger if exists _600_sync_eir_registration_ins on event_registration;
create trigger _600_sync_eir_registration_ins after insert on event_registration
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event_registration__sync_eir();

drop trigger if exists _600_sync_eir_registration_upd on event_registration;
create trigger _600_sync_eir_registration_upd after update on event_registration
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event_registration__sync_eir();

drop trigger if exists _600_sync_eir_registration_del on event_registration;
create trigger _600_sync_eir_registration_del after delete on event_registration
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_event_registration__sync_eir();

create or replace function app_private.tg_event_instance__sync_eir() returns trigger
  language plpgsql security definer as $$
begin
  -- @plpgsql_check_options: newtable = changed_rows
  perform app_private.sync_eir_registrations(array(
    select distinct er.id from event_registration er
    join changed_rows ci on ci.event_id = er.event_id));
  return null;
end;
$$;
select verify_function('app_private.tg_event_instance__sync_eir', 'public.event_instance');

drop trigger if exists _600_sync_eir_instance_ins on event_instance;
create trigger _600_sync_eir_instance_ins after insert on event_instance
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event_instance__sync_eir();

create or replace function app_private.tg_event_attendance__propagate_status() returns trigger
  language plpgsql security definer as $$
begin
  update event_instance_registration eir
    set status = new.status
  where eir.legacy_registration_id = new.registration_id
    and eir.instance_id = new.instance_id
    and eir.person_id = new.person_id
    and eir.status is distinct from new.status;
  return null;
end;
$$;
select verify_function('app_private.tg_event_attendance__propagate_status', 'public.event_attendance');

drop trigger if exists _600_propagate_status on event_attendance;
create trigger _600_propagate_status after update of status on event_attendance
  for each row execute function app_private.tg_event_attendance__propagate_status();

-- One-time backfill: build every registration's skeleton, then seed native
-- status from the current attendance table.
select app_private.sync_eir_registrations(array(select id from event_registration));

update event_instance_registration eir
  set status = ea.status
from event_attendance ea
where ea.registration_id = eir.legacy_registration_id
  and ea.instance_id = eir.instance_id
  and ea.person_id = eir.person_id
  and eir.status is distinct from ea.status;

create or replace function event_instance_registrations(inst event_instance)
  returns setof event_registration language sql stable as $$
  select er.*
  from event_registration er
  join event_instance_registration eir
    on eir.legacy_registration_id = er.id
   and eir.instance_id = inst.id
   and eir.parent_registration_id is null;
$$;
grant all on function event_instance_registrations to anonymous;

create or replace function event_instance_my_registrations(inst event_instance)
  returns setof event_registration language sql stable as $$
  select er.*
  from event_registration er
  join event_instance_registration eir
    on eir.legacy_registration_id = er.id
   and eir.instance_id = inst.id
   and eir.parent_registration_id is null
  where er.person_id = any (current_person_ids())
     or er.couple_id = any (current_couple_ids());
$$;
comment on function event_instance_my_registrations is '@simpleCollections only';
grant all on function event_instance_my_registrations to anonymous;

create or replace function event_instance_remaining_person_spots(inst event_instance)
  returns integer language sql stable security definer as $$
  select inst.capacity
    - (select coalesce(count(*), 0) from event_instance_registration eir
        where eir.instance_id = inst.id and eir.person_id is not null)
    - (select coalesce(count(id), 0) from event_external_registration
        where event_id = inst.event_id);
$$;
grant all on function event_instance_remaining_person_spots to anonymous;

create or replace function event_instance_target_cohorts(inst event_instance)
  returns setof event_target_cohort language sql stable as $$
  select * from event_target_cohort where event_id = inst.event_id;
$$;
comment on function event_instance_target_cohorts is '@simpleCollections only';
grant all on function event_instance_target_cohorts to anonymous;
