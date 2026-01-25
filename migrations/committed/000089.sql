--! Previous: sha1:1456130de8024729f572421efd6d1dc0e92300ba
--! Hash: sha1:0303c38a0cb29f1f58b44f9a58d138de93e2f993

--! split: 1-current.sql
CREATE or replace FUNCTION app_private.tg_event_registration__create_attendance() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
begin
  insert into event_attendance (tenant_id, registration_id, instance_id, person_id)
  select NEW.tenant_id, NEW.id, event_instance.id, app_private.event_registration_person_ids(NEW) from event_instance where event_id = NEW.event_id
  on conflict do nothing;
  return NEW;
end;
$$;
select verify_function('app_private.tg_event_registration__create_attendance', 'event_registration');

CREATE or replace FUNCTION app_private.tg_event_instance__create_attendance() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
begin
  insert into event_attendance (tenant_id, registration_id, instance_id, person_id)
  select NEW.tenant_id, event_registration.id, NEW.id, app_private.event_registration_person_ids(event_registration) as id
  from event_registration where event_registration.event_id = NEW.event_id
  on conflict do nothing;
  return NEW;
end;
$$;
select verify_function('app_private.tg_event_instance__create_attendance', 'event_instance');


create index if not exists event_target_cohort_tenant_id_id_idx
  on event_target_cohort (tenant_id, id)
  include (cohort_id);

alter table public.event_attendance add column if not exists event_id bigint;
alter table public.event_instance_trainer add column if not exists event_id bigint;
alter table public.event_lesson_demand add column if not exists event_id bigint;

update public.event_registration r
set tenant_id = e.tenant_id
from public.event e
where e.id = r.event_id and e.tenant_id <> r.tenant_id;

update public.event_target_cohort etc
set tenant_id = e.tenant_id
from public.event e
where e.id = etc.event_id and e.tenant_id <> etc.tenant_id;

update public.event_attendance ea
set event_id = i.event_id, tenant_id = i.tenant_id
from public.event_instance i
where i.id = ea.instance_id and (ea.event_id is null or ea.tenant_id <> i.tenant_id);

update public.event_instance_trainer eit
set event_id = i.event_id, tenant_id = i.tenant_id
from public.event_instance i
where i.id = eit.instance_id and (eit.event_id is null or eit.tenant_id <> i.tenant_id);

update public.event_lesson_demand eld
set event_id = r.event_id, tenant_id = r.tenant_id
from public.event_registration r
where r.id = eld.registration_id and (eld.event_id is null or eld.tenant_id <> r.tenant_id);

do
$$
begin
  if not exists (select 1 from pg_constraint where conname = 'event_registration_tenant_id_id_event_id_ux') then
    alter table public.event_registration
      add constraint event_registration_tenant_id_id_event_id_ux
        unique (tenant_id, id, event_id);
  end if;
  if not exists (select 1 from pg_constraint where conname = 'event_instance_tenant_id_id_event_id_ux') then
    alter table public.event_instance
      add constraint event_instance_tenant_id_id_event_id_ux
        unique (tenant_id, id, event_id);
  end if;
end;
$$;

alter table public.event_attendance
  drop constraint if exists event_attendance_tenant_id_instance_id_event_id_fkey,
  add constraint event_attendance_tenant_id_instance_id_event_id_fkey
    foreign key (tenant_id, instance_id, event_id)
      references public.event_instance (tenant_id, id, event_id) on update cascade on delete cascade;

alter table public.event_instance_trainer
  drop constraint if exists event_instance_trainer_tenant_id_instance_id_event_id_fkey,
  add constraint event_instance_trainer_tenant_id_instance_id_event_id_fkey
    foreign key (tenant_id, instance_id, event_id)
      references public.event_instance (tenant_id, id, event_id) on update cascade on delete cascade;

alter table public.event_lesson_demand
  drop constraint if exists event_lesson_demand_tenant_id_registration_id_event_id_fkey,
  add constraint event_lesson_demand_tenant_id_registration_id_event_id_fkey
    foreign key (tenant_id, registration_id, event_id)
      references public.event_registration (tenant_id, id, event_id) on update cascade on delete cascade;


alter table event_attendance alter event_id set not null;
alter table event_instance_trainer alter event_id set not null;
alter table event_lesson_demand alter event_id set not null;

comment on column event_attendance.event_id is '@omit';
comment on column event_instance_trainer.event_id is '@omit';
comment on column event_lesson_demand.event_id is '@omit';

create or replace function app_private.tg__set_event_id_from_instance_id() returns trigger language plpgsql as $$
begin
  select i.event_id into new.event_id from public.event_instance i where i.id = new.instance_id;
  return new;
end $$;
select verify_function('app_private.tg__set_event_id_from_instance_id', 'event_attendance');
select verify_function('app_private.tg__set_event_id_from_instance_id', 'event_instance_trainer');

create or replace function app_private.tg__set_event_id_from_registration_id() returns trigger language plpgsql as $$
begin
  select i.event_id into new.event_id from public.event_registration i where i.id = new.registration_id;
  return new;
end $$;
select verify_function('app_private.tg__set_event_id_from_registration_id', 'event_lesson_demand');

drop trigger if exists _100_event_id on public.event_attendance;
create trigger _100_event_id
  before insert or update of instance_id
  on public.event_attendance
  for each row execute function app_private.tg__set_event_id_from_instance_id();

drop trigger if exists _100_event_id on public.event_instance_trainer;
create trigger _100_event_id
  before insert or update of instance_id
  on public.event_instance_trainer
  for each row execute function app_private.tg__set_event_id_from_instance_id();

drop trigger if exists _100_event_id on public.event_lesson_demand;
create trigger _100_event_id
  before insert or update of registration_id
  on public.event_lesson_demand
  for each row execute function app_private.tg__set_event_id_from_registration_id();


with
  mism as (
    select
      ea.id              as attendance_id,
      ea.tenant_id,
      ea.instance_id,
      ea.registration_id as old_registration_id,
      ea.person_id,

      i.event_id         as new_event_id,

      r.event_id         as old_event_id,
      r.person_id        as reg_person_id,
      r.couple_id        as reg_couple_id,
      r.note             as reg_note,
      r.created_at       as reg_created_at,
      r.updated_at       as reg_updated_at,
      tc_old.cohort_id   as reg_cohort_id
    from public.event_attendance ea
           join public.event_instance i
                on i.id = ea.instance_id
           join public.event_registration r
                on r.id = ea.registration_id
           left join public.event_target_cohort tc_old
                     on tc_old.id = r.target_cohort_id
    where r.event_id <> i.event_id
  ),

  regs_to_fix as (
    select distinct on (old_registration_id)
      tenant_id,
      old_registration_id,
      new_event_id,
      reg_person_id,
      reg_couple_id,
      reg_note,
      reg_created_at,
      reg_updated_at,
      reg_cohort_id
    from mism
    order by old_registration_id
  ),

-- ensure target cohort exists on the new event (keyed by cohort_id)
  ensure_target_cohort as (
    insert into public.event_target_cohort (tenant_id, event_id, cohort_id, created_at, updated_at)
      select distinct
        rtf.tenant_id,
        rtf.new_event_id,
        rtf.reg_cohort_id,
        now(),
        now()
      from regs_to_fix rtf
      where rtf.reg_cohort_id is not null
      on conflict (event_id, cohort_id) do update
        set updated_at = excluded.updated_at
      returning tenant_id, event_id, cohort_id, id
  ),

-- create (or reuse) the new registrations *and* get their ids back
-- critical: conflict target is your existing UNIQUE NULLS NOT DISTINCT (event_id, person_id, couple_id)
  ins_reg as (
    insert into public.event_registration
      (tenant_id, event_id, target_cohort_id, couple_id, person_id, note, created_at, updated_at)
      select
        rtf.tenant_id,
        rtf.new_event_id,
        case
          when rtf.reg_cohort_id is null then null
          else (
            select tc.id
            from public.event_target_cohort tc
            where tc.tenant_id = rtf.tenant_id
              and tc.event_id  = rtf.new_event_id
              and tc.cohort_id = rtf.reg_cohort_id
          )
          end as target_cohort_id,
        rtf.reg_couple_id,
        rtf.reg_person_id,
        rtf.reg_note,
        rtf.reg_created_at,
        rtf.reg_updated_at
      from regs_to_fix rtf
      on conflict on constraint event_registration_unique_event_person_couple_key
        do update
          set updated_at = excluded.updated_at
      returning tenant_id, event_id, person_id, couple_id, id
  ),

-- build a total mapping old_registration_id -> new_registration_id (must exist for all)
  reg_map as (
    select
      rtf.old_registration_id,
      (
        select ir.id
        from ins_reg ir
        where ir.tenant_id = rtf.tenant_id
          and ir.event_id  = rtf.new_event_id
          and ir.person_id is not distinct from rtf.reg_person_id
          and ir.couple_id is not distinct from rtf.reg_couple_id
        limit 1
      ) as new_registration_id
    from regs_to_fix rtf
  ),

-- hard assertion helper: any missing target reg means you cannot add the FK
  missing_targets as (
    select *
    from reg_map
    where new_registration_id is null
  ),

  target_regs as (
    select distinct new_registration_id
    from reg_map
  ),

  candidates as (
    -- mismatched rows to be moved
    select
      ea.id as attendance_id,
      ea.tenant_id,
      ea.instance_id,
      ea.person_id,
      ea.status,
      rm.new_registration_id as target_registration_id
    from mism m
           join reg_map rm
                on rm.old_registration_id = m.old_registration_id
           join public.event_attendance ea
                on ea.id = m.attendance_id

    union all

    -- rows already on the target registration (could collide with moved rows)
    select
      ea.id,
      ea.tenant_id,
      ea.instance_id,
      ea.person_id,
      ea.status,
      ea.registration_id as target_registration_id
    from public.event_attendance ea
           join target_regs tr
                on tr.new_registration_id = ea.registration_id
  ),

  choose_keep as (
    select
      tenant_id,
      instance_id,
      person_id,
      target_registration_id,
      coalesce(
          min(attendance_id) filter (where status <> 'unknown'),
          min(attendance_id)
      ) as keep_attendance_id
    from candidates
    group by 1,2,3,4
  ),

  delete_dupes as (
    delete from public.event_attendance ea
      using candidates c
        join choose_keep k
        on k.tenant_id = c.tenant_id
          and k.instance_id = c.instance_id
          and k.person_id = c.person_id
          and k.target_registration_id = c.target_registration_id
      where ea.id = c.attendance_id
        and ea.id <> k.keep_attendance_id
      returning ea.id
  ),

  fix_attendance as (
    update public.event_attendance ea
      set registration_id = c.target_registration_id
      from candidates c
        join choose_keep k
        on k.tenant_id = c.tenant_id
          and k.instance_id = c.instance_id
          and k.person_id = c.person_id
          and k.target_registration_id = c.target_registration_id
          and k.keep_attendance_id = c.attendance_id
      where ea.id = c.attendance_id
        and ea.registration_id <> c.target_registration_id
      returning ea.id
  ),

-- final integrity check: after repairs, registration.event_id must match attendance.event_id
  remaining_mismatches as (
    select ea.id
    from public.event_attendance ea
           join public.event_registration r on r.id = ea.registration_id
    where r.tenant_id = ea.tenant_id
      and r.event_id <> ea.event_id
  )

select
  (select count(*) from missing_targets)       as missing_target_registrations,
  (select count(*) from delete_dupes)          as attendance_rows_deleted,
  (select count(*) from fix_attendance)        as attendance_rows_updated,
  (select count(*) from remaining_mismatches)  as remaining_fk_violations;

alter table public.event_attendance
  drop constraint if exists event_attendance_tenant_id_registration_id_event_id_fkey,
  add constraint event_attendance_tenant_id_registration_id_event_id_fkey
    foreign key (tenant_id, registration_id, event_id)
      references public.event_registration (tenant_id, id, event_id) on update cascade on delete cascade;

comment on constraint event_attendance_tenant_id_registration_id_event_id_fkey
  on public.event_attendance
  is E'@fieldName registration
@foreignFieldName eventAttendancesByRegistrationId
@behavior -delete';

comment on constraint event_attendance_tenant_id_instance_id_event_id_fkey
  on public.event_attendance
  is E'@fieldName instance
@foreignFieldName eventAttendancesByInstanceId';

ALTER TABLE event_attendance
  drop constraint if exists event_attendance_instance_id_fkey,
  drop constraint if exists event_attendance_registration_id_fkey;

comment on constraint event_instance_trainer_tenant_id_instance_id_event_id_fkey
  on public.event_instance_trainer
  is E'@fieldName instance
@foreignFieldName eventInstanceTrainersByInstanceId';

ALTER TABLE event_instance_trainer
  drop constraint if exists event_instance_trainer_instance_id_fkey;

comment on constraint event_lesson_demand_tenant_id_registration_id_event_id_fkey
  on public.event_lesson_demand
  is E'@fieldName registration
@foreignFieldName eventLessonDemandsByRegistrationId
@behavior -delete';

ALTER TABLE event_lesson_demand
  drop constraint if exists event_lesson_demand_registration_id_fkey;

select app_private.drop_policies('public.event_registration');
create policy admin_all on event_registration to administrator using (true);
CREATE POLICY trainer_same_tenant ON event_registration to trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
create policy update_my on event_registration for update using (
  (select event_is_registration_open(event) from event where event_id = event.id)
  and (person_id = any ((select current_person_ids())::bigint[]) or couple_id = any ((select current_couple_ids())::bigint[]))
);
create policy delete_my on event_registration for delete using (
  (select event_is_registration_open(event) from event where event_id = event.id)
  and (person_id = any ((select current_person_ids())::bigint[]) or couple_id = any ((select current_couple_ids())::bigint[]))
);
create policy view_visible_event on event_registration for select using (event_id = any (select id from event));

select app_private.drop_policies('public.event_attendance');
CREATE POLICY admin_all ON event_attendance TO administrator USING (true);
CREATE POLICY admin_trainer_insert ON event_attendance for insert TO trainer with check (exists (
  select 1
  from event_instance
  left join event_trainer on event_instance.event_id=event_trainer.event_id
  left join event_instance_trainer on event_instance.id=event_instance_trainer.instance_id
  where event_attendance.instance_id=event_instance.id and (
    event_instance_trainer.person_id = any ((select current_person_ids())::bigint[])
    or event_trainer.person_id = any ((select current_person_ids())::bigint[])
  )
));
CREATE POLICY admin_trainer ON event_attendance for update TO trainer USING (exists (
  select 1
  from event_instance
  left join event_trainer on event_instance.event_id=event_trainer.event_id
  left join event_instance_trainer on event_instance.id=event_instance_trainer.instance_id
  where event_attendance.instance_id=event_instance.id and (
    event_instance_trainer.person_id = any ((select current_person_ids())::bigint[])
    or event_trainer.person_id = any ((select current_person_ids())::bigint[])
  )
));
create policy view_visible_event on event_attendance for select using (event_id = any (select id from event));

select app_private.drop_policies('public.event_instance');
CREATE POLICY admin_same_tenant ON public.event_instance TO administrator USING (true);
CREATE POLICY trainer_same_tenant ON public.event_instance TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
CREATE POLICY view_visible_event ON public.event_instance FOR SELECT USING (event_id = any (select id from event));

select app_private.drop_policies('public.event_external_registration');

CREATE POLICY admin_all ON public.event_external_registration TO administrator USING (true);
CREATE POLICY admin_my ON public.event_external_registration TO member
  USING ((SELECT public.event_is_registration_open(event.*) FROM event WHERE event_id = event.id) AND (created_by = public.current_user_id()));
CREATE POLICY register_public ON public.event_external_registration FOR INSERT TO anonymous WITH CHECK ((SELECT event.is_public FROM event WHERE event_id = event.id));
CREATE POLICY trainer_same_tenant ON public.event_external_registration TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
CREATE POLICY view_visible_event ON public.event_external_registration FOR SELECT TO member USING (event_id = any (SELECT id from event));

select app_private.drop_policies('public.event_lesson_demand');
CREATE POLICY admin_all ON public.event_lesson_demand TO administrator USING (true);
CREATE POLICY view_visible_event ON public.event_lesson_demand FOR SELECT USING (event_id = any (SELECT id from event));

CREATE or replace FUNCTION public.event_instances_for_range(
  only_type public.event_type,
  start_range timestamp with time zone,
  end_range timestamp with time zone DEFAULT NULL::timestamp with time zone,
  trainer_ids bigint[] = null,
  only_mine boolean = false
) RETURNS SETOF event_instance as $$
  select i.*
  from event_instance i
  join event e on event_id=e.id and e.is_visible AND (only_type IS NULL OR e.type = only_type)
  where i.tenant_id = current_tenant_id()
    and i.range && tstzrange(start_range, coalesce(end_range, 'infinity'::timestamptz), '[]')
    and (trainer_ids is null
      or exists (select 1 from event_trainer where person_id = any (trainer_ids) and event_id = e.id)
      or exists (select 1 from event_instance_trainer where person_id = any (trainer_ids) and instance_id=i.id))
    and (only_mine is FALSE
      or i.event_id = any (
        select r.event_id from event_registration r where r.person_id = any ((select current_person_ids())::bigint[])
        union all
        select r.event_id from event_registration r where r.couple_id = any ((select current_couple_ids())::bigint[])
        union all
        SELECT et2.event_id FROM event_trainer et2 WHERE et2.person_id = ANY ((select current_person_ids())::bigint[])
      )
      OR i.id IN (
        SELECT eit2.instance_id FROM event_instance_trainer eit2 WHERE eit2.person_id = ANY ((select current_person_ids())::bigint[])));
$$ stable language sql;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;

create or replace function app_private.can_trainer_edit_event(eid bigint) returns boolean language sql as $$
  select exists (
    select 1 from event_trainer where eid = event_id and person_id = any ((select current_person_ids())::bigint[])
  ) or not exists (
    select 1 from event_trainer where eid = event_id
  );
$$ security definer stable leakproof parallel safe;

grant all on function app_private.can_trainer_edit_event to anonymous;


drop index if exists event_instance_range_idx;
CREATE INDEX event_instance_range_idx on event_instance USING gist (range);
CREATE INDEX if not exists event_instance_tenant_idx on event_instance (tenant_id);

CREATE INDEX if not exists event_attendance_event_id_idx ON public.event_attendance USING btree (event_id);

create index if not exists event_attendance_tenant_instance_idx on event_attendance (tenant_id, instance_id) include (registration_id, person_id, status, event_id);

create index if not exists event_registration_tenant_event_id_idx
  on event_registration (tenant_id, event_id);
