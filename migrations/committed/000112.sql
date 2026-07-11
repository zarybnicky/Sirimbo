--! Previous: sha1:bf5fe0c2ec1095bc783aae399898b8462c82589c
--! Hash: sha1:87f56e3b07dc33f83d62b34b41af157d2b22369a

--! split: 1-current.sql
drop function if exists public.event_instance_target_cohorts(public.event_instance);
drop function if exists app_private.register_new_cohort_member_to_events(public.cohort_membership);
drop function if exists app_private.tg_event_target_cohort__register_members();
drop function if exists app_private.tg_event_target_cohort__unregister_members();
drop function if exists app_private.detach_event_instance_impl(bigint, bigint, text);

drop function if exists public.register_to_event_many;
drop function if exists public.edit_registration(bigint, text);
drop function if exists public.cancel_registration(bigint);
drop type if exists public.register_to_event_type;

drop function if exists public.upsert_event;
drop type if exists public.event_registration_type_input;

-- Compatibility rekeys are not edits to the exact occurrence records.
create or replace trigger _100_timestamps
  before insert or update of
    tenant_id, instance_id, parent_registration_id, couple_id, person_id,
    target_cohort_id, status, note, attendance_note, registration_status,
    source, created_at, updated_at
  on public.event_instance_registration
  for each row execute function app_private.tg__timestamps();

create or replace trigger _100_timestamps
  before insert or update of
    tenant_id, instance_id, person_id, created_at, updated_at, lessons_offered
  on public.event_instance_trainer
  for each row execute function app_private.tg__timestamps();

create or replace trigger _100_timestamps
  before insert or update of
    tenant_id, trainer_id, registration_id, lesson_count, created_at, updated_at
  on public.event_lesson_demand
  for each row execute function app_private.tg__timestamps();

--! Included functions/sync_eir_registrations.sql
create or replace function app_private.sync_eir_registrations(reg_ids bigint[])
  returns void
  language plpgsql
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
begin
  -- Legacy targetless registrations do not record who created them, so their
  -- source stays null; cohort-backed roots can be classified exactly.
  insert into event_instance_registration as registration
    (legacy_registration_id, tenant_id, instance_id, event_id, couple_id,
      target_cohort_id, source, note)
  select er.id, er.tenant_id, ei.id, er.event_id, er.couple_id,
    target.cohort_id,
    case when target.cohort_id is not null then 'cohort'::event_registration_source end,
    er.note
  from event_registration er
  join event_instance ei on ei.event_id = er.event_id
  left join event_target_cohort target on target.id = er.target_cohort_id
  where er.couple_id is not null and er.id = any (reg_ids)
  on conflict (legacy_registration_id, instance_id, (coalesce(person_id, -1))) do update
    set event_id = excluded.event_id,
        couple_id = excluded.couple_id,
        note = excluded.note
    where (registration.event_id, registration.couple_id, registration.note)
      is distinct from (excluded.event_id, excluded.couple_id, excluded.note);

  insert into event_instance_registration as registration
    (legacy_registration_id, parent_registration_id, tenant_id, instance_id, event_id, person_id, status)
  select er.id, unit.id, er.tenant_id, ei.id, er.event_id, people.person_id, 'unknown'
  from event_registration er
  join event_instance ei on ei.event_id = er.event_id
  join event_instance_registration unit
    on unit.legacy_registration_id = er.id and unit.instance_id = ei.id and unit.person_id is null
  cross join lateral app_private.event_registration_person_ids(er) as people(person_id)
  where er.couple_id is not null and er.id = any (reg_ids)
  on conflict (legacy_registration_id, instance_id, (coalesce(person_id, -1))) do update
    set parent_registration_id = excluded.parent_registration_id,
        event_id = excluded.event_id
    where (registration.parent_registration_id, registration.event_id)
      is distinct from (excluded.parent_registration_id, excluded.event_id);

  insert into event_instance_registration as registration
    (legacy_registration_id, tenant_id, instance_id, event_id, person_id, status,
      note, target_cohort_id, source)
  select er.id, er.tenant_id, ei.id, er.event_id, er.person_id, 'unknown', er.note,
    target.cohort_id,
    case when target.cohort_id is not null then 'cohort'::event_registration_source end
  from event_registration er
  join event_instance ei on ei.event_id = er.event_id
  left join event_target_cohort target on target.id = er.target_cohort_id
  where er.couple_id is null and er.id = any (reg_ids)
  on conflict (legacy_registration_id, instance_id, (coalesce(person_id, -1))) do update
    set event_id = excluded.event_id,
        note = excluded.note,
        parent_registration_id = excluded.parent_registration_id,
        couple_id = excluded.couple_id
    where (
      registration.event_id,
      registration.note,
      registration.parent_registration_id,
      registration.couple_id
    ) is distinct from (
      excluded.event_id,
      excluded.note,
      excluded.parent_registration_id,
      excluded.couple_id
    );

  delete from event_instance_registration registration
  where registration.legacy_registration_id = any (reg_ids)
    and not exists (
      select 1
      from event_registration er
      join event_instance ei on ei.event_id = er.event_id
      where er.id = registration.legacy_registration_id
        and ei.id = registration.instance_id
        and (
          (registration.person_id is null and er.couple_id is not null)
          or registration.person_id in (select app_private.event_registration_person_ids(er))
        )
    );
end;
$$;

select verify_function('app_private.sync_eir_registrations');
grant all on function app_private.sync_eir_registrations(bigint[]) to anonymous;
--! EndIncluded functions/sync_eir_registrations.sql
--! Included functions/reconcile_event_instance_cohort_registrations.sql
create or replace function app_private.reconcile_event_instance_cohort_registrations(
  p_instance_ids bigint[],
  p_person_ids bigint[] default null
) returns void
  language sql
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select instance.id
  from event_instance instance
  where instance.id = any (p_instance_ids)
  order by instance.id
  for update;

  with desired as (
    select target.instance_id, membership.person_id,
      min(target.cohort_id) as target_cohort_id
    from event_instance_target_cohort target
    join event_instance instance on instance.id = target.instance_id
    join cohort_membership membership on membership.cohort_id = target.cohort_id
    where target.instance_id = any (p_instance_ids)
      and instance.since >= now()
      and membership.status = 'active'
      and membership.active_range @> now()
      and (p_person_ids is null or membership.person_id = any (p_person_ids))
    group by target.instance_id, membership.person_id
  )
  update event_instance_registration r
  set registration_status = 'active',
      source = 'cohort',
      target_cohort_id = desired.target_cohort_id
  from desired
  where r.instance_id = desired.instance_id
    and r.person_id = desired.person_id
    and r.parent_registration_id is null
    and r.source = 'cohort'
    and not exists (
      select 1
      from event_instance_registration attendee
      where attendee.instance_id = desired.instance_id
        and attendee.person_id = desired.person_id
        and attendee.registration_status = 'active'
        and coalesce(attendee.parent_registration_id, attendee.id) <> r.id
    )
    and (r.registration_status, r.target_cohort_id)
      is distinct from (
        'active'::event_instance_registration_status,
        desired.target_cohort_id
      );

  with desired as (
    select target.instance_id, instance.tenant_id, membership.person_id,
      min(target.cohort_id) as target_cohort_id
    from event_instance_target_cohort target
    join event_instance instance on instance.id = target.instance_id
    join cohort_membership membership on membership.cohort_id = target.cohort_id
    where target.instance_id = any (p_instance_ids)
      and instance.since >= now()
      and membership.status = 'active'
      and membership.active_range @> now()
      and (p_person_ids is null or membership.person_id = any (p_person_ids))
    group by target.instance_id, instance.tenant_id, membership.person_id
  )
  insert into event_instance_registration (
    tenant_id, instance_id, person_id, target_cohort_id,
    source, status
  )
  select desired.tenant_id, desired.instance_id, desired.person_id,
    desired.target_cohort_id, 'cohort', 'unknown'
  from desired
  where not exists (
    select 1
    from event_instance_registration r
    where r.instance_id = desired.instance_id
      and r.person_id = desired.person_id
      and r.parent_registration_id is null
  ) and not exists (
    select 1
    from event_instance_registration attendee
    where attendee.instance_id = desired.instance_id
      and attendee.person_id = desired.person_id
      and attendee.registration_status = 'active'
  );

  update event_instance_registration r
  set registration_status = 'cancelled'
  from event_instance instance
  where r.instance_id = instance.id
    and r.instance_id = any (p_instance_ids)
    and r.parent_registration_id is null
    and r.person_id is not null
    and r.source = 'cohort'
    and r.registration_status = 'active'
    and instance.since >= now()
    and (p_person_ids is null or r.person_id = any (p_person_ids))
    and not exists (
      select 1
      from event_instance_target_cohort target
      join cohort_membership membership
        on membership.cohort_id = target.cohort_id
       and membership.person_id = r.person_id
      where target.instance_id = r.instance_id
        and membership.status = 'active'
        and membership.active_range @> now()
    );
$$;
revoke execute on function app_private.reconcile_event_instance_cohort_registrations from public, anonymous;

create or replace function app_private.tg_event_instance_target_cohort__reconcile()
  returns trigger
  language plpgsql
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
begin
  if tg_op = 'DELETE' then
    perform app_private.reconcile_event_instance_cohort_registrations(array[old.instance_id]);
    return old;
  end if;

  perform app_private.reconcile_event_instance_cohort_registrations(
    case when tg_op = 'UPDATE' and old.instance_id <> new.instance_id
      then array[old.instance_id, new.instance_id]
      else array[new.instance_id]
    end
  );
  return new;
end;
$$;
revoke execute on function app_private.tg_event_instance_target_cohort__reconcile()
  from public, anonymous;

select verify_function(
  'app_private.tg_event_instance_target_cohort__reconcile',
  'public.event_instance_target_cohort'
);

drop trigger if exists _500_reconcile_registrations on public.event_instance_target_cohort;
create trigger _500_reconcile_registrations
  after insert or delete or update of instance_id, cohort_id
  on public.event_instance_target_cohort
  for each row execute function app_private.tg_event_instance_target_cohort__reconcile();
--! EndIncluded functions/reconcile_event_instance_cohort_registrations.sql
--! Included functions/detach_event_instance.sql
create or replace function public.detach_event_instance(
  p_instance_id bigint,
  p_new_event_name text default null
)
  returns public.event
  language plpgsql
  security definer
  set search_path = pg_catalog, pg_temp
as $$
declare
  v_tenant_id bigint;
  v_old_event_id bigint;
  v_new_event_id bigint;
  v_result public.event;
begin
  select instance.tenant_id, instance.event_id
  into v_tenant_id, v_old_event_id
  from public.event_instance instance
  where instance.id = p_instance_id
  for update;

  if not found or v_old_event_id is null then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;
  if v_tenant_id <> public.current_tenant_id() then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  if not (
    coalesce(app_private.is_system_admin(public.current_user_id()), false)
    or exists (
      select 1
      from public.current_tenant_administrator administrator
      where administrator.person_id = any (
        coalesce(public.current_person_ids(), '{}'::bigint[])
      )
        and administrator.active_range @> now()
    )
    or (
      exists (
        select 1
        from public.current_tenant_trainer trainer
        where trainer.person_id = any (
          coalesce(public.current_person_ids(), '{}'::bigint[])
        )
          and trainer.active_range @> now()
      )
      and app_private.can_trainer_edit_instance(p_instance_id)
    )
  ) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  perform 1
  from public.event event
  where event.id = v_old_event_id
    and event.tenant_id = v_tenant_id
  for update;

  if not exists (
    select 1
    from public.event_instance sibling
    where sibling.event_id = v_old_event_id
      and sibling.id <> p_instance_id
  ) then
    raise exception 'CANNOT_DETACH_ONLY_INSTANCE' using errcode = '22023';
  end if;

  insert into public.event (
    name, location_text, description, capacity, files_legacy,
    is_locked, is_visible, summary, is_public, enable_notes, tenant_id,
    type, location_id
  )
  select coalesce(p_new_event_name, event.name), event.location_text,
    event.description, event.capacity, event.files_legacy, event.is_locked,
    event.is_visible, event.summary, event.is_public, event.enable_notes,
    event.tenant_id, event.type, event.location_id
  from public.event event
  where event.id = v_old_event_id
  returning id into v_new_event_id;

  -- The exact instance lists are authoritative; these rows only keep the
  -- legacy event-shaped API working during the transition.
  insert into public.event_target_cohort (tenant_id, event_id, cohort_id)
  select v_tenant_id, v_new_event_id, cohort_id
  from (
    select target.cohort_id
    from public.event_instance_target_cohort target
    where target.instance_id = p_instance_id
    union
    select registration.target_cohort_id
    from public.event_instance_registration registration
    where registration.instance_id = p_instance_id
      and registration.parent_registration_id is null
      and registration.target_cohort_id is not null
  ) cohort;

  insert into public.event_trainer (
    tenant_id, event_id, person_id, lessons_offered
  )
  select trainer.tenant_id, v_new_event_id, trainer.person_id,
    trainer.lessons_offered
  from public.event_instance_trainer trainer
  where trainer.instance_id = p_instance_id
  union all
  select trainer.tenant_id, v_new_event_id, trainer.person_id,
    trainer.lessons_offered
  from public.event_trainer trainer
  where trainer.event_id = v_old_event_id
    and not exists (
      select 1
      from public.event_instance_trainer
      where instance_id = p_instance_id
    );

  insert into public.event_registration (
    tenant_id, event_id, target_cohort_id, couple_id, person_id, note
  )
  select exact.tenant_id, v_new_event_id, target.id,
    legacy.couple_id, legacy.person_id, exact.note
  from public.event_instance_registration exact
  join public.event_registration legacy
    on legacy.id = exact.legacy_registration_id
  left join public.event_target_cohort target
    on target.event_id = v_new_event_id
   and target.cohort_id = exact.target_cohort_id
  where exact.instance_id = p_instance_id
    and exact.parent_registration_id is null
    and exact.legacy_registration_id is not null;

  -- The existing bridge and unit keys make this an unambiguous in-place rekey.
  update public.event_instance_registration exact
  set event_id = v_new_event_id,
      legacy_registration_id = replacement.id
  from public.event_registration legacy
  join public.event_registration replacement
    on replacement.event_id = v_new_event_id
   and replacement.person_id is not distinct from legacy.person_id
   and replacement.couple_id is not distinct from legacy.couple_id
  where exact.instance_id = p_instance_id
    and exact.legacy_registration_id = legacy.id
    and legacy.event_id = v_old_event_id;

  update public.event_instance_registration exact
  set event_id = v_new_event_id
  where exact.instance_id = p_instance_id
    and exact.legacy_registration_id is null
    and exact.event_id is distinct from v_new_event_id;

  update public.event_instance instance
  set event_id = v_new_event_id
  where instance.id = p_instance_id;

  update public.event_instance_trainer trainer
  set event_id = v_new_event_id
  where trainer.instance_id = p_instance_id
    and trainer.event_id is distinct from v_new_event_id;

  update public.event_lesson_demand demand
  set event_id = v_new_event_id
  from public.event_instance_registration registration
  where registration.id = demand.registration_id
    and registration.instance_id = p_instance_id
    and demand.event_id is distinct from v_new_event_id;

  select event.* into strict v_result
  from public.event event
  where event.id = v_new_event_id;

  return v_result;
end;
$$;

select verify_function('public.detach_event_instance');
revoke execute on function public.detach_event_instance(bigint, text)
  from public, anonymous, member;
grant execute on function public.detach_event_instance(bigint, text)
  to trainer;
--! EndIncluded functions/detach_event_instance.sql
--! Included functions/upsert_event.sql
drop function if exists upsert_event;

drop type if exists event_type_input;
drop type if exists event_instance_type_input;
drop type if exists event_instance_trainer_type_input;
drop type if exists event_trainer_type_input;

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

CREATE OR REPLACE FUNCTION upsert_event(
  info event_type_input,
  event_instances event_instance_type_input[],
  trainers event_trainer_type_input[]
) RETURNS event LANGUAGE plpgsql AS $$
declare
  instance event_instance_type_input;
  trainer event_trainer_type_input;
  instance_trainer event_instance_trainer_type_input;
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

  return v_event;
end;
$$;

select verify_function('public.upsert_event');

COMMENT ON FUNCTION public.upsert_event is null;
GRANT ALL ON FUNCTION public.upsert_event TO anonymous;
--! EndIncluded functions/upsert_event.sql
