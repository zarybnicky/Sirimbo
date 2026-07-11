--! Previous: sha1:cf3c111cf5631b429111cc918fb3328c4d2d2186
--! Hash: sha1:4bb4fd6dadb86e22e4fd85c5cfa708c9d13c9409

--! split: 1-current.sql
alter table public.event_external_registration
  add column if not exists instance_id bigint;

select app_private.drop_policies('public.event_external_registration');

do $$
begin
  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'public'
      and table_name = 'event_external_registration'
      and column_name = 'event_id'
  ) then
    if exists (
      select 1
      from public.event_external_registration external_registration
      where (
        select count(*)
        from public.event_instance instance
        where instance.event_id = external_registration.event_id
          and instance.parent_id is null
      ) <> 1
    ) then
      raise exception 'Cannot map every external registration to exactly one root event instance';
    end if;

    alter table public.event_external_registration disable trigger _100_timestamps;

    update public.event_external_registration external_registration
    set instance_id = (
      select instance.id
      from public.event_instance instance
      where instance.event_id = external_registration.event_id
        and instance.parent_id is null
    );

    alter table public.event_external_registration enable trigger _100_timestamps;
    alter table public.event_external_registration
      drop constraint if exists event_external_registration_event_id_fkey,
      drop column if exists event_id;
  end if;
end;
$$;

alter table public.event_external_registration
  alter column instance_id set not null;

do $$
begin
  if not exists (
    select 1 from pg_constraint
    where conname = 'event_external_registration_instance_id_fkey'
      and conrelid = 'public.event_external_registration'::regclass
  ) then
    alter table public.event_external_registration
      add constraint event_external_registration_instance_id_fkey
      foreign key (instance_id) references public.event_instance(id)
      on update cascade on delete cascade;
  end if;
end;
$$;

create index if not exists event_external_registration_instance_id_idx
  on public.event_external_registration (instance_id);

grant insert (instance_id) on public.event_external_registration to anonymous;

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
    select coalesce(count(external_registration.id), 0)
    from event_external_registration external_registration
    join event_instance instance on instance.id = external_registration.instance_id
    where instance.event_id = e.id
  );
$$ LANGUAGE sql STABLE security definer;

CREATE or replace FUNCTION event_instance_remaining_person_spots(inst event_instance) RETURNS integer AS $$
  select case
    when inst.capacity is null or inst.capacity <= 0 then null
    else inst.capacity - case inst.capacity_unit
        when 'people' then (
          select count(*)::integer
          from event_instance_registration registration
          where registration.instance_id = inst.id
            and registration.person_id is not null
            and registration.registration_status = 'active'
            and registration.status <> 'cancelled'
        )
        when 'registrations' then (
          select count(*)::integer
          from event_instance_registration registration
          where registration.instance_id = inst.id
            and registration.parent_registration_id is null
            and registration.registration_status = 'active'
        )
      end - (
        select count(*)::integer
        from event_external_registration external_registration
        where external_registration.instance_id = inst.id
      )
  end;
$$ LANGUAGE sql STABLE security definer set search_path = public, pg_catalog, pg_temp;

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
GRANT ALL ON FUNCTION event_instance_remaining_person_spots(inst event_instance) TO anonymous;
GRANT ALL ON FUNCTION event_remaining_lessons(e event) TO anonymous;
--! EndIncluded functions/event_remaining_x.sql
--! Included policies/event_external_registration.sql
select app_private.drop_policies('public.event_external_registration');

CREATE POLICY admin_all ON public.event_external_registration TO administrator USING (true);
CREATE POLICY admin_my ON public.event_external_registration TO member
  USING ((SELECT not instance.is_locked FROM event_instance instance WHERE instance_id = instance.id) AND (created_by = public.current_user_id()));
CREATE POLICY register_public ON public.event_external_registration FOR INSERT TO anonymous WITH CHECK ((SELECT instance.is_public FROM event_instance instance WHERE instance_id = instance.id));
CREATE POLICY trainer_same_tenant ON public.event_external_registration TO trainer USING (app_private.can_trainer_edit_instance(instance_id)) WITH CHECK (true);
CREATE POLICY view_visible_instance ON public.event_external_registration FOR SELECT TO member USING (instance_id = any (SELECT id from event_instance));
--! EndIncluded policies/event_external_registration.sql

-- Remove the unused legacy payment relation only after its dependent function is updated.
--! Included functions/calculate_transaction_effective_date.sql
create or replace function app_private.calculate_transaction_effective_date(t transaction)
  returns timestamptz language sql volatile
begin atomic
  select coalesce(
    (select since from payment join event_instance on event_instance_id = event_instance.id where t.payment_id = payment.id),
    (select due_at from payment where t.payment_id = payment.id),
    t.created_at
  );
end;
--! EndIncluded functions/calculate_transaction_effective_date.sql

do $$
begin
  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'public'
      and table_name = 'payment'
      and column_name = 'event_registration_id'
  ) then
    if exists (
      select 1 from public.payment where event_registration_id is not null
    ) then
      raise exception 'Cannot remove payment.event_registration_id while it contains data';
    end if;

    alter table public.payment
      drop constraint if exists payment_event_registration_id_fkey,
      drop column if exists event_registration_id;
  end if;
end;
$$;
