-- Write your migration here

do $$
begin
  if not exists (select * from pg_type where typcategory='E' and typname = 'event_payment_type') then
    create type event_payment_type as enum (
      'upfront',
      'after_instance',
      'none'
    );
  end if;
end
$$;

alter table event drop column if exists registration_price;
alter table event add column if not exists payment_type event_payment_type not null default 'none';
alter table event add column if not exists use_default_price boolean not null default true;
alter table event add column if not exists member_price price null default null;
alter table event add column if not exists guest_price price null default null;

alter table tenant_trainer drop column if exists default_price;
alter table tenant_trainer add column if not exists member_price_45min price null default null;
alter table tenant_trainer add column if not exists guest_price_45min price null default null;


CREATE or replace FUNCTION public.register_to_event_many(registrations public.register_to_event_type[]) RETURNS SETOF public.event_registration
    LANGUAGE plpgsql STRICT security definer
    AS $$
declare
  event event;
  created_ids bigint[] := array[]::bigint[];
  registration register_to_event_type;
  created event_registration;
  demand event_lesson_demand;
begin
  foreach registration in array registrations loop
    select * into event from event where id = registration.event_id;

    if event is null then
      raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
    end if;
    if event.is_locked = true then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;
    if registration.person_id not in (select my_person_ids()) and registration.couple_id not in (select my_couple_ids()) then
      raise exception 'ACCESS_DENIED' using errcode = '42501';
    end if;

    insert into event_registration (event_id, person_id, couple_id, note)
    values (registration.event_id, registration.person_id, registration.couple_id, registration.note)
    returning * into created;
    created_ids := created_ids || created.id;
    foreach demand in array registration.lessons loop
      perform set_lesson_demand(created.id, demand.trainer_id, demand.lesson_count);
    end loop;
  end loop;
  return query select * from event_registration where id = any (created_ids);
end;
$$;

alter table tenant_location alter column description type text;
alter table tenant_location alter column description set default '';
alter table tenant_location alter column description set not null;


CREATE or replace FUNCTION public.upsert_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]) RETURNS public.event LANGUAGE plpgsql AS $$
declare
  instance event_instance;
  trainer event_trainer;
  cohort event_target_cohort;
  registration event_registration;
begin
  if info.id is not null then
    update event set name=info.name, summary=info.summary, description=info.description, type=info.type, location_id=info.location_id, location_text=info.location_text, capacity=info.capacity, is_visible=info.is_visible, is_public=info.is_public, is_locked=info.is_locked, enable_notes=info.enable_notes where id=info.id;
  else
    insert into event (name, summary, description, type, location_id, location_text, capacity, is_visible, is_public, is_locked, enable_notes)
    values (info.name, info.summary, info.description, info.type, info.location_id, info.location_text, info.capacity, info.is_visible, info.is_public, info.is_locked, info.enable_notes)
    returning * into info;
  end if;

  foreach instance in array instances loop
    if instance.id is not null then
      if instance.since is null and instance.until is null then
        delete from event_instance where id=instance.id;
      else
        update event_instance set since=instance.since, until=instance.until where id=instance.id;
      end if;
    else
      insert into event_instance (event_id, since, until) values (info.id, instance.since, instance.until);
    end if;
  end loop;

  foreach trainer in array trainers loop
    if trainer.id is not null then
      if trainer.person_id is null then
        delete from event_trainer where id=trainer.id;
      else
        update event_trainer set lessons_offered=trainer.lessons_offered where id=trainer.id;
      end if;
    else
      insert into event_trainer (event_id, person_id, lessons_offered) values (info.id, trainer.person_id, coalesce(trainer.lessons_offered, 0));
    end if;
  end loop;

  foreach cohort in array cohorts loop
    if cohort.id is not null then
      if cohort.cohort_id is null then
        delete from event_target_cohort where id=cohort.id;
      end if;
    else
      insert into event_target_cohort (event_id, cohort_id) values (info.id, cohort.cohort_id);
    end if;
  end loop;

  foreach registration in array registrations loop
    if registration.id is not null then
      if registration.person_id is null and registration.couple_id is null then
        delete from event_registration where id=registration.id;
      else
        update event_registration
        set person_id=registration.person_id, couple_id=registration.couple_id, is_confirmed=registration.is_confirmed
        where id=registration.id;
      end if;
    else
      insert into event_registration (event_id, person_id, couple_id, is_confirmed)
      values (info.id, registration.person_id, registration.couple_id, registration.is_confirmed);
    end if;
  end loop;
end;
$$;


-- cohort subscriptions: CRON payment+transactions, paused (over summer)
-- price model = main + rows of deductions
--   -> fixed per minute
--   -> fixed
--   -> percentage
--   -> per tag = conditional
-- accounting period 1.9. - 30.6.
-- payment group -> "členské příspěvky KS1 2024", "lekce", payment model (fees), symbols?
-- payment -> debtor, recipient, amount, invoice_id, transaction_id, subscription_id, status, tags (array), symboly (VS, SS, KS),
-- payment status -> tentative/hold, pending/missing funds, complete
-- invoice -> JSON, source of PDFs?
-- transaction -> date, confirmation (PDF)
-- posting -> account, amount (+/-), currency, exchange rate
-- account = 1 for person, many for tenant (multi account import for Kometa)
-- event: cancelled, registration period
-- account_balances (materialized) view
--
-- členský poplatek skupiny = pravidelně vypisované platby pro všechny účastníky, payment group???
-- ručně vypsané platby
-- poplatek za událost (při registraci, při proběhnutí)
-- auto-resolution of payments (when there's sufficient balance)
-- manual resolution ("paid in cash")
-- generate QR for "credit payment"
--
-- intra account transactions (multi-person accounts)
--
