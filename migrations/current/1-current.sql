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
