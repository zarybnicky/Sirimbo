alter table event_registration drop constraint if exists event_registration_unique_event_person_couple_key;
alter table event_registration add constraint event_registration_unique_event_person_couple_key unique (event_id, person_id, couple_id);
alter table event_lesson_demand drop constraint if exists eld_unique_registration_trainer_key;
alter table event_lesson_demand add constraint eld_unique_registration_trainer_key unique (registration_id, trainer_id);

drop function if exists event_instances_for_range;
CREATE or replace FUNCTION public.event_instances_for_range(search_range tstzrange, only_mine boolean) RETURNS SETOF public.event_instance LANGUAGE sql STABLE AS $$
  select event_instance.* from event_instance join event on event_id=event.id
  where event.is_visible = true and search_range && range
  and case only_mine
    when false then true
    else exists (select 1 from event_registration where event_id=event.id and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()))) end
  order by range asc;
$$;
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;
comment on function event_instances_for_range is E'@simpleCollections only';

CREATE or replace FUNCTION public.person_primary_phone(p public.person) RETURNS text LANGUAGE sql STABLE AS $$
  select phone from person_phone where person_id = p.id and is_primary = true;
$$;
CREATE or replace FUNCTION public.person_primary_address(p public.person) RETURNS address LANGUAGE sql STABLE AS $$
  select address.* from address join person_address on address_id=address.id where person_id = p.id and is_primary = true;
$$;
CREATE or replace FUNCTION public.person_primary_email(p public.person) RETURNS text LANGUAGE sql STABLE AS $$
  select email from person_email where person_id = p.id and is_primary = true;
$$;
CREATE or replace FUNCTION public.event_remaining_person_spots(e public.event) RETURNS integer LANGUAGE sql STABLE AS $$
  select e.capacity - (select sum(case when couple_id is not null then 2 else 1 end) from event_registration where event_id = e.id);
$$;
CREATE or replace FUNCTION public.event_remaining_lessons(e public.event) RETURNS integer LANGUAGE sql STABLE AS $$
  select (select sum(lessons_offered) from event_trainer where event_id = e.id) - (select sum(lesson_count) from event_registration join event_lesson_demand on registration_id = event_registration.id where event_id = e.id);
$$;
CREATE or replace FUNCTION public.event_trainer_lessons_remaining(e public.event_trainer) RETURNS integer LANGUAGE sql STABLE AS $$
  select e.lessons_offered - (select sum(lesson_count) from event_lesson_demand where trainer_id = e.id);
$$;

GRANT ALL ON FUNCTION public.event_trainer_lessons_remaining TO anonymous;
GRANT ALL ON FUNCTION public.event_remaining_person_spots TO anonymous;
GRANT ALL ON FUNCTION public.event_remaining_lessons TO anonymous;
GRANT ALL ON FUNCTION public.person_primary_phone TO anonymous;
GRANT ALL ON FUNCTION public.person_primary_email TO anonymous;
GRANT ALL ON FUNCTION public.person_primary_address TO anonymous;

drop function if exists register_to_event;
create or replace function register_to_event(event_id bigint, note text, person_id bigint default null, couple_id bigint default null) returns public.event_registration language plpgsql strict as $$
#variable_conflict use_variable
declare
  event event;
  registration event_registration;
begin
  select * into event from event where id = event_id;
  select * into registration from event_registration er
  where er.event_id = event_id and (er.person_id = person_id or er.couple_id = couple_id);

  if event is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if person_id not in (select my_person_ids()) and couple_id not in (select my_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  insert into event_registration (event_id, person_id, couple_id, note)
  values (event_id, person_id, couple_id, note) returning * into registration;
  return registration;
end;
$$;
select verify_function('public.register_to_event');
GRANT ALL ON FUNCTION public.register_to_event TO anonymous;

create or replace function cancel_registration(registration_id bigint) returns void language plpgsql strict as $$
#variable_conflict use_variable
declare
  event event;
  reg event_registration;
begin
  select * into reg from event_registration er where er.id = registration_id;
  select * into event from event where id = reg.event_id;

  if event is null or reg is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked = true or reg.payment_id is not null then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if reg.person_id not in (select my_person_ids()) and reg.couple_id not in (select my_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  delete from event_registration where id = reg.id;
end;
$$;
select verify_function('public.cancel_registration');
GRANT ALL ON FUNCTION public.cancel_registration TO anonymous;

drop function if exists public.set_lesson_demand;
CREATE or replace FUNCTION public.set_lesson_demand(
  registration_id bigint,
  trainer_id bigint,
  lesson_count int
) RETURNS public.event_lesson_demand LANGUAGE plpgsql STRICT AS $$
#variable_conflict use_variable
declare
  event event;
  registration event_registration;
  current_lessons int;
  lesson_demand event_lesson_demand;
begin
  select * into registration from event_registration where id = registration_id;
  select * into event from event where id = registration.event_id;
  select sum(lesson_count)::int into current_lessons from event_lesson_demand eld where eld.registration_id = registration_id;

  if lesson_count = 0 then
    delete from event_lesson_demand eld where registration_id = registration.id and eld.trainer_id = trainer_id;
    return null;
  end if;

  if lesson_count > (current_lessons + event_remaining_lessons(event)) then
    select (current_lessons + event_remaining_lessons(event)) into lesson_count;
  end if;

  INSERT INTO event_lesson_demand (registration_id, trainer_id, lesson_count)
  values (registration.id, trainer_id, lesson_count)
  on conflict on constraint eld_unique_registration_trainer_key do update set lesson_count = lesson_count
  returning * into lesson_demand;

  return lesson_demand;
end;
$$;
select verify_function('public.set_lesson_demand');
GRANT ALL ON FUNCTION public.set_lesson_demand TO anonymous;
