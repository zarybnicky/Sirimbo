create or replace function register_to_event(inout registration event_registration, lessons event_lesson_demand[]) language plpgsql strict security definer as $$
declare
  event event;
  demand event_lesson_demand;
begin
  select * into event from event where id = registration.event_id;

  if event is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if registration.person_id <> all (current_person_ids()) and registration.couple_id <> all (current_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  insert into event_registration (event_id, person_id, couple_id, note) select registration.event_id, registration.person_id, registration.couple_id, registration.note returning * into registration;
  foreach demand in array lessons loop
    perform set_lesson_demand(registration.id, demand.trainer_id, demand.lesson_count);
  end loop;
end;
$$;

comment on function register_to_event is E'@arg0variant create
@arg1variant patch';

select verify_function('public.register_to_event');

GRANT ALL ON FUNCTION public.register_to_event TO anonymous;
