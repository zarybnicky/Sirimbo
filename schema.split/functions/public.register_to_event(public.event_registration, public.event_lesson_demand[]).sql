CREATE FUNCTION public.register_to_event(INOUT registration public.event_registration, lessons public.event_lesson_demand[]) RETURNS public.event_registration
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
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
  if registration.person_id not in (select my_person_ids()) and registration.couple_id not in (select my_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  insert into event_registration (event_id, person_id, couple_id, note) select registration.event_id, registration.person_id, registration.couple_id, registration.note returning * into registration;
  foreach demand in array lessons loop
    perform set_lesson_demand(registration.id, demand.trainer_id, demand.lesson_count);
  end loop;
end;
$$;

COMMENT ON FUNCTION public.register_to_event(INOUT registration public.event_registration, lessons public.event_lesson_demand[]) IS '@arg0variant create
@arg1variant patch';

GRANT ALL ON FUNCTION public.register_to_event(INOUT registration public.event_registration, lessons public.event_lesson_demand[]) TO anonymous;


