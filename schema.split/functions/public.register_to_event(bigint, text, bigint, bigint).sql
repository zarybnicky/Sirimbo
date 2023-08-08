CREATE FUNCTION public.register_to_event(event_id bigint, note text, person_id bigint DEFAULT NULL::bigint, couple_id bigint DEFAULT NULL::bigint) RETURNS public.event_registration
    LANGUAGE plpgsql STRICT
    AS $$
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

GRANT ALL ON FUNCTION public.register_to_event(event_id bigint, note text, person_id bigint, couple_id bigint) TO anonymous;


