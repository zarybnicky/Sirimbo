create or replace function edit_registration(registration_id bigint, note text) returns event_registration language plpgsql strict as $$
declare
  v_event event;
  reg event_registration;
begin
  select * into reg from event_registration where id = registration_id;
  select * into v_event from event where id = reg.event_id;

  if v_event is null or reg is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if v_event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if reg.person_id <> all (current_person_ids()) and reg.couple_id <> all (current_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  update event_registration set note=$2 where id = reg.id returning * into reg;
  return reg;
end;
$$;
select verify_function('public.edit_registration');
GRANT ALL ON FUNCTION public.edit_registration TO anonymous;
