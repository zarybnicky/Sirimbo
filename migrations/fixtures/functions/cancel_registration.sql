create or replace function cancel_registration(registration_id bigint) returns void language plpgsql as $$
declare
  v_event event;
  v_reg event_registration;
begin
  select * into v_reg from event_registration er where er.id = registration_id;
  select * into v_event from event where id = v_reg.event_id;

  if v_event is null or v_reg is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if v_event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if v_reg.person_id <> all (current_person_ids()) and v_reg.couple_id <> all (current_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  delete from event_registration where id = v_reg.id;
end;
$$;

select verify_function('public.cancel_registration');

GRANT ALL ON FUNCTION public.cancel_registration TO anonymous;
