CREATE FUNCTION public.create_participation_external(event_id bigint, first_name text, last_name text, guardian_name text, email text, phone text, notes text, birth_number text) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  event akce;
begin
  select * into event from akce where a_id=event_id;
  if event is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;

  if event.a_lock then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  INSERT INTO attendee_external (event_id, first_name, last_name, guardian_name, email, phone, notes, birth_number)
  values (event_id, first_name, last_name, guardian_name, email, phone, notes, birth_number);
end;
$$;

GRANT ALL ON FUNCTION public.create_participation_external(event_id bigint, first_name text, last_name text, guardian_name text, email text, phone text, notes text, birth_number text) TO anonymous;


