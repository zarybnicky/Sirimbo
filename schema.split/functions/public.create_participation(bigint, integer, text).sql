CREATE FUNCTION public.create_participation(event_id bigint, year_of_birth integer, my_notes text) RETURNS void
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

  INSERT INTO akce_item
    (ai_id_rodic, ai_user, ai_rok_narozeni, notes)
  values
    (event_id, current_user_id(), year_of_birth, my_notes)
  ON CONFLICT (ai_id_rodic, ai_user)
  DO UPDATE SET notes = my_notes, ai_rok_narozeni=year_of_birth;
end;
$$;

GRANT ALL ON FUNCTION public.create_participation(event_id bigint, year_of_birth integer, my_notes text) TO member;


