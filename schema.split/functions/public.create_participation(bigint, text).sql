CREATE FUNCTION public.create_participation(event_id bigint, my_notes text) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
#variable_conflict use_column
declare
  event event;
begin
  select * into event from event where id=event_id;
  if event is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  INSERT INTO attendee_user (event_id, user_id, notes)
  values (event_id, current_user_id(), my_notes)
  ON CONFLICT (user_id, event_id) DO UPDATE SET notes = my_notes;
end;
$$;

GRANT ALL ON FUNCTION public.create_participation(event_id bigint, my_notes text) TO member;


