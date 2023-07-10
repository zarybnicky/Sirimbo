CREATE FUNCTION public.cancel_participation(event_id bigint) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
#variable_conflict use_variable
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

  delete from attendee_user where attendee_user.event_id=event_id and user_id=current_user_id();
end;
$$;

GRANT ALL ON FUNCTION public.cancel_participation(event_id bigint) TO member;


