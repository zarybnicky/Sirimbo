CREATE FUNCTION public.cancel_participation(event_id bigint) RETURNS void
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

  delete from akce_item where ai_id_rodic=event.a_id and ai_user=current_user_id();
end;
$$;

GRANT ALL ON FUNCTION public.cancel_participation(event_id bigint) TO member;


