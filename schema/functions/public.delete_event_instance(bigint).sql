CREATE FUNCTION public.delete_event_instance(id bigint, OUT deleted public.event_instance) RETURNS public.event_instance
    LANGUAGE plpgsql STRICT
    AS $_$
declare
  inst event_instance;
begin
  select * into inst from event_instance where event_instance.id = $1;
  if inst is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  delete from event_instance where event_instance.id=inst.id returning * into deleted;
  if (select count(*) < 2 from event_instance where event_instance.event_id = inst.event_id) then
    delete from event where event.id=inst.event_id;
  end if;
end
$_$;
