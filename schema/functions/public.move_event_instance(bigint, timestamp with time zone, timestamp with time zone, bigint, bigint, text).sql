CREATE FUNCTION public.move_event_instance(id bigint, since timestamp with time zone, until timestamp with time zone, trainer_person_id bigint DEFAULT NULL::bigint, location_id bigint DEFAULT NULL::bigint, location_text text DEFAULT NULL::text) RETURNS public.event_instance
    LANGUAGE plpgsql
    AS $$
declare
  inst public.event_instance;
begin
  update public.event_instance
  set since = move_event_instance.since,
      until = move_event_instance.until,
      location_id = case
        when move_event_instance.location_id is not null then move_event_instance.location_id
        when move_event_instance.location_text is not null then null
        else event_instance.location_id
      end,
      location_text = case
        when move_event_instance.location_id is not null then ''
        else coalesce(move_event_instance.location_text, event_instance.location_text)
      end
  where event_instance.id = move_event_instance.id
  returning * into inst;

  if not found then
    raise exception 'Event instance % not found', move_event_instance.id;
  end if;

  if trainer_person_id is not null then
    update public.event_instance_trainer
    set person_id = trainer_person_id
    where instance_id = inst.id
      and 1 = (select count(*) from public.event_instance_trainer where instance_id = inst.id);

    if not found
      and not exists (select 1 from public.event_instance_trainer where instance_id = inst.id)
      and (inst.event_id is null or (select count(*) from public.event_trainer where event_id = inst.event_id) <= 1)
    then
      insert into public.event_instance_trainer (instance_id, person_id)
      values (inst.id, trainer_person_id);
    end if;
  end if;

  return (select instance from public.event_instance instance where instance.id = inst.id);
end;
$$;

GRANT ALL ON FUNCTION public.move_event_instance(id bigint, since timestamp with time zone, until timestamp with time zone, trainer_person_id bigint, location_id bigint, location_text text) TO anonymous;
