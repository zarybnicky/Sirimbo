CREATE FUNCTION public.move_event_instance(id bigint, since timestamp with time zone, until timestamp with time zone, trainer_person_id bigint, location_id bigint, location_text text) RETURNS public.event_instance
    LANGUAGE plpgsql
    AS $$
declare
  inst event_instance;
begin
  select * from event_instance into inst where event_instance.id = move_event_instance.id;

  if location_id is not null then
    update event set location_id = move_event_instance.location_id where event.id = inst.event_id;
  end if;
  if location_text is not null then
    update event set location_text = move_event_instance.location_text where event.id = inst.event_id;
  end if;

  if trainer_person_id is not null then
    if (select count(*) = 1 from event_instance_trainer where instance_id = inst.id) then
      update event_instance_trainer set person_id = trainer_person_id where instance_id = inst.id;
    elsif (select count(*) = 1 from event_trainer where event_id = inst.event_id) then
      update event_trainer set person_id = trainer_person_id where event_id = inst.event_id;
    end if;
  end if;

  update event_instance set since=move_event_instance.since, until=move_event_instance.until where event_instance.id=inst.id
  returning * into inst;
  return inst;
end;
$$;

GRANT ALL ON FUNCTION public.move_event_instance(id bigint, since timestamp with time zone, until timestamp with time zone, trainer_person_id bigint, location_id bigint, location_text text) TO anonymous;


