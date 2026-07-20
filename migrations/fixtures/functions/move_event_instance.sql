create or replace function move_event_instance(
  id bigint,
  since timestamptz,
  until timestamptz,
  trainer_person_id bigint default null,
  location_id bigint default null,
  location_text text default null
) returns event_instance
  language plpgsql
as $$
declare
  inst event_instance;
begin
  update event_instance
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
    update event_instance_trainer
    set person_id = trainer_person_id
    where instance_id = inst.id
      and 1 = (select count(*) from event_instance_trainer where instance_id = inst.id);

    if not found and not exists (select 1 from event_instance_trainer where instance_id = inst.id)
    then
      insert into event_instance_trainer (instance_id, person_id)
      values (inst.id, trainer_person_id);
    end if;
  end if;

  return (select instance from event_instance instance where instance.id = inst.id);
end;
$$;

grant execute on function move_event_instance to anonymous;
