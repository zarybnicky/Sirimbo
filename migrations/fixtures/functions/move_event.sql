create or replace function move_event(
  id bigint,
  since timestamptz,
  until timestamptz,
  trainer_person_id bigint default null,
  location_id bigint default null,
  location_text text default null
) returns event
  language plpgsql
as $$
declare
  inst event;
begin
  update event
  set since = move_event.since,
      until = move_event.until,
      location_id = case
        when move_event.location_id is not null then move_event.location_id
        when move_event.location_text is not null then null
        else event.location_id
      end,
      location_text = case
        when move_event.location_id is not null then ''
        else coalesce(move_event.location_text, event.location_text)
      end
  where event.id = move_event.id
  returning * into inst;

  if not found then
    raise exception 'Event instance % not found', move_event.id;
  end if;

  if trainer_person_id is not null then
    update event_trainer
    set person_id = trainer_person_id
    where event_id = inst.id
      and 1 = (select count(*) from event_trainer where event_id = inst.id);

    if not found and not exists (select 1 from event_trainer where event_id = inst.id)
    then
      insert into event_trainer (event_id, person_id)
      values (inst.id, trainer_person_id);
    end if;
  end if;

  return (select instance from event instance where instance.id = inst.id);
end;
$$;

grant execute on function move_event to anonymous;
