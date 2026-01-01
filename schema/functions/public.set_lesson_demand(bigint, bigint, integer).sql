CREATE FUNCTION public.set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer) RETURNS public.event_lesson_demand
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $_$
declare
  v_event event;
  registration event_registration;
  lesson_demand event_lesson_demand;
begin
  select * into registration from event_registration where id = $1;
  select * into v_event from event where id = registration.event_id;

  if v_event is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if v_event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  if $3 = 0 then
    delete from event_lesson_demand eld where eld.registration_id = registration.id and eld.trainer_id = $2;
    return null;
  end if;

  INSERT INTO event_lesson_demand (registration_id, trainer_id, lesson_count)
  values ($1, $2, $3)
  on conflict on constraint eld_unique_registration_trainer_key do update set lesson_count = $3
  returning * into lesson_demand;

  return lesson_demand;
end;
$_$;

GRANT ALL ON FUNCTION public.set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer) TO anonymous;
