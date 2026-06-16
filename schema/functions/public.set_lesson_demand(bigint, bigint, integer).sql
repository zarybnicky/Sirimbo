CREATE FUNCTION public.set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer) RETURNS public.event_lesson_demand
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $_$
declare
  v_event event;
  v_trainer event_trainer;
  registration event_registration;
  lesson_demand event_lesson_demand;
  other_lessons bigint;
begin
  select * into registration from event_registration where id = $1;
  select * into v_event from event where id = registration.event_id;
  select * into v_trainer from event_trainer where id = $2 and event_id = registration.event_id;

  if v_event is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if v_event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if v_trainer is null then
    raise exception 'TRAINER_NOT_FOUND' using errcode = '28000';
  end if;

  if $3 = 0 then
    delete from event_lesson_demand eld where eld.registration_id = registration.id and eld.trainer_id = $2;
    return null;
  end if;
  if v_trainer.lessons_offered = 0 then
    raise exception 'LESSONS_NOT_OFFERED' using errcode = '28000';
  end if;
  if v_trainer.lessons_offered is not null then
    select coalesce(sum(eld.lesson_count), 0) into other_lessons
    from event_lesson_demand eld
    where eld.trainer_id = $2 and eld.registration_id <> registration.id;

    if $3 > v_trainer.lessons_offered - other_lessons then
      raise exception 'LESSON_LIMIT_EXCEEDED' using errcode = '22023';
    end if;
  end if;

  INSERT INTO event_lesson_demand (registration_id, trainer_id, lesson_count)
  values ($1, $2, $3)
  on conflict on constraint eld_unique_registration_trainer_key do update set lesson_count = $3
  returning * into lesson_demand;

  return lesson_demand;
end;
$_$;

GRANT ALL ON FUNCTION public.set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer) TO anonymous;
