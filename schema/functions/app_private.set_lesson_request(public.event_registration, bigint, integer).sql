CREATE FUNCTION app_private.set_lesson_request(registration public.event_registration, trainer_id bigint, lesson_count integer) RETURNS public.event_lesson_request
    LANGUAGE plpgsql STRICT
    AS $_$
declare
  trainer event_trainer;
  lesson_request event_lesson_request;
  other_lessons bigint;
begin
  select * into trainer
  from event_trainer
  where id = $2 and event_id = registration.event_id and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'TRAINER_NOT_FOUND' using errcode = '28000';
  end if;

  if $3 = 0 then
    delete from event_lesson_request eld
    where eld.registration_id = registration.id and eld.trainer_id = $2;

    return null;
  end if;

  if trainer.lessons_offered = 0 then
    raise exception 'LESSONS_NOT_OFFERED' using errcode = '28000';
  end if;

  if trainer.lessons_offered is not null then
    select coalesce(sum(eld.lesson_count), 0) into other_lessons
    from event_lesson_request eld
    join event_registration other_registration
      on other_registration.id = eld.registration_id
     and other_registration.registration_status = 'active'
    where eld.trainer_id = $2 and eld.registration_id <> registration.id;

    if $3 > trainer.lessons_offered - other_lessons then
      raise exception 'LESSON_LIMIT_EXCEEDED' using errcode = '22023';
    end if;
  end if;

  insert into event_lesson_request (registration_id, trainer_id, lesson_count)
  values (registration.id, $2, $3)
  on conflict on constraint event_lesson_request_registration_id_trainer_id_key do update set lesson_count = $3
  returning * into lesson_request;

  return lesson_request;
end;
$_$;
