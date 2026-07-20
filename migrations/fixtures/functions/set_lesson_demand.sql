drop function if exists set_lesson_demand;

CREATE FUNCTION set_lesson_demand(instance_registration_id bigint, instance_trainer_id bigint, lesson_count integer) RETURNS event_lesson_demand
  LANGUAGE plpgsql
  SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
declare
  registration event_instance_registration;
  instance event_instance;
  trainer event_instance_trainer;
  lesson_demand event_lesson_demand;
  is_manager boolean;
  is_self boolean;
  other_lessons bigint;
begin
  select * into registration
  from event_instance_registration
  where id = $1 and parent_registration_id is null;

  if not found then
    raise exception 'REGISTRATION_NOT_FOUND' using errcode = '28000';
  end if;

  select * into instance from event_instance where id = registration.instance_id;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '28000';
  end if;

  is_self := (registration.person_id is not null and registration.person_id = any(current_person_ids()))
    or (registration.couple_id is not null and registration.couple_id = any(current_couple_ids()));
  is_manager := app_private.is_system_admin(current_user_id())
    or exists (select 1 from current_tenant_administrator where person_id = any(current_person_ids()))
    or and app_private.can_trainer_edit_instance(instance.id);

  if not is_self and not is_manager then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;
  if instance.is_locked and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  select * into trainer
  from event_instance_trainer
  where id = $2 and instance_id = instance.id
  for update;

  if not found then
    raise exception 'TRAINER_NOT_FOUND' using errcode = '28000';
  end if;

  if $3 = 0 then
    delete from event_lesson_demand eld where eld.registration_id = registration.id and eld.trainer_id = $2;
    return null;
  end if;
  if trainer.lessons_offered = 0 then
    raise exception 'LESSONS_NOT_OFFERED' using errcode = '28000';
  end if;
  if trainer.lessons_offered is not null then
    select coalesce(sum(eld.lesson_count), 0) into other_lessons
    from event_lesson_demand eld
    join event_instance_registration other_registration
      on other_registration.id = eld.registration_id
     and other_registration.registration_status = 'active'
    where eld.trainer_id = $2 and eld.registration_id <> registration.id;

    if $3 > trainer.lessons_offered - other_lessons then
      raise exception 'LESSON_LIMIT_EXCEEDED' using errcode = '22023';
    end if;
  end if;

  INSERT INTO event_lesson_demand (registration_id, trainer_id, lesson_count)
  values (registration.id, $2, $3)
  on conflict on constraint eld_unique_registration_trainer_key do update set lesson_count = $3
  returning * into lesson_demand;

  return lesson_demand;
end;
$$;

GRANT ALL ON FUNCTION set_lesson_demand TO anonymous;
