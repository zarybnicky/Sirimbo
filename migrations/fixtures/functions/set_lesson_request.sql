drop function if exists set_lesson_demand;
drop function if exists set_lesson_request;
drop function if exists app_private.set_lesson_demand;
drop function if exists app_private.set_lesson_request;

create function app_private.set_lesson_request(
  registration event_instance_registration,
  trainer_id bigint,
  lesson_count integer
) returns event_lesson_demand
language plpgsql strict
as $$
declare
  trainer event_instance_trainer;
  lesson_demand event_lesson_demand;
  other_lessons bigint;
begin
  select * into trainer
  from event_instance_trainer
  where id = $2 and instance_id = registration.instance_id and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'TRAINER_NOT_FOUND' using errcode = '28000';
  end if;

  if $3 = 0 then
    delete from event_lesson_demand eld
    where eld.registration_id = registration.id and eld.trainer_id = $2;

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

  insert into event_lesson_demand (registration_id, trainer_id, lesson_count)
  values (registration.id, $2, $3)
  on conflict on constraint eld_unique_registration_trainer_key do update set lesson_count = $3
  returning * into lesson_demand;

  return lesson_demand;
end;
$$;

create function set_lesson_request(
  registration_id bigint,
  trainer_id bigint,
  lesson_count integer
) returns event_lesson_demand
language plpgsql strict security definer
set search_path = pg_catalog, public, pg_temp
as $$
declare
  registration event_instance_registration;
  instance event_instance;
  is_manager boolean;
  is_self boolean;
begin
  select * into registration
  from event_instance_registration
  where id = $1;

  if not found then
    raise exception 'REGISTRATION_NOT_FOUND' using errcode = '28000';
  end if;

  select * into instance
  from event_instance
  where id = registration.instance_id and tenant_id = current_tenant_id();

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '28000';
  end if;

  is_self := registration.person_id = any(current_person_ids())
    or registration.couple_id = any(current_couple_ids());
  is_manager := pg_has_role(
    coalesce(nullif(current_setting('role'), 'none'), session_user), 'administrator', 'member'
  ) or (
    pg_has_role(
      coalesce(nullif(current_setting('role'), 'none'), session_user), 'trainer', 'member'
    ) and app_private.can_trainer_edit_instance(instance.id)
  );

  if not coalesce(is_self, false) and not coalesce(is_manager, false) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  if instance.is_locked and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  return app_private.set_lesson_request(registration, $2, $3);
end;
$$;

grant all on function set_lesson_request to anonymous;

create function set_lesson_demand(
  instance_registration_id bigint,
  instance_trainer_id bigint,
  lesson_count integer
) returns event_lesson_demand
language sql strict
as $$
  select public.set_lesson_request($1, $2, $3);
$$;

comment on function set_lesson_demand is E'@deprecated Use setLessonRequest.\n@resultFieldName eventLessonDemand';
grant execute on function set_lesson_demand to anonymous;
