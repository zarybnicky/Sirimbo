drop function if exists upsert_event;

drop type if exists event_type_input;
drop type if exists event_instance_type_input;
drop type if exists event_instance_trainer_type_input;
drop type if exists event_trainer_type_input;
drop type if exists event_target_cohort_type_input;
drop type if exists event_registration_type_input;

CREATE TYPE public.event_type_input AS (
  id bigint,
  name text,
  summary text,
  description text,
  type public.event_type,
  location_id bigint,
  location_text text,
  capacity integer,
  is_visible boolean,
  is_public boolean,
  is_locked boolean,
  enable_notes boolean,
  payment_type event_payment_type
);

CREATE TYPE public.event_instance_trainer_type_input AS (
  id bigint,
  person_id bigint
);

CREATE TYPE public.event_instance_type_input AS (
  id bigint,
  since timestamp with time zone,
  until timestamp with time zone,
  is_cancelled boolean,
  trainers event_instance_trainer_type_input[]
);

CREATE TYPE public.event_trainer_type_input AS (
  id bigint,
  person_id bigint,
  lessons_offered integer
);

CREATE TYPE public.event_target_cohort_type_input AS (
  id bigint,
  cohort_id bigint
);

CREATE TYPE public.event_registration_type_input AS (
  id bigint,
  person_id bigint,
  couple_id bigint
);

CREATE OR REPLACE FUNCTION upsert_event(
  info event_type_input,
  instances event_instance_type_input[],
  trainers event_trainer_type_input[],
  cohorts event_target_cohort_type_input[],
  registrations event_registration_type_input[]
) RETURNS event LANGUAGE plpgsql AS $$
declare
  instance event_instance_type_input;
  trainer event_trainer_type_input;
  instance_trainer event_instance_trainer_type_input;
  cohort event_target_cohort_type_input;
  registration event_registration_type_input;
  v_event event;
  v_instance_id bigint;
begin
  if info.id is null then

    insert into event (
      name,
      summary,
      description,
      type,
      location_id,
      location_text,
      capacity,
      is_visible,
      is_public,
      is_locked,
      enable_notes
    )
    values (
      info.name,
      info.summary,
      info.description,
      info.type,
      info.location_id,
      info.location_text,
      info.capacity,
      info.is_visible,
      info.is_public,
      info.is_locked,
      info.enable_notes
    )
    returning * into v_event;
  else
    update event set
      name=info.name,
      summary=info.summary,
      description=info.description,
      type=info.type,
      location_id=info.location_id,
      location_text=info.location_text,
      capacity=info.capacity,
      is_visible=info.is_visible,
      is_public=info.is_public,
      is_locked=info.is_locked,
      enable_notes=info.enable_notes
    where id=info.id
    returning * into v_event;

    if not found then
      raise exception 'event % not found', info.id;
    end if;
  end if;

  foreach instance in array coalesce(instances, '{}'::event_instance_type_input[]) loop
    if instance.id is null then
      insert into event_instance (event_id, since, until, is_cancelled)
      values (v_event.id, instance.since, instance.until, instance.is_cancelled)
      returning id into v_instance_id;
    elsif instance.since is null and instance.until is null then
      delete from event_instance where id=instance.id;
      v_instance_id := null;
    else
      update event_instance
      set since=instance.since, until=instance.until, is_cancelled=instance.is_cancelled
      where id=instance.id
      returning id into v_instance_id;
    end if;

    if v_instance_id is not null then
      foreach instance_trainer in array coalesce(instance.trainers, '{}'::event_instance_trainer_type_input[]) loop
        if instance_trainer.id is null then
          insert into event_instance_trainer (instance_id, person_id)
          values (v_instance_id, instance_trainer.person_id)
          on conflict (instance_id, person_id) do nothing;
        elsif instance_trainer.person_id is null then
          delete from event_instance_trainer where id=instance_trainer.id;
        end if;
      end loop;
    end if;
  end loop;

  foreach trainer in array coalesce(trainers, '{}'::event_trainer_type_input[]) loop
    if trainer.id is null then
      insert into event_trainer (event_id, person_id, lessons_offered)
      values (v_event.id, trainer.person_id, coalesce(trainer.lessons_offered, 0))
      on conflict (event_id, person_id) do update
        set lessons_offered = coalesce(trainer.lessons_offered, 0);
    elsif trainer.person_id is null then
      delete from event_trainer where id=trainer.id;
    else
      update event_trainer set lessons_offered=trainer.lessons_offered where id=trainer.id;
    end if;
  end loop;

  foreach cohort in array coalesce(cohorts, '{}'::event_target_cohort_type_input[]) loop
    if cohort.id is null then
      insert into event_target_cohort (event_id, cohort_id)
      values (v_event.id, cohort.cohort_id)
      on conflict (event_id, cohort_id) do nothing;
    elsif cohort.cohort_id is null then
      delete from event_target_cohort where id=cohort.id;
    end if;
  end loop;

  foreach registration in array coalesce(registrations, '{}'::event_registration_type_input[]) loop
    if registration.id is null then
      insert into event_registration (event_id, person_id, couple_id)
      values (v_event.id, registration.person_id, registration.couple_id)
      on conflict (event_id, person_id, couple_id) do nothing;
    elsif registration.person_id is null and registration.couple_id is null then
      delete from event_registration where id=registration.id;
    else
      update event_registration
      set person_id=registration.person_id, couple_id=registration.couple_id
      where id=registration.id;
    end if;
  end loop;

  return v_event;
end;
$$;

select verify_function('public.upsert_event');

COMMENT ON FUNCTION public.upsert_event is null;
GRANT ALL ON FUNCTION public.upsert_event TO anonymous;
