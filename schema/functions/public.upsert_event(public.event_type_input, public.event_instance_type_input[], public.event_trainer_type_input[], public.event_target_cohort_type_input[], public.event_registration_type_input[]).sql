CREATE FUNCTION public.upsert_event(info public.event_type_input, instances public.event_instance_type_input[], trainers public.event_trainer_type_input[], cohorts public.event_target_cohort_type_input[], registrations public.event_registration_type_input[]) RETURNS public.event
    LANGUAGE plpgsql
    AS $$
declare
  instance event_instance_type_input;
  trainer event_trainer_type_input;
  instance_trainer event_instance_trainer_type_input;
  cohort event_target_cohort_type_input;
  registration event_registration_type_input;
  v_event event;
  v_instance event_instance;
begin
  if info.id is not null then
    update event set
      name=info.name,
      summary=info.summary,
      description=info.description,
      description_member=info.description_member,
      type=info.type,
      location_id=info.location_id,
      location_text=info.location_text,
      capacity=info.capacity,
      is_visible=info.is_visible,
      is_public=info.is_public,
      is_locked=info.is_locked,
      enable_notes=info.enable_notes,
      payment_type=info.payment_type,
      guest_price=info.guest_price,
      member_price=info.member_price
    where id=info.id
    returning * into v_event;
  else
    insert into event (
      name,
      summary,
      description,
      description_member,
      type,
      location_id,
      location_text,
      capacity,
      is_visible,
      is_public,
      is_locked,
      enable_notes,
      payment_type,
      guest_price,
      member_price
    )
    values (
      info.name,
      info.summary,
      info.description,
      info.description_member,
      info.type,
      info.location_id,
      info.location_text,
      info.capacity,
      info.is_visible,
      info.is_public,
      info.is_locked,
      info.enable_notes,
      info.payment_type,
      info.guest_price,
      info.member_price
    )
    returning * into v_event;
  end if;

  foreach instance in array instances loop
    if instance.id is not null then
      if instance.since is null and instance.until is null then
        delete from event_instance where id=instance.id;
        v_instance.id := null;
      else
        update event_instance
        set since=instance.since, until=instance.until, is_cancelled=instance.is_cancelled
        where id=instance.id
        returning * into v_instance;
      end if;
    else
      insert into event_instance (event_id, since, until, is_cancelled)
      values (v_event.id, instance.since, instance.until, instance.is_cancelled)
      returning * into v_instance;
    end if;

    if v_instance.id is not null then
      foreach instance_trainer in array instance.trainers loop
        if instance_trainer.id is not null then
          if instance_trainer.person_id is null then
            delete from event_instance_trainer where id=instance_trainer.id;
          end if;
        else
          insert into event_instance_trainer (instance_id, person_id)
          values (v_instance.id, instance_trainer.person_id);
        end if;
      end loop;
    end if;
  end loop;

  foreach trainer in array trainers loop
    if trainer.id is not null then
      if trainer.person_id is null then
        delete from event_trainer where id=trainer.id;
      else
        update event_trainer set lessons_offered=trainer.lessons_offered where id=trainer.id;
      end if;
    else
      insert into event_trainer (event_id, person_id, lessons_offered)
      values (v_event.id, trainer.person_id, coalesce(trainer.lessons_offered, 0))
      on conflict (event_id, person_id) do update
      set lessons_offered = coalesce(trainer.lessons_offered, 0);
    end if;
  end loop;

  foreach cohort in array cohorts loop
    if cohort.id is not null then
      if cohort.cohort_id is null then
        delete from event_target_cohort where id=cohort.id;
      end if;
    else
      insert into event_target_cohort (event_id, cohort_id)
      values (v_event.id, cohort.cohort_id);
    end if;
  end loop;

  foreach registration in array registrations loop
    if registration.id is not null then
      if registration.person_id is null and registration.couple_id is null then
        delete from event_registration where id=registration.id;
      else
        update event_registration
        set person_id=registration.person_id, couple_id=registration.couple_id
        where id=registration.id;
      end if;
    else
      insert into event_registration (event_id, person_id, couple_id)
      values (v_event.id, registration.person_id, registration.couple_id);
    end if;
  end loop;

  return v_event;
end;
$$;

GRANT ALL ON FUNCTION public.upsert_event(info public.event_type_input, instances public.event_instance_type_input[], trainers public.event_trainer_type_input[], cohorts public.event_target_cohort_type_input[], registrations public.event_registration_type_input[]) TO anonymous;
