CREATE FUNCTION public.register_to_event_many(event_registrations public.register_to_event_type[]) RETURNS SETOF public.event_registration
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  event event;
  created_ids bigint[] := array[]::bigint[];
  registration register_to_event_type;
  created event_registration;
  demand event_lesson_demand;
  instance_registration_id bigint;
begin
  foreach registration in array event_registrations loop
    select * into event from event where id = registration.event_id;

    if event is null then
      raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
    end if;
    if event.is_locked = true then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;
    if registration.person_id <> all (current_person_ids()) and registration.couple_id <> all (current_couple_ids()) then
      raise exception 'ACCESS_DENIED' using errcode = '42501';
    end if;

    insert into event_registration (event_id, person_id, couple_id, note)
    values (registration.event_id, registration.person_id, registration.couple_id, registration.note)
    returning * into created;
    created_ids := created_ids || created.id;
    if coalesce(cardinality(registration.lessons), 0) > 0 then
      foreach demand in array registration.lessons loop
        select instance_registration.id into instance_registration_id
        from event_instance_trainer trainer
        join event_instance_registration instance_registration
          on instance_registration.instance_id = trainer.instance_id
         and instance_registration.legacy_registration_id = created.id
         and instance_registration.parent_registration_id is null
        where trainer.id = demand.trainer_id;

        if not found then
          raise exception 'TRAINER_NOT_FOUND' using errcode = '28000';
        end if;

        perform set_lesson_demand(instance_registration_id, demand.trainer_id, demand.lesson_count);
      end loop;
    end if;
  end loop;
  return query select * from event_registration where id = any (created_ids);
end;
$$;

COMMENT ON FUNCTION public.register_to_event_many(event_registrations public.register_to_event_type[]) IS '@omit';

GRANT ALL ON FUNCTION public.register_to_event_many(event_registrations public.register_to_event_type[]) TO anonymous;
