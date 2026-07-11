CREATE FUNCTION public.event_trainer_lessons_remaining(e public.event_trainer) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select case
    when e.lessons_offered is null then null
    else e.lessons_offered - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_demand demand
      join event_instance_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      join event_instance_trainer trainer on trainer.id = demand.trainer_id
      join event_instance instance on instance.id = trainer.instance_id
      where instance.event_id = e.event_id and trainer.person_id = e.person_id
    )
  end;
$$;

GRANT ALL ON FUNCTION public.event_trainer_lessons_remaining(e public.event_trainer) TO anonymous;
