CREATE FUNCTION public.event_instance_trainer_lessons_remaining(e public.event_instance_trainer) RETURNS integer
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
      where demand.trainer_id = e.id
    )
  end;
$$;

GRANT ALL ON FUNCTION public.event_instance_trainer_lessons_remaining(e public.event_instance_trainer) TO anonymous;
