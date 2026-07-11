CREATE FUNCTION public.event_remaining_lessons(e public.event) RETURNS integer
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  select case
    when exists (
      select 1 from event_trainer et
      where et.event_id = e.id and et.lessons_offered is null
    ) then null
    else (
      select coalesce(sum(lessons_offered), 0) from event_trainer et where et.event_id = e.id
    ) - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_demand demand
      join event_instance_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      where demand.event_id = e.id
    )
  end;
$$;

GRANT ALL ON FUNCTION public.event_remaining_lessons(e public.event) TO anonymous;
