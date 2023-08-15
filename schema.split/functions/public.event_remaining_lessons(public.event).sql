CREATE FUNCTION public.event_remaining_lessons(e public.event) RETURNS integer
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  select (select coalesce(sum(lessons_offered), 0) from event_trainer where event_id = e.id) - (select coalesce(sum(lesson_count), 0) from event_registration join event_lesson_demand on registration_id = event_registration.id where event_id = e.id);
$$;

GRANT ALL ON FUNCTION public.event_remaining_lessons(e public.event) TO anonymous;


