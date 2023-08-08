CREATE FUNCTION public.event_remaining_lessons(e public.event) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select (select sum(lessons_offered) from event_trainer where event_id = e.id) - (select sum(lesson_count) from event_registration join event_lesson_demand on registration_id = event_registration.id where event_id = e.id);
$$;

GRANT ALL ON FUNCTION public.event_remaining_lessons(e public.event) TO anonymous;


