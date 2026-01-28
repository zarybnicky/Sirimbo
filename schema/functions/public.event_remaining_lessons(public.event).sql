CREATE FUNCTION public.event_remaining_lessons(e public.event) RETURNS integer
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  select (
    select coalesce(sum(lessons_offered), 0) from event_trainer et where et.event_id = e.id
  ) - (
    select coalesce(sum(lesson_count), 0) from event_registration er join event_lesson_demand eld on eld.registration_id = er.id where er.event_id = e.id
  );
$$;

GRANT ALL ON FUNCTION public.event_remaining_lessons(e public.event) TO anonymous;
