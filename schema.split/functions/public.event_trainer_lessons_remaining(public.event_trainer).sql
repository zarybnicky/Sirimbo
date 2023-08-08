CREATE FUNCTION public.event_trainer_lessons_remaining(e public.event_trainer) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select e.lessons_offered - (select sum(lesson_count) from event_lesson_demand where trainer_id = e.id);
$$;

GRANT ALL ON FUNCTION public.event_trainer_lessons_remaining(e public.event_trainer) TO anonymous;


