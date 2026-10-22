CREATE FUNCTION public.set_lesson_demand(instance_registration_id bigint, instance_trainer_id bigint, lesson_count integer) RETURNS public.event_lesson_request
    LANGUAGE sql STRICT
    AS $_$
  select public.set_lesson_request($1, $2, $3);
$_$;

COMMENT ON FUNCTION public.set_lesson_demand(instance_registration_id bigint, instance_trainer_id bigint, lesson_count integer) IS '@deprecated Use setLessonRequest.
@resultFieldName eventLessonDemand';

GRANT ALL ON FUNCTION public.set_lesson_demand(instance_registration_id bigint, instance_trainer_id bigint, lesson_count integer) TO anonymous;
