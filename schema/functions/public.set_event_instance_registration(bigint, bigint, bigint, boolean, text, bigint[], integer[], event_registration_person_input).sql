CREATE FUNCTION public.set_event_instance_registration(p_instance_id bigint, p_person_id bigint, p_couple_id bigint, p_is_registered boolean, p_note text DEFAULT NULL::text, p_lesson_trainer_ids bigint[] DEFAULT NULL::bigint[], p_lesson_counts integer[] DEFAULT NULL::integer[], p_person public.event_registration_person_input DEFAULT NULL::public.event_registration_person_input) RETURNS public.event_registration
    LANGUAGE sql
    AS $_$
  select public.set_event_registration($1, $2, $3, $4, $5, $6, $7, $8);
$_$;

COMMENT ON FUNCTION public.set_event_instance_registration(p_instance_id bigint, p_person_id bigint, p_couple_id bigint, p_is_registered boolean, p_note text, p_lesson_trainer_ids bigint[], p_lesson_counts integer[], p_person public.event_registration_person_input) IS '@deprecated Use setEventRegistration.
@resultFieldName eventInstanceRegistration';

GRANT ALL ON FUNCTION public.set_event_instance_registration(p_instance_id bigint, p_person_id bigint, p_couple_id bigint, p_is_registered boolean, p_note text, p_lesson_trainer_ids bigint[], p_lesson_counts integer[], p_person public.event_registration_person_input) TO anonymous;
