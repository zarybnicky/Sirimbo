CREATE FUNCTION public.event_instance_trainer_name(t public.event_instance_trainer) RETURNS text
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT public.person_name(person.*) AS person_name
    FROM public.person
   WHERE ((event_instance_trainer_name.t).person_id = person.id);
END;

GRANT ALL ON FUNCTION public.event_instance_trainer_name(t public.event_instance_trainer) TO anonymous;


