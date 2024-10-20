CREATE FUNCTION public.event_trainer_name(t public.event_trainer) RETURNS text
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT (concat_ws(' '::text, person.prefix_title, person.first_name, person.last_name) ||
         CASE person.suffix_title
             WHEN ''::text THEN ''::text
             ELSE (', '::text || person.suffix_title)
         END)
    FROM public.person
   WHERE ((event_trainer_name.t).person_id = person.id);
END;

GRANT ALL ON FUNCTION public.event_trainer_name(t public.event_trainer) TO anonymous;
