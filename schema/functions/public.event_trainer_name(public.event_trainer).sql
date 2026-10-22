CREATE FUNCTION public.event_trainer_name(trainer public.event_trainer) RETURNS text
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT (concat_ws(' '::text, p.prefix_title, p.first_name, p.last_name) ||
         CASE p.suffix_title
             WHEN ''::text THEN ''::text
             ELSE (', '::text || p.suffix_title)
         END)
    FROM public.person p
   WHERE (p.id = (event_trainer_name.trainer).person_id);
END;

GRANT ALL ON FUNCTION public.event_trainer_name(trainer public.event_trainer) TO anonymous;
