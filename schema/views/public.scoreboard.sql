CREATE VIEW public.scoreboard AS
 SELECT se.person_id,
    se.cohort_id,
    se.lesson_total_score,
    se.group_total_score,
    se.event_total_score,
    se.manual_total_score,
    se.total_score,
    se.ranking
   FROM public.scoreboard_entries() se;

COMMENT ON VIEW public.scoreboard IS '@foreignKey (person_id) references person (id)
@foreignKey (cohort_id) references cohort (id)
@simpleCollections only';

GRANT ALL ON TABLE public.scoreboard TO anonymous;
