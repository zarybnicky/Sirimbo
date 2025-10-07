CREATE VIEW public.scoreboard AS
 SELECT person_id,
    cohort_id,
    lesson_total_score,
    group_total_score,
    event_total_score,
    manual_total_score,
    total_score,
    ranking
   FROM public.scoreboard_entries() scoreboard_entries(person_id, cohort_id, lesson_total_score, group_total_score, event_total_score, manual_total_score, total_score, ranking);

COMMENT ON VIEW public.scoreboard IS '@foreignKey (person_id) references person (id)
@foreignKey (cohort_id) references cohort (id)
@simpleCollections only';

GRANT ALL ON TABLE public.scoreboard TO anonymous;
