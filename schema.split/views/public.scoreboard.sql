CREATE VIEW public.scoreboard AS
 WITH members AS (
         SELECT person.id
           FROM (public.person
             JOIN public.cohort_membership ON ((cohort_membership.person_id = person.id)))
          WHERE (now() <@ cohort_membership.active_range)
        ), attendances AS (
         SELECT event_attendance.person_id,
                CASE
                    WHEN (event_registration.target_cohort_id IS NULL) THEN 3
                    ELSE 0
                END AS lesson_score,
                CASE
                    WHEN (event_registration.target_cohort_id IS NULL) THEN 0
                    ELSE 2
                END AS group_score,
            event_instance.since
           FROM (((public.event_attendance
             JOIN public.event_registration ON ((event_registration.id = event_attendance.registration_id)))
             JOIN public.event ON ((event.id = event_registration.event_id)))
             JOIN public.event_instance ON ((event_attendance.instance_id = event_instance.id)))
          WHERE ((event_attendance.status = 'attended'::public.attendance_type) AND (event.type = 'lesson'::public.event_type) AND (event_instance.since > '2022-01-01 00:00:00+00'::timestamp with time zone) AND (event_attendance.person_id IN ( SELECT members.id
                   FROM members)))
        )
 SELECT attendances.person_id,
    sum(attendances.lesson_score) AS lesson_total_score,
    sum(attendances.group_score) AS group_total_score,
    sum((attendances.lesson_score + attendances.group_score)) AS total_score,
    rank() OVER (ORDER BY (sum((attendances.lesson_score + attendances.group_score))) DESC) AS ranking
   FROM attendances
  GROUP BY attendances.person_id
  ORDER BY (sum((attendances.lesson_score + attendances.group_score))) DESC;

COMMENT ON VIEW public.scoreboard IS '@foreignKey (person_id) references person (id)
@simpleCollections only';

GRANT ALL ON TABLE public.scoreboard TO anonymous;


