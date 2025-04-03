CREATE OR REPLACE VIEW public.scoreboard AS
 WITH members AS (
         SELECT person.id
           FROM (public.person
             JOIN public.cohort_membership ON ((cohort_membership.person_id = person.id)))
          WHERE (cohort_membership.active AND (cohort_membership.tenant_id = public.current_tenant_id()))
        ), attendances AS (
         SELECT event_attendance.person_id,
                CASE
                    WHEN (event.type = 'lesson'::public.event_type) THEN 1
                    ELSE 0
                END AS lesson_score,
                CASE
                    WHEN (event.type = 'group'::public.event_type) THEN floor(((EXTRACT(epoch FROM (i.until - i.since)) / (60)::numeric) / (45)::numeric))
                    ELSE (0)::numeric
                END AS group_score,
                CASE
                    WHEN (event.type = 'camp'::public.event_type) THEN (3 + (2 * ((EXTRACT(epoch FROM (i.until - i.since)) > (86400)::numeric))::integer))
                    ELSE 0
                END AS event_score,
            i.since
           FROM (((public.event_attendance
             JOIN public.event_registration ON ((event_registration.id = event_attendance.registration_id)))
             JOIN public.event ON ((event.id = event_registration.event_id)))
             JOIN public.event_instance i ON ((event_attendance.instance_id = i.id)))
          WHERE (((event_attendance.status = 'attended'::public.attendance_type) OR (event.type = 'lesson'::public.event_type)) AND (event.type <> 'reservation'::public.event_type) AND (NOT i.is_cancelled) AND (i.since > '2023-09-01 00:00:00+00'::timestamp with time zone) AND (i.until < date_trunc('day'::text, now())) AND (event_attendance.person_id IN ( SELECT members.id
                   FROM members)))
        ), per_day AS (
         SELECT attendances.person_id,
            LEAST(sum(attendances.lesson_score), (4)::bigint) AS lesson_score,
            sum(attendances.group_score) AS group_score,
            sum(attendances.event_score) AS event_score,
            (((LEAST(sum(attendances.lesson_score), (4)::bigint))::numeric + sum(attendances.group_score)) + (sum(attendances.event_score))::numeric) AS total_score,
            attendances.since
           FROM attendances
          GROUP BY attendances.person_id, attendances.since
        )
 SELECT person_id,
    (sum(lesson_score))::bigint AS lesson_total_score,
    (sum(group_score))::bigint AS group_total_score,
    (sum(event_score))::bigint AS event_total_score,
    (sum((((lesson_score)::numeric + group_score) + (event_score)::numeric)))::bigint AS total_score,
    rank() OVER (ORDER BY (sum((((lesson_score)::numeric + group_score) + (event_score)::numeric)))::bigint DESC) AS ranking
   FROM per_day
  GROUP BY person_id
  ORDER BY (sum((((lesson_score)::numeric + group_score) + (event_score)::numeric)))::bigint DESC;

COMMENT ON VIEW public.scoreboard IS '@foreignKey (person_id) references person (id)
@simpleCollections only';

GRANT ALL ON TABLE public.scoreboard TO anonymous;
