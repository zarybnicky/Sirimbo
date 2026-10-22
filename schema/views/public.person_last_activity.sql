CREATE VIEW public.person_last_activity WITH (security_barrier='false') AS
 SELECT ( SELECT public.current_tenant_id() AS current_tenant_id) AS tenant_id,
    p.id AS person_id,
    web.web_at,
    COALESCE(card.card_entry_at, ( SELECT '1970-01-01 00:00:00+00'::timestamp with time zone AS timestamptz
           FROM public.access_credential c
          WHERE ((c.person_id = p.id) AND (c.tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)))
         LIMIT 1)) AS card_entry_at,
    training.training_at,
    competition.competition_on
   FROM ((((public.person p
     LEFT JOIN LATERAL ( SELECT COALESCE(max(u.last_active_at), '1970-01-01 00:00:00+00'::timestamp with time zone) AS web_at
           FROM (public.user_proxy up
             JOIN public.users u ON ((u.id = up.user_id)))
          WHERE ((up.person_id = p.id) AND (up.status = 'active'::public.relationship_status))
         HAVING (count(*) > 0)) web ON (true))
     LEFT JOIN LATERAL ( SELECT e.occurred_at AS card_entry_at
           FROM public.access_event e
          WHERE ((e.tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (e.person_id = p.id) AND e.allowed AND (e.occurred_at <= now()))
          ORDER BY e.occurred_at DESC
         LIMIT 1) card ON (true))
     LEFT JOIN LATERAL ( SELECT COALESCE(max(e.since) FILTER (WHERE (((r.status = 'attended'::public.attendance_type) OR (e.type = 'lesson'::public.event_type)) AND (NOT e.is_cancelled) AND (e.since <= now()))), '1970-01-01 00:00:00+00'::timestamp with time zone) AS training_at
           FROM (public.event_registration r
             JOIN public.event e ON (((e.id = r.event_id) AND (e.tenant_id = r.tenant_id))))
          WHERE ((r.person_id = p.id) AND (r.tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (r.registration_status = 'active'::public.event_registration_status) AND (e.type = ANY (ARRAY['lesson'::public.event_type, 'group'::public.event_type, 'camp'::public.event_type])))
         HAVING (count(*) > 0)) training ON (true))
     LEFT JOIN LATERAL ( SELECT COALESCE(max(
                CASE
                    WHEN (c.latest_result_at > CURRENT_DATE) THEN ( SELECT max(comp.start_date) AS max
                       FROM (federated.competition_result r
                         JOIN federated.competition comp ON ((comp.id = r.competition_id)))
                      WHERE ((r.competitor_id = c.id) AND (comp.start_date <= CURRENT_DATE)))
                    ELSE c.latest_result_at
                END), '1970-01-01'::date) AS competition_on
           FROM (((( VALUES ('csts'::text,(p.csts_id)::bigint), ('wdsf'::text,(p.wdsf_id)::bigint)) l(federation, external_id)
             JOIN federated.person fp ON (((fp.federation = l.federation) AND (fp.external_id = l.external_id))))
             LEFT JOIN federated.competitor_component cc ON ((cc.person_id = fp.id)))
             LEFT JOIN federated.competitor c ON ((c.id = cc.competitor_id)))
          WHERE (l.external_id <> 0)
         HAVING (count(*) > 0)) competition ON (true))
  WHERE ((( SELECT pg_has_role(CURRENT_USER, 'trainer'::name, 'member'::text) AS pg_has_role) OR (p.id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[]))) AND ((EXISTS ( SELECT
           FROM public.current_tenant_membership m
          WHERE (m.person_id = p.id))) OR (EXISTS ( SELECT
           FROM public.current_tenant_trainer t
          WHERE (t.person_id = p.id))) OR (EXISTS ( SELECT
           FROM public.current_tenant_administrator a
          WHERE (a.person_id = p.id))) OR (EXISTS ( SELECT
           FROM public.current_cohort_membership cm
          WHERE (cm.person_id = p.id)))));

COMMENT ON VIEW public.person_last_activity IS '@primaryKey person_id
@unique tenant_id,person_id
@foreignKey (person_id) references public.person (id)|@foreignFieldName lastActivity
@behavior -query:resource:single -query:resource:list -query:resource:connection -insert -update -delete';
COMMENT ON COLUMN public.person_last_activity.tenant_id IS '@notNull';
COMMENT ON COLUMN public.person_last_activity.person_id IS '@notNull';

GRANT SELECT ON TABLE public.person_last_activity TO anonymous;
