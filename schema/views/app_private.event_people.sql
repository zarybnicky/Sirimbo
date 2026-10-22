CREATE MATERIALIZED VIEW app_private.event_people AS
 WITH RECURSIVE participants AS (
         SELECT p_1.tenant_id,
            p_1.event_id,
            array_agg(DISTINCT p_1.person_id ORDER BY p_1.person_id) FILTER (WHERE p_1.is_attendee) AS viewers,
            array_agg(DISTINCT p_1.person_id ORDER BY p_1.person_id) AS people
           FROM ( SELECT event_registration.tenant_id,
                    event_registration.event_id,
                    event_registration.person_id,
                    true AS is_attendee
                   FROM public.event_registration
                  WHERE ((event_registration.registration_status = 'active'::public.event_registration_status) AND (event_registration.person_id IS NOT NULL))
                UNION ALL
                 SELECT event_trainer.tenant_id,
                    event_trainer.event_id,
                    event_trainer.person_id,
                    false
                   FROM public.event_trainer) p_1
          GROUP BY p_1.tenant_id, p_1.event_id
        ), attended(tenant_id, viewers, event_id) AS (
         SELECT participants.tenant_id,
            participants.viewers,
            participants.event_id
           FROM participants
          WHERE (participants.viewers IS NOT NULL)
        UNION
         SELECT parent.tenant_id,
            parent.viewers,
            child.id
           FROM (attended parent
             JOIN public.event child ON (((child.parent_id = parent.event_id) AND (child.tenant_id = parent.tenant_id))))
          WHERE (child.parent_id IS NOT NULL)
        ), rosters AS (
         SELECT DISTINCT a.tenant_id,
            a.viewers,
            p_1.people
           FROM (attended a
             JOIN participants p_1 USING (tenant_id, event_id))
        )
 SELECT DISTINCT rosters.tenant_id,
    v.viewer_person_id,
    p.visible_person_id
   FROM ((rosters
     CROSS JOIN LATERAL unnest(rosters.viewers) v(viewer_person_id))
     CROSS JOIN LATERAL unnest(rosters.people) p(visible_person_id))
  WITH NO DATA;



CREATE UNIQUE INDEX event_people_key ON app_private.event_people USING btree (tenant_id, viewer_person_id, visible_person_id);
