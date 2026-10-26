CREATE VIEW public.couple_federation_link WITH (security_invoker='true') AS
 WITH links AS MATERIALIZED (
         SELECT person_federation_link.person_id,
            person_federation_link.federation,
            person_federation_link.external_id
           FROM public.person_federation_link
        )
 SELECT c.id AS couple_id,
    fc.id AS competitor_id
   FROM (((((((public.couple c
     JOIN links ml ON ((ml.person_id = c.man_id)))
     JOIN federated.person mp ON (((mp.federation = ml.federation) AND (mp.external_id = ml.external_id))))
     JOIN federated.competitor_component mc ON (((mc.person_id = mp.id) AND (mc.role = 'lead'::federated.competitor_role))))
     JOIN federated.competitor fc ON (((fc.id = mc.competitor_id) AND (fc.competitor_type = 'couple'::federated.competitor_type))))
     JOIN federated.competitor_component wc ON (((wc.competitor_id = fc.id) AND (wc.role = 'follow'::federated.competitor_role))))
     JOIN federated.person wp ON ((wp.id = wc.person_id)))
     JOIN links wl ON (((wl.person_id = c.woman_id) AND ((wl.federation = wp.federation) AND (wl.external_id = wp.external_id)))));

COMMENT ON VIEW public.couple_federation_link IS '@primaryKey couple_id,competitor_id
@foreignKey (couple_id) references public.couple (id)
@foreignKey (competitor_id) references federated.competitor (id)
@behavior -single -list -connection -insert -update -delete';

GRANT SELECT ON TABLE public.couple_federation_link TO anonymous;
