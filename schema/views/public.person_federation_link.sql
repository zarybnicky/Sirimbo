CREATE VIEW public.person_federation_link WITH (security_invoker='true') AS
 SELECT person.id AS person_id,
    'csts'::text AS federation,
    (person.csts_id)::bigint AS external_id
   FROM public.person
  WHERE (person.csts_id <> 0)
UNION ALL
 SELECT person.id AS person_id,
    'wdsf'::text AS federation,
    (person.wdsf_id)::bigint AS external_id
   FROM public.person
  WHERE (person.wdsf_id <> 0);

COMMENT ON VIEW public.person_federation_link IS '@primaryKey person_id,federation
@foreignKey (person_id) references public.person (id)
@foreignKey (federation,external_id) references federated.person (federation,external_id)
@behavior -single -list -connection -insert -update -delete';

GRANT SELECT ON TABLE public.person_federation_link TO anonymous;
