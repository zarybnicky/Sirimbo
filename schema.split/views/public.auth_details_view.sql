CREATE VIEW public.auth_details_view AS
 SELECT person.id AS person_id,
    array_remove(array_agg(couple.id), NULL::bigint) AS couple_ids,
    array_remove(array_agg(cohort_membership.cohort_id), NULL::bigint) AS cohort_memberships,
    array_remove(array_agg(tenant_membership.tenant_id), NULL::bigint) AS tenant_memberships,
    array_remove(array_agg(tenant_trainer.tenant_id), NULL::bigint) AS tenant_trainers,
    array_remove(array_agg(tenant_administrator.tenant_id), NULL::bigint) AS tenant_administrators,
    array_remove(((array_agg(tenant_administrator.tenant_id) || array_agg(tenant_trainer.tenant_id)) || array_agg(tenant_membership.tenant_id)), NULL::bigint) AS allowed_tenants
   FROM (((((public.person
     LEFT JOIN public.couple ON ((((person.id = couple.man_id) OR (person.id = couple.woman_id)) AND (couple.status = 'active'::public.relationship_status))))
     LEFT JOIN public.cohort_membership ON (((person.id = cohort_membership.person_id) AND (cohort_membership.status = 'active'::public.relationship_status))))
     LEFT JOIN public.tenant_membership ON (((person.id = tenant_membership.person_id) AND (tenant_membership.status = 'active'::public.relationship_status))))
     LEFT JOIN public.tenant_trainer ON (((person.id = tenant_trainer.person_id) AND (tenant_trainer.status = 'active'::public.relationship_status))))
     LEFT JOIN public.tenant_administrator ON (((person.id = tenant_administrator.person_id) AND (tenant_administrator.status = 'active'::public.relationship_status))))
  GROUP BY person.id;

COMMENT ON VIEW public.auth_details_view IS '@omit';

GRANT ALL ON TABLE public.auth_details_view TO anonymous;


