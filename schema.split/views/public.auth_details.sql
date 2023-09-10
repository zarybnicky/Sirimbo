CREATE MATERIALIZED VIEW public.auth_details AS
 SELECT person.id AS person_id,
    array_remove(array_agg(couple.id), NULL::bigint) AS couple_ids,
    array_remove(array_agg(cohort_membership.cohort_id), NULL::bigint) AS cohort_memberships,
    array_remove(array_agg(tenant_membership.tenant_id), NULL::bigint) AS tenant_memberships,
    array_remove(array_agg(tenant_trainer.tenant_id), NULL::bigint) AS tenant_trainers,
    array_remove(array_agg(tenant_administrator.tenant_id), NULL::bigint) AS tenant_administrators,
    array_remove(((array_agg(tenant_administrator.tenant_id) || array_agg(tenant_trainer.tenant_id)) || array_agg(tenant_membership.tenant_id)), NULL::bigint) AS allowed_tenants
   FROM (((((public.person
     LEFT JOIN public.couple ON ((((person.id = couple.man_id) OR (person.id = couple.woman_id)) AND (now() <@ couple.active_range))))
     LEFT JOIN public.cohort_membership ON (((person.id = cohort_membership.person_id) AND (now() <@ cohort_membership.active_range))))
     LEFT JOIN public.tenant_membership ON (((person.id = tenant_membership.person_id) AND (now() <@ tenant_membership.active_range))))
     LEFT JOIN public.tenant_trainer ON (((person.id = tenant_trainer.person_id) AND (now() <@ tenant_trainer.active_range))))
     LEFT JOIN public.tenant_administrator ON (((person.id = tenant_administrator.person_id) AND (now() <@ tenant_administrator.active_range))))
  GROUP BY person.id
  WITH NO DATA;

GRANT ALL ON TABLE public.auth_details TO anonymous;



CREATE UNIQUE INDEX auth_details_person_id_idx ON public.auth_details USING btree (person_id);