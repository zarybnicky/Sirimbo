CREATE VIEW app_private.auth_details AS
 SELECT person.id AS person_id,
    array_agg(cohort_membership.cohort_id) AS cohort_memberships,
    array_agg(tenant_membership.tenant_id) AS tenant_memberships,
    array_agg(tenant_trainer.tenant_id) AS tenant_trainers,
    array_agg(tenant_administrator.tenant_id) AS tenant_administrators
   FROM ((((public.person
     LEFT JOIN public.cohort_membership ON (((person.id = cohort_membership.person_id) AND (now() <@ cohort_membership.active_range))))
     LEFT JOIN public.tenant_membership ON (((person.id = tenant_membership.person_id) AND (now() <@ tenant_membership.active_range))))
     LEFT JOIN public.tenant_trainer ON (((person.id = tenant_trainer.person_id) AND (now() <@ tenant_trainer.active_range))))
     LEFT JOIN public.tenant_administrator ON (((person.id = tenant_administrator.person_id) AND (now() <@ tenant_administrator.active_range))))
  GROUP BY person.id;



