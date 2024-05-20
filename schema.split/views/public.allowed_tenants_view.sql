CREATE VIEW public.allowed_tenants_view AS
 SELECT person.id AS person_id,
    tenant.id AS tenant_id,
    (max(member.id) IS NOT NULL) AS is_member,
    (max(trainer.id) IS NOT NULL) AS is_trainer,
    (max(admin.id) IS NOT NULL) AS is_admin,
    ((max(member.id) IS NOT NULL) OR (max(trainer.id) IS NOT NULL) OR (max(admin.id) IS NOT NULL)) AS is_allowed
   FROM ((((public.person
     CROSS JOIN public.tenant)
     LEFT JOIN public.tenant_membership member ON (((member.tenant_id = tenant.id) AND (member.person_id = person.id) AND (member.status = 'active'::public.relationship_status))))
     LEFT JOIN public.tenant_trainer trainer ON (((trainer.tenant_id = tenant.id) AND (trainer.person_id = person.id) AND (trainer.status = 'active'::public.relationship_status))))
     LEFT JOIN public.tenant_administrator admin ON (((admin.tenant_id = tenant.id) AND (admin.person_id = person.id) AND (admin.status = 'active'::public.relationship_status))))
  WHERE ((member.id IS NOT NULL) OR (trainer.id IS NOT NULL) OR (admin.id IS NOT NULL))
  GROUP BY person.id, tenant.id;

COMMENT ON VIEW public.allowed_tenants_view IS '@omit';

GRANT ALL ON TABLE public.allowed_tenants_view TO anonymous;


