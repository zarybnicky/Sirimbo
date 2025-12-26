CREATE VIEW public.current_cohort_membership AS
 SELECT cohort_id,
    person_id,
    since,
    until,
    created_at,
    updated_at,
    id,
    tenant_id,
    active_range,
    status
   FROM public.cohort_membership
  WHERE ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (status = 'active'::public.relationship_status));

COMMENT ON VIEW public.current_cohort_membership IS '@omit';

GRANT SELECT ON TABLE public.current_cohort_membership TO anonymous;
