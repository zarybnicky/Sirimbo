CREATE VIEW public.current_tenant_membership AS
 SELECT tenant_id,
    person_id,
    since,
    until,
    created_at,
    updated_at,
    id,
    active_range,
    status
   FROM public.tenant_membership
  WHERE ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (status = 'active'::public.relationship_status));

COMMENT ON VIEW public.current_tenant_membership IS '@omit';

GRANT SELECT ON TABLE public.current_tenant_membership TO anonymous;
