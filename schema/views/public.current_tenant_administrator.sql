CREATE VIEW public.current_tenant_administrator AS
 SELECT tenant_id,
    person_id,
    since,
    until,
    created_at,
    updated_at,
    id,
    is_visible,
    description,
    active_range,
    status
   FROM public.tenant_administrator
  WHERE ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (status = 'active'::public.relationship_status));

COMMENT ON VIEW public.current_tenant_administrator IS '@omit';

GRANT SELECT ON TABLE public.current_tenant_administrator TO anonymous;
