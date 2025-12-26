CREATE VIEW public.current_tenant_trainer AS
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
    member_price_45min,
    member_payout_45min,
    guest_price_45min,
    guest_payout_45min,
    create_payout_payments,
    status
   FROM public.tenant_trainer
  WHERE ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (status = 'active'::public.relationship_status));

COMMENT ON VIEW public.current_tenant_trainer IS '@omit';

GRANT SELECT ON TABLE public.current_tenant_trainer TO anonymous;
