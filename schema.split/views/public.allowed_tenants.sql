CREATE MATERIALIZED VIEW public.allowed_tenants AS
 SELECT allowed_tenants_view.person_id,
    allowed_tenants_view.tenant_id,
    allowed_tenants_view.is_member,
    allowed_tenants_view.is_trainer,
    allowed_tenants_view.is_admin,
    allowed_tenants_view.is_allowed
   FROM public.allowed_tenants_view
  WITH NO DATA;

GRANT ALL ON TABLE public.allowed_tenants TO anonymous;



CREATE UNIQUE INDEX allowed_tenants_person_id_tenant_id_idx ON public.allowed_tenants USING btree (person_id, tenant_id);