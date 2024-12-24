CREATE MATERIALIZED VIEW public.allowed_tenants AS
 SELECT person_id,
    tenant_id,
    is_member,
    is_trainer,
    is_admin,
    is_allowed
   FROM public.allowed_tenants_view
  WITH NO DATA;

COMMENT ON MATERIALIZED VIEW public.allowed_tenants IS '@omit';

GRANT ALL ON TABLE public.allowed_tenants TO anonymous;


CREATE UNIQUE INDEX allowed_tenants_person_id_tenant_id_idx ON public.allowed_tenants USING btree (person_id, tenant_id);
