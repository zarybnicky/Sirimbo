CREATE MATERIALIZED VIEW public.auth_details AS
 SELECT person_id,
    couple_ids,
    cohort_memberships,
    tenant_memberships,
    tenant_trainers,
    tenant_administrators,
    allowed_tenants
   FROM public.auth_details_view
  WITH NO DATA;

COMMENT ON MATERIALIZED VIEW public.auth_details IS '@omit';

GRANT ALL ON TABLE public.auth_details TO anonymous;


CREATE UNIQUE INDEX auth_details_person_id_idx ON public.auth_details USING btree (person_id);
