CREATE MATERIALIZED VIEW public.auth_details AS
 SELECT auth_details_view.person_id,
    auth_details_view.couple_ids,
    auth_details_view.cohort_memberships,
    auth_details_view.tenant_memberships,
    auth_details_view.tenant_trainers,
    auth_details_view.tenant_administrators,
    auth_details_view.allowed_tenants
   FROM public.auth_details_view
  WITH NO DATA;

COMMENT ON MATERIALIZED VIEW public.auth_details IS '@omit';

GRANT ALL ON TABLE public.auth_details TO anonymous;


CREATE UNIQUE INDEX auth_details_person_id_idx ON public.auth_details USING btree (person_id);
