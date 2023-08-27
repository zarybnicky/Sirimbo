CREATE TABLE public.cohort_group (
    id bigint NOT NULL,
    name text NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    ordering integer DEFAULT 1 NOT NULL,
    is_public boolean DEFAULT true NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

GRANT ALL ON TABLE public.cohort_group TO anonymous;
ALTER TABLE public.cohort_group ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.cohort_group
    ADD CONSTRAINT cohort_group_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.cohort_group
    ADD CONSTRAINT cohort_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_all ON public.cohort_group TO administrator USING (true) WITH CHECK (true);
CREATE POLICY my_tenant ON public.cohort_group AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));
CREATE POLICY public_view ON public.cohort_group FOR SELECT TO anonymous USING (true);

CREATE INDEX idx_cg_tenant ON public.cohort_group USING btree (tenant_id);
CREATE INDEX is_public ON public.cohort_group USING btree (is_public);
CREATE INDEX ordering ON public.cohort_group USING btree (ordering);