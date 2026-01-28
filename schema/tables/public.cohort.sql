CREATE TABLE public.cohort (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    cohort_group_id bigint,
    name text NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    color_rgb text NOT NULL,
    location text DEFAULT ''::text NOT NULL,
    is_visible boolean DEFAULT true NOT NULL,
    ordering integer DEFAULT 1 NOT NULL,
    external_ids text[]
);

COMMENT ON TABLE public.cohort IS '@simpleCollections only';

GRANT ALL ON TABLE public.cohort TO anonymous;
ALTER TABLE public.cohort ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_tenant_id_id_key UNIQUE (tenant_id, id);
ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_cohort_group_fkey FOREIGN KEY (tenant_id, cohort_group_id) REFERENCES public.cohort_group(tenant_id, id) ON UPDATE CASCADE ON DELETE SET NULL;
ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.cohort TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON public.cohort FOR SELECT USING (true);

CREATE INDEX cohort_cohort_group_id_idx ON public.cohort USING btree (cohort_group_id);
CREATE INDEX cohort_tenant_id_idx ON public.cohort USING btree (tenant_id);
