CREATE TABLE public.cohort_group (
    id bigint NOT NULL,
    name text NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    ordering integer DEFAULT 1 NOT NULL,
    is_public boolean DEFAULT true NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.cohort_group IS '@omit create,update,delete';

GRANT ALL ON TABLE public.cohort_group TO anonymous;
ALTER TABLE public.cohort_group ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.cohort_group
    ADD CONSTRAINT cohort_group_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.cohort_group
    ADD CONSTRAINT cohort_group_tenant_id_id_key UNIQUE (tenant_id, id);
ALTER TABLE ONLY public.cohort_group
    ADD CONSTRAINT cohort_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.cohort_group TO administrator USING (true);
CREATE POLICY current_tenant ON public.cohort_group AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY public_view ON public.cohort_group FOR SELECT USING (true);

CREATE INDEX cohort_group_tenant_id_idx ON public.cohort_group USING btree (tenant_id);
