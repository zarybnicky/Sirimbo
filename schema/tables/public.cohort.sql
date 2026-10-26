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
    external_ids text[],
    is_archived boolean DEFAULT false NOT NULL,
    image_id bigint
);

COMMENT ON TABLE public.cohort IS '@behavior +list -connection';

GRANT ALL ON TABLE public.cohort TO anonymous;
ALTER TABLE public.cohort ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_tenant_id_id_key UNIQUE (tenant_id, id);
ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_cohort_group_fkey FOREIGN KEY (tenant_id, cohort_group_id) REFERENCES public.cohort_group(tenant_id, id) ON UPDATE CASCADE ON DELETE SET NULL;
ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_image_fk FOREIGN KEY (tenant_id, image_id) REFERENCES public.file(tenant_id, id) ON DELETE SET NULL (image_id);
ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.cohort TO administrator USING (true);
CREATE POLICY current_tenant ON public.cohort AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY public_view ON public.cohort FOR SELECT TO anonymous USING ((is_visible OR app_private.cohort_has_accessible_event(id)));

CREATE INDEX cohort_cohort_group_id_idx ON public.cohort USING btree (cohort_group_id);
CREATE INDEX cohort_image_idx ON public.cohort USING btree (tenant_id, image_id) WHERE (image_id IS NOT NULL);
CREATE INDEX cohort_tenant_id_idx ON public.cohort USING btree (tenant_id);
