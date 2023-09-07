CREATE TABLE public.tenant_location (
    tenant_id bigint NOT NULL,
    location_id bigint NOT NULL,
    id bigint NOT NULL
);

COMMENT ON TABLE public.tenant_location IS '@omit create,update,delete
@simpleCollections only';

GRANT ALL ON TABLE public.tenant_location TO anonymous;
ALTER TABLE public.tenant_location ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.tenant_location
    ADD CONSTRAINT tenant_location_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.tenant_location
    ADD CONSTRAINT tenant_location_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.location(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.tenant_location
    ADD CONSTRAINT tenant_location_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.tenant_location TO administrator USING (true) WITH CHECK (true);
CREATE POLICY public_view ON public.tenant_location FOR SELECT TO anonymous USING (true);

CREATE INDEX tenant_location_location_id_idx ON public.tenant_location USING btree (location_id);
CREATE INDEX tenant_location_tenant_id_idx ON public.tenant_location USING btree (tenant_id);