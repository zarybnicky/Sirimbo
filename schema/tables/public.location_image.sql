CREATE TABLE public.location_image (
    tenant_id bigint DEFAULT public.current_tenant_id() CONSTRAINT tenant_location_image_tenant_id_not_null NOT NULL,
    location_id bigint CONSTRAINT tenant_location_image_location_id_not_null NOT NULL,
    file_id bigint CONSTRAINT tenant_location_image_file_id_not_null NOT NULL
);

COMMENT ON TABLE public.location_image IS '@omit create,update,delete
@simpleCollections only';

GRANT ALL ON TABLE public.location_image TO anonymous;
ALTER TABLE public.location_image ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.location_image
    ADD CONSTRAINT tenant_location_image_pkey PRIMARY KEY (tenant_id, location_id, file_id);
ALTER TABLE ONLY public.location_image
    ADD CONSTRAINT tenant_location_image_file_fk FOREIGN KEY (tenant_id, file_id) REFERENCES public.file(tenant_id, id) ON DELETE CASCADE;
ALTER TABLE ONLY public.location_image
    ADD CONSTRAINT tenant_location_image_location_fk FOREIGN KEY (tenant_id, location_id) REFERENCES public.location(tenant_id, id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.location_image TO administrator USING (true);
CREATE POLICY current_tenant ON public.location_image AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY public_view ON public.location_image FOR SELECT USING (true);

CREATE INDEX tenant_location_image_file_idx ON public.location_image USING btree (tenant_id, file_id);
