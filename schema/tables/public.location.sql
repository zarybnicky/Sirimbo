CREATE TABLE public.location (
    id bigint CONSTRAINT tenant_location_id_not_null NOT NULL,
    name text CONSTRAINT tenant_location_name_not_null NOT NULL,
    description text DEFAULT ''::text CONSTRAINT tenant_location_description_not_null NOT NULL,
    address public.address_domain,
    show_in_lists boolean DEFAULT false CONSTRAINT tenant_location_is_public_not_null NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() CONSTRAINT tenant_location_tenant_id_not_null NOT NULL,
    created_at timestamp with time zone DEFAULT now() CONSTRAINT tenant_location_created_at_not_null NOT NULL,
    updated_at timestamp with time zone DEFAULT now() CONSTRAINT tenant_location_updated_at_not_null NOT NULL,
    cover_image_id bigint,
    is_public boolean GENERATED ALWAYS AS (show_in_lists) STORED,
    long_name text,
    latitude double precision,
    longitude double precision,
    ordering integer DEFAULT 1 CONSTRAINT tenant_location_ordering_not_null NOT NULL
);

COMMENT ON TABLE public.location IS '@simpleCollections only
@behavior -query:resource:list -query:resource:connection -queryField:resource:connection';

GRANT ALL ON TABLE public.location TO anonymous;
ALTER TABLE public.location ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.location
    ADD CONSTRAINT tenant_location_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.location
    ADD CONSTRAINT tenant_location_tenant_id_id_key UNIQUE (tenant_id, id);
ALTER TABLE ONLY public.location
    ADD CONSTRAINT tenant_location_cover_image_fk FOREIGN KEY (tenant_id, cover_image_id) REFERENCES public.file(tenant_id, id) ON DELETE SET NULL (cover_image_id);
ALTER TABLE ONLY public.location
    ADD CONSTRAINT tenant_location_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_all ON public.location TO administrator USING (true);
CREATE POLICY current_tenant ON public.location AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY public_view ON public.location FOR SELECT USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.location FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX tenant_location_cover_image_idx ON public.location USING btree (tenant_id, cover_image_id) WHERE (cover_image_id IS NOT NULL);
CREATE INDEX tenant_location_tenant_id_idx ON public.location USING btree (tenant_id);
