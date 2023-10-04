CREATE TABLE public.tenant_location (
    id bigint NOT NULL,
    name text NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    address public.address_domain,
    is_public boolean DEFAULT true,
    tenant_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.tenant_location IS '@simpleCollections only';

GRANT ALL ON TABLE public.tenant_location TO anonymous;
ALTER TABLE public.tenant_location ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.tenant_location
    ADD CONSTRAINT tenant_location_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.tenant_location
    ADD CONSTRAINT tenant_location_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_all ON public.tenant_location TO administrator USING (true) WITH CHECK (true);
CREATE POLICY my_tenant ON public.tenant_location AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));
CREATE POLICY public_view ON public.tenant_location FOR SELECT TO anonymous;

CREATE INDEX tenant_location_tenant_id_idx ON public.tenant_location USING btree (tenant_id);