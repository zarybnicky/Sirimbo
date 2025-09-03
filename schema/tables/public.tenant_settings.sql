CREATE TABLE public.tenant_settings (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    settings jsonb NOT NULL
);

COMMENT ON TABLE public.tenant_settings IS '@simpleCollections only
@omit create,delete';

GRANT ALL ON TABLE public.tenant_settings TO anonymous;
ALTER TABLE public.tenant_settings ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.tenant_settings
    ADD CONSTRAINT tenant_settings_pkey PRIMARY KEY (tenant_id);
ALTER TABLE ONLY public.tenant_settings
    ADD CONSTRAINT tenant_settings_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_own ON public.tenant_settings TO administrator USING (true) WITH CHECK (true);
CREATE POLICY current_tenant ON public.tenant_settings AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
