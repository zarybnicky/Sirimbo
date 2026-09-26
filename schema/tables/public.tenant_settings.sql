CREATE TABLE public.tenant_settings (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    settings jsonb NOT NULL
);

COMMENT ON TABLE public.tenant_settings IS '@omit create,delete
@behavior -query:resource:list -query:resource:connection -singularRelation:resource:list';

GRANT ALL ON TABLE public.tenant_settings TO anonymous;
ALTER TABLE public.tenant_settings ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.tenant_settings
    ADD CONSTRAINT tenant_settings_pkey PRIMARY KEY (tenant_id);
ALTER TABLE ONLY public.tenant_settings
    ADD CONSTRAINT tenant_settings_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_own ON public.tenant_settings TO administrator USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY system_admin_all ON public.tenant_settings TO system_admin USING (true);
