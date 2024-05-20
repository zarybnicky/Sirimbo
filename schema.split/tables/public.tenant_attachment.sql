CREATE TABLE public.tenant_attachment (
    tenant_id bigint NOT NULL,
    object_name text NOT NULL,
    type public.tenant_attachment_type
);

COMMENT ON TABLE public.tenant_attachment IS '@omit create,update,delete';

GRANT ALL ON TABLE public.tenant_attachment TO anonymous;
ALTER TABLE public.tenant_attachment ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.tenant_attachment
    ADD CONSTRAINT tenant_attachment_pkey PRIMARY KEY (tenant_id, object_name);
ALTER TABLE ONLY public.tenant_attachment
    ADD CONSTRAINT tenant_attachment_object_name_fkey FOREIGN KEY (object_name) REFERENCES public.attachment(object_name) ON DELETE CASCADE;
ALTER TABLE ONLY public.tenant_attachment
    ADD CONSTRAINT tenant_attachment_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.tenant_attachment TO administrator USING (true);
CREATE POLICY current_tenant ON public.tenant_attachment AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));
CREATE POLICY public_view ON public.tenant_attachment FOR SELECT USING (true);

CREATE INDEX tenant_attachment_object_name_idx ON public.tenant_attachment USING btree (object_name);