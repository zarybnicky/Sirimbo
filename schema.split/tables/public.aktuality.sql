CREATE TABLE public.aktuality (
    at_id bigint NOT NULL,
    at_kdo bigint,
    at_kat text DEFAULT '1'::text NOT NULL,
    at_jmeno text NOT NULL,
    at_text text NOT NULL,
    at_preview text NOT NULL,
    at_foto bigint,
    at_foto_main bigint,
    at_timestamp timestamp with time zone,
    at_timestamp_add timestamp with time zone DEFAULT now(),
    id bigint GENERATED ALWAYS AS (at_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    title_photo_url text
);

GRANT ALL ON TABLE public.aktuality TO anonymous;
ALTER TABLE public.aktuality ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT idx_23753_primary PRIMARY KEY (at_id);
ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_at_foto_main_fkey FOREIGN KEY (at_foto_main) REFERENCES public.galerie_foto(gf_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_at_kdo_fkey FOREIGN KEY (at_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.aktuality TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON public.aktuality FOR SELECT USING (true);
CREATE POLICY my_tenant ON public.aktuality AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));

CREATE TRIGGER on_update_author BEFORE UPDATE ON public.aktuality FOR EACH ROW EXECUTE FUNCTION public.on_update_author_aktuality();
CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.aktuality FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_aktuality();

CREATE INDEX idx_23753_aktuality_at_foto_main_fkey ON public.aktuality USING btree (at_foto_main);
CREATE INDEX idx_23753_aktuality_at_kdo_fkey ON public.aktuality USING btree (at_kdo);
CREATE INDEX idx_23753_at_timestamp_add ON public.aktuality USING btree (at_timestamp_add);
CREATE INDEX tenant_id ON public.aktuality USING btree (tenant_id);