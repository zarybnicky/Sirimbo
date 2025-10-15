CREATE TABLE public.galerie_foto (
    id bigint NOT NULL,
    gf_id_rodic bigint NOT NULL,
    gf_name text NOT NULL,
    gf_path text NOT NULL,
    gf_kdo bigint NOT NULL,
    updated_at timestamp with time zone,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE public.galerie_foto IS '@omit create,update,delete';

GRANT ALL ON TABLE public.galerie_foto TO anonymous;
ALTER TABLE public.galerie_foto ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT idx_23791_primary PRIMARY KEY (id);
ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT galerie_foto_gf_id_rodic_fkey FOREIGN KEY (gf_id_rodic) REFERENCES public.galerie_dir(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT galerie_foto_gf_kdo_fkey FOREIGN KEY (gf_kdo) REFERENCES public.users(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT galerie_foto_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.galerie_foto TO administrator USING (true);
CREATE POLICY current_tenant ON public.galerie_foto AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY public_view ON public.galerie_foto FOR SELECT USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.galerie_foto FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX galerie_foto_gf_id_rodic_idx ON public.galerie_foto USING btree (gf_id_rodic);
CREATE INDEX galerie_foto_tenant_id_idx ON public.galerie_foto USING btree (tenant_id);
CREATE INDEX idx_23791_galerie_foto_gf_kdo_fkey ON public.galerie_foto USING btree (gf_kdo);
CREATE INDEX idx_23791_gf_id_rodic ON public.galerie_foto USING btree (gf_id_rodic);
