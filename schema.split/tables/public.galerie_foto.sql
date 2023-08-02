CREATE TABLE public.galerie_foto (
    gf_id bigint NOT NULL,
    gf_id_rodic bigint NOT NULL,
    gf_name text NOT NULL,
    gf_path text NOT NULL,
    gf_kdo bigint NOT NULL,
    gf_timestamp timestamp with time zone,
    id bigint GENERATED ALWAYS AS (gf_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.galerie_foto IS '@omit create,update,delete';

GRANT ALL ON TABLE public.galerie_foto TO anonymous;
ALTER TABLE public.galerie_foto ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT idx_23791_primary PRIMARY KEY (gf_id);
ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT galerie_foto_gf_id_rodic_fkey FOREIGN KEY (gf_id_rodic) REFERENCES public.galerie_dir(gd_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT galerie_foto_gf_kdo_fkey FOREIGN KEY (gf_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;

CREATE POLICY admin_all ON public.galerie_foto TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON public.galerie_foto FOR SELECT USING (true);
CREATE POLICY my_tenant ON public.galerie_foto AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.galerie_foto FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_galerie_foto();

CREATE INDEX idx_23791_galerie_foto_gf_kdo_fkey ON public.galerie_foto USING btree (gf_kdo);
CREATE INDEX idx_23791_gf_id_rodic ON public.galerie_foto USING btree (gf_id_rodic);