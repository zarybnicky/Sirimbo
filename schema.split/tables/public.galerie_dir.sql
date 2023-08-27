CREATE TABLE public.galerie_dir (
    gd_id bigint NOT NULL,
    gd_id_rodic bigint NOT NULL,
    gd_name text NOT NULL,
    gd_level smallint DEFAULT '1'::smallint NOT NULL,
    gd_path text NOT NULL,
    gd_hidden boolean DEFAULT true NOT NULL,
    id bigint GENERATED ALWAYS AS (gd_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.galerie_dir IS '@omit create,update,delete';

GRANT ALL ON TABLE public.galerie_dir TO anonymous;
ALTER TABLE public.galerie_dir ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.galerie_dir
    ADD CONSTRAINT idx_23780_primary PRIMARY KEY (gd_id);
ALTER TABLE ONLY public.galerie_dir
    ADD CONSTRAINT galerie_dir_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_all ON public.galerie_dir TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON public.galerie_dir FOR SELECT USING (true);
CREATE POLICY my_tenant ON public.galerie_dir AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));

CREATE INDEX idx_23780_gd_id_rodic ON public.galerie_dir USING btree (gd_id_rodic);
CREATE INDEX idx_gd_tenant ON public.galerie_dir USING btree (tenant_id);