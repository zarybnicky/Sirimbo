CREATE TABLE public.galerie_dir (
    id bigint NOT NULL,
    gd_id_rodic bigint NOT NULL,
    gd_name text NOT NULL,
    gd_level smallint DEFAULT '1'::smallint NOT NULL,
    gd_path text NOT NULL,
    gd_hidden boolean DEFAULT true NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.galerie_dir IS '@omit create,update,delete';

GRANT ALL ON TABLE public.galerie_dir TO anonymous;
ALTER TABLE public.galerie_dir ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.galerie_dir
    ADD CONSTRAINT idx_23780_primary PRIMARY KEY (id);
ALTER TABLE ONLY public.galerie_dir
    ADD CONSTRAINT galerie_dir_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.galerie_dir TO administrator USING (true);
CREATE POLICY current_tenant ON public.galerie_dir AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY public_view ON public.galerie_dir FOR SELECT USING (true);

CREATE INDEX idx_23780_gd_id_rodic ON public.galerie_dir USING btree (gd_id_rodic);
