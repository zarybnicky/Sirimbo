CREATE TABLE public.dokumenty (
    d_id bigint NOT NULL,
    d_path text NOT NULL,
    d_name text NOT NULL,
    d_filename text NOT NULL,
    d_kategorie smallint NOT NULL,
    d_kdo bigint NOT NULL,
    d_timestamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    id bigint GENERATED ALWAYS AS (d_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

GRANT ALL ON TABLE public.dokumenty TO anonymous;
ALTER TABLE public.dokumenty ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.dokumenty
    ADD CONSTRAINT idx_23771_primary PRIMARY KEY (d_id);
ALTER TABLE ONLY public.dokumenty
    ADD CONSTRAINT dokumenty_d_kdo_fkey FOREIGN KEY (d_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;

CREATE POLICY admin_all ON public.dokumenty TO administrator USING (true) WITH CHECK (true);
CREATE POLICY my_tenant ON public.dokumenty AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));
CREATE POLICY public_view ON public.dokumenty FOR SELECT TO member USING (true);

CREATE TRIGGER on_delete_file_dokumenty AFTER DELETE ON public.dokumenty FOR EACH ROW EXECUTE FUNCTION public.on_delete_file_dokumenty();
CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.dokumenty FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_dokumenty();

CREATE INDEX d_kategorie ON public.dokumenty USING btree (d_kategorie);
CREATE INDEX d_timestamp ON public.dokumenty USING btree (d_timestamp);
CREATE UNIQUE INDEX idx_23771_d_path ON public.dokumenty USING btree (d_path);
CREATE INDEX idx_23771_dokumenty_d_kdo_fkey ON public.dokumenty USING btree (d_kdo);