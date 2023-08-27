CREATE TABLE public.rozpis (
    r_id bigint NOT NULL,
    r_trener bigint NOT NULL,
    r_kde text NOT NULL,
    r_datum date NOT NULL,
    r_visible boolean DEFAULT true NOT NULL,
    r_lock boolean DEFAULT true NOT NULL,
    r_timestamp timestamp with time zone,
    id bigint GENERATED ALWAYS AS (r_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.rozpis IS '@omit';

GRANT ALL ON TABLE public.rozpis TO anonymous;
ALTER TABLE public.rozpis ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.rozpis
    ADD CONSTRAINT idx_23909_primary PRIMARY KEY (r_id);
ALTER TABLE ONLY public.rozpis
    ADD CONSTRAINT rozpis_r_trener_fkey FOREIGN KEY (r_trener) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.rozpis
    ADD CONSTRAINT rozpis_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_all ON public.rozpis TO administrator USING (true) WITH CHECK (true);
CREATE POLICY member_view ON public.rozpis FOR SELECT TO member USING (true);
CREATE POLICY my_tenant ON public.rozpis AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.rozpis FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_rozpis();

CREATE INDEX idx_23909_r_trener ON public.rozpis USING btree (r_trener);
CREATE INDEX idx_ro_tenant ON public.rozpis USING btree (tenant_id);
CREATE INDEX r_datum ON public.rozpis USING btree (r_datum);