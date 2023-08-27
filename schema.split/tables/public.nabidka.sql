CREATE TABLE public.nabidka (
    n_id bigint NOT NULL,
    n_trener bigint NOT NULL,
    n_pocet_hod smallint DEFAULT '1'::smallint NOT NULL,
    n_max_pocet_hod smallint DEFAULT '0'::bigint NOT NULL,
    n_od date NOT NULL,
    n_do date NOT NULL,
    n_visible boolean DEFAULT true NOT NULL,
    n_lock boolean DEFAULT true NOT NULL,
    n_timestamp timestamp with time zone,
    id bigint GENERATED ALWAYS AS (n_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.nabidka IS '@omit';

GRANT ALL ON TABLE public.nabidka TO anonymous;
ALTER TABLE public.nabidka ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.nabidka
    ADD CONSTRAINT idx_23800_primary PRIMARY KEY (n_id);
ALTER TABLE ONLY public.nabidka
    ADD CONSTRAINT nabidka_n_trener_fkey FOREIGN KEY (n_trener) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.nabidka
    ADD CONSTRAINT nabidka_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_all ON public.nabidka TO administrator USING (true) WITH CHECK (true);
CREATE POLICY member_view ON public.nabidka FOR SELECT TO member USING (true);
CREATE POLICY my_tenant ON public.nabidka AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.nabidka FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_nabidka();

CREATE INDEX idx_23800_n_trener ON public.nabidka USING btree (n_trener);
CREATE INDEX idx_na_tenant ON public.nabidka USING btree (tenant_id);
CREATE INDEX n_od ON public.nabidka USING btree (n_od);