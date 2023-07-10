CREATE TABLE public.upozorneni_skupiny (
    ups_id bigint NOT NULL,
    ups_id_rodic bigint NOT NULL,
    ups_id_skupina bigint NOT NULL,
    ups_color text NOT NULL,
    id bigint GENERATED ALWAYS AS (ups_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

GRANT ALL ON TABLE public.upozorneni_skupiny TO anonymous;
ALTER TABLE public.upozorneni_skupiny ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT idx_23955_primary PRIMARY KEY (ups_id);
ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT upozorneni_skupiny_ups_id_rodic_fkey FOREIGN KEY (ups_id_rodic) REFERENCES public.upozorneni(up_id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT upozorneni_skupiny_ups_id_skupina_fkey FOREIGN KEY (ups_id_skupina) REFERENCES public.skupiny(s_id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.upozorneni_skupiny TO administrator USING (true) WITH CHECK (true);
CREATE POLICY member_view ON public.upozorneni_skupiny FOR SELECT TO member USING (true);
CREATE POLICY my_tenant ON public.upozorneni_skupiny AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));

CREATE INDEX idx_23955_upozorneni_skupiny_ups_id_rodic_fkey ON public.upozorneni_skupiny USING btree (ups_id_rodic);
CREATE INDEX idx_23955_upozorneni_skupiny_ups_id_skupina_fkey ON public.upozorneni_skupiny USING btree (ups_id_skupina);