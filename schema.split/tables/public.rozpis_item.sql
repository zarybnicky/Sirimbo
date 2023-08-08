CREATE TABLE public.rozpis_item (
    ri_id bigint NOT NULL,
    ri_id_rodic bigint NOT NULL,
    ri_partner bigint,
    ri_od time without time zone NOT NULL,
    ri_do time without time zone NOT NULL,
    ri_lock boolean DEFAULT true NOT NULL,
    id bigint GENERATED ALWAYS AS (ri_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.rozpis_item IS '@omit';

GRANT ALL ON TABLE public.rozpis_item TO anonymous;
ALTER TABLE public.rozpis_item ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.rozpis_item
    ADD CONSTRAINT idx_23920_primary PRIMARY KEY (ri_id);
ALTER TABLE ONLY public.rozpis_item
    ADD CONSTRAINT rozpis_item_ri_id_rodic_fkey FOREIGN KEY (ri_id_rodic) REFERENCES public.rozpis(r_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.rozpis_item
    ADD CONSTRAINT rozpis_item_ri_partner_fkey FOREIGN KEY (ri_partner) REFERENCES public.pary(p_id) ON UPDATE RESTRICT ON DELETE RESTRICT;

CREATE POLICY admin_all ON public.rozpis_item TO administrator USING (true) WITH CHECK (true);
CREATE POLICY manage_own ON public.rozpis_item TO member USING ((ri_partner IN ( SELECT public.current_couple_ids() AS current_couple_ids))) WITH CHECK ((ri_partner IN ( SELECT public.current_couple_ids() AS current_couple_ids)));
CREATE POLICY member_view ON public.rozpis_item FOR SELECT TO member USING (true);
CREATE POLICY my_tenant ON public.rozpis_item AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));

CREATE INDEX idx_23920_rozpis_item_ri_id_rodic_fkey ON public.rozpis_item USING btree (ri_id_rodic);
CREATE INDEX idx_23920_rozpis_item_ri_partner_fkey ON public.rozpis_item USING btree (ri_partner);
CREATE INDEX ri_od ON public.rozpis_item USING btree (ri_od);