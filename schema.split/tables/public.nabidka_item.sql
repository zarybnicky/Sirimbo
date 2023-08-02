CREATE TABLE public.nabidka_item (
    ni_id bigint NOT NULL,
    ni_id_rodic bigint NOT NULL,
    ni_partner bigint NOT NULL,
    ni_pocet_hod smallint DEFAULT '1'::smallint NOT NULL,
    ni_lock boolean DEFAULT true NOT NULL,
    id bigint GENERATED ALWAYS AS (ni_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.nabidka_item IS '@omit create,update,delete';

GRANT ALL ON TABLE public.nabidka_item TO anonymous;
ALTER TABLE public.nabidka_item ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.nabidka_item
    ADD CONSTRAINT idx_23810_primary PRIMARY KEY (ni_id);
ALTER TABLE ONLY public.nabidka_item
    ADD CONSTRAINT nabidka_item_unique_user_nabidka_key UNIQUE (ni_partner, ni_id_rodic);
ALTER TABLE ONLY public.nabidka_item
    ADD CONSTRAINT nabidka_item_ni_id_rodic_fkey FOREIGN KEY (ni_id_rodic) REFERENCES public.nabidka(n_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.nabidka_item
    ADD CONSTRAINT nabidka_item_ni_partner_fkey FOREIGN KEY (ni_partner) REFERENCES public.pary(p_id) ON UPDATE RESTRICT ON DELETE RESTRICT;

CREATE POLICY admin_all ON public.nabidka_item TO administrator USING (true) WITH CHECK (true);
CREATE POLICY manage_own ON public.nabidka_item TO member USING ((ni_partner IN ( SELECT public.current_couple_ids() AS current_couple_ids))) WITH CHECK ((ni_partner IN ( SELECT public.current_couple_ids() AS current_couple_ids)));
CREATE POLICY member_view ON public.nabidka_item FOR SELECT TO member USING (true);
CREATE POLICY my_tenant ON public.nabidka_item AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));

CREATE INDEX idx_23810_nabidka_item_ni_partner_fkey ON public.nabidka_item USING btree (ni_partner);
CREATE UNIQUE INDEX idx_23810_ni_id_rodic ON public.nabidka_item USING btree (ni_id_rodic, ni_partner);