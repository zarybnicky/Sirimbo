CREATE TABLE public.pary_navrh (
    pn_id bigint NOT NULL,
    pn_navrhl bigint NOT NULL,
    pn_partner bigint NOT NULL,
    pn_partnerka bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pn_id) STORED
);

GRANT ALL ON TABLE public.pary_navrh TO anonymous;
ALTER TABLE public.pary_navrh ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.pary_navrh
    ADD CONSTRAINT idx_23840_primary PRIMARY KEY (pn_id);
ALTER TABLE ONLY public.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_navrhl_fkey FOREIGN KEY (pn_navrhl) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_partner_fkey FOREIGN KEY (pn_partner) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_partnerka_fkey FOREIGN KEY (pn_partnerka) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;

CREATE POLICY admin_all ON public.pary_navrh TO administrator USING (true) WITH CHECK (true);
CREATE POLICY manage_own ON public.pary_navrh USING (((pn_navrhl = public.current_user_id()) OR (pn_partner = public.current_user_id()) OR (pn_partnerka = public.current_user_id()))) WITH CHECK (((pn_navrhl = public.current_user_id()) AND ((pn_partner = public.current_user_id()) OR (pn_partnerka = public.current_user_id()))));

CREATE INDEX idx_23840_pary_navrh_pn_navrhl_fkey ON public.pary_navrh USING btree (pn_navrhl);
CREATE INDEX idx_23840_pary_navrh_pn_partner_fkey ON public.pary_navrh USING btree (pn_partner);
CREATE INDEX idx_23840_pary_navrh_pn_partnerka_fkey ON public.pary_navrh USING btree (pn_partnerka);