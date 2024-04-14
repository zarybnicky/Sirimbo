CREATE TABLE app_private.pary_navrh (
    pn_id bigint NOT NULL,
    pn_navrhl bigint NOT NULL,
    pn_partner bigint NOT NULL,
    pn_partnerka bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pn_id) STORED NOT NULL
);

COMMENT ON TABLE app_private.pary_navrh IS '@omit create,update,delete';

GRANT ALL ON TABLE app_private.pary_navrh TO anonymous;
ALTER TABLE app_private.pary_navrh ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY app_private.pary_navrh
    ADD CONSTRAINT idx_23840_primary PRIMARY KEY (pn_id);
ALTER TABLE ONLY app_private.pary_navrh
    ADD CONSTRAINT pary_navrh_unique_id UNIQUE (id);
ALTER TABLE ONLY app_private.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_navrhl_fkey FOREIGN KEY (pn_navrhl) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY app_private.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_partner_fkey FOREIGN KEY (pn_partner) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY app_private.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_partnerka_fkey FOREIGN KEY (pn_partnerka) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;

CREATE POLICY admin_all ON app_private.pary_navrh TO administrator USING (true) WITH CHECK (true);
CREATE POLICY manage_own ON app_private.pary_navrh USING (((pn_navrhl = public.current_user_id()) OR (pn_partner = public.current_user_id()) OR (pn_partnerka = public.current_user_id()))) WITH CHECK (((pn_navrhl = public.current_user_id()) AND ((pn_partner = public.current_user_id()) OR (pn_partnerka = public.current_user_id()))));

CREATE INDEX idx_23840_pary_navrh_pn_navrhl_fkey ON app_private.pary_navrh USING btree (pn_navrhl);
CREATE INDEX idx_23840_pary_navrh_pn_partner_fkey ON app_private.pary_navrh USING btree (pn_partner);
CREATE INDEX idx_23840_pary_navrh_pn_partnerka_fkey ON app_private.pary_navrh USING btree (pn_partnerka);