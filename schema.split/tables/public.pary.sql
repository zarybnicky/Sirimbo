CREATE TABLE public.pary (
    p_id bigint NOT NULL,
    p_id_partner bigint NOT NULL,
    p_id_partnerka bigint DEFAULT '0'::bigint,
    p_stt_trida public.pary_p_stt_trida DEFAULT 'Z'::public.pary_p_stt_trida NOT NULL,
    p_stt_body integer DEFAULT 0 NOT NULL,
    p_stt_finale boolean DEFAULT false NOT NULL,
    p_lat_trida public.pary_p_lat_trida DEFAULT 'Z'::public.pary_p_lat_trida NOT NULL,
    p_lat_body integer DEFAULT 0 NOT NULL,
    p_lat_finale boolean DEFAULT false NOT NULL,
    p_hodnoceni integer DEFAULT 0 NOT NULL,
    p_archiv boolean DEFAULT false NOT NULL,
    p_timestamp_add timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    p_timestamp_archive timestamp with time zone,
    id bigint GENERATED ALWAYS AS (p_id) STORED
);

COMMENT ON TABLE public.pary IS '@omit create,update,delete';

GRANT ALL ON TABLE public.pary TO anonymous;
ALTER TABLE public.pary ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.pary
    ADD CONSTRAINT idx_23824_primary PRIMARY KEY (p_id);
ALTER TABLE ONLY public.pary
    ADD CONSTRAINT pary_p_id_partner_fkey FOREIGN KEY (p_id_partner) REFERENCES public.users(u_id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.pary
    ADD CONSTRAINT pary_p_id_partnerka_fkey FOREIGN KEY (p_id_partnerka) REFERENCES public.users(u_id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.pary TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON public.pary FOR SELECT USING (true);

CREATE INDEX idx_23824_p_hodnoceni ON public.pary USING btree (p_hodnoceni);
CREATE INDEX idx_23824_pary_p_id_partner_fkey ON public.pary USING btree (p_id_partner);
CREATE INDEX idx_23824_pary_p_id_partnerka_fkey ON public.pary USING btree (p_id_partnerka);