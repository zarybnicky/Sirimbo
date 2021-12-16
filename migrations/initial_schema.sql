SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;



CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


CREATE TYPE public.pary_p_lat_trida AS ENUM ('Z', 'H', 'D', 'C', 'B', 'A', 'M');
CREATE TYPE public.pary_p_stt_trida AS ENUM ('Z', 'H', 'D', 'C', 'B', 'A', 'M');

SET default_tablespace = '';
SET default_with_oids = false;

CREATE TABLE public.akce (
    a_id bigint NOT NULL,
    a_jmeno text NOT NULL,
    a_kde text NOT NULL,
    a_info text NOT NULL,
    a_od date NOT NULL,
    a_do date NOT NULL,
    a_kapacita bigint DEFAULT '0'::bigint NOT NULL,
    a_dokumenty text NOT NULL,
    a_timestamp timestamp with time zone,
    a_lock boolean DEFAULT false NOT NULL,
    a_visible boolean DEFAULT false NOT NULL
);

CREATE SEQUENCE public.akce_a_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.akce_a_id_seq OWNED BY public.akce.a_id;
ALTER TABLE ONLY public.akce ALTER COLUMN a_id SET DEFAULT nextval('public.akce_a_id_seq'::regclass);
ALTER TABLE ONLY public.akce ADD CONSTRAINT idx_23735_primary PRIMARY KEY (a_id);

CREATE FUNCTION public.on_update_current_timestamp_akce() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
   NEW.a_timestamp = now();
   RETURN NEW;
END;
$$;
CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.akce FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_akce();


CREATE TABLE public.akce_item (
    ai_id bigint NOT NULL,
    ai_id_rodic bigint NOT NULL,
    ai_user bigint NOT NULL,
    ai_rok_narozeni smallint NOT NULL
);

CREATE SEQUENCE public.akce_item_ai_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.akce_item_ai_id_seq OWNED BY public.akce_item.ai_id;
ALTER TABLE ONLY public.akce_item ALTER COLUMN ai_id SET DEFAULT nextval('public.akce_item_ai_id_seq'::regclass);
ALTER TABLE ONLY public.akce_item ADD CONSTRAINT idx_23747_primary PRIMARY KEY (ai_id);
CREATE INDEX idx_23747_akce_item_ai_id_rodic_fkey ON public.akce_item USING btree (ai_id_rodic);
CREATE INDEX idx_23747_akce_item_ai_user_fkey ON public.akce_item USING btree (ai_user);


CREATE TABLE public.aktuality (
    at_id bigint NOT NULL,
    at_kdo bigint NOT NULL,
    at_kat text NOT NULL,
    at_jmeno text NOT NULL,
    at_text text NOT NULL,
    at_preview text NOT NULL,
    at_foto bigint,
    at_foto_main bigint,
    at_timestamp timestamp with time zone,
    at_timestamp_add timestamp with time zone
);

CREATE SEQUENCE public.aktuality_at_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.aktuality_at_id_seq OWNED BY public.aktuality.at_id;
ALTER TABLE ONLY public.aktuality ALTER COLUMN at_id SET DEFAULT nextval('public.aktuality_at_id_seq'::regclass);
ALTER TABLE ONLY public.aktuality ADD CONSTRAINT idx_23753_primary PRIMARY KEY (at_id);
CREATE INDEX idx_23753_aktuality_at_foto_main_fkey ON public.aktuality USING btree (at_foto_main);
CREATE INDEX idx_23753_aktuality_at_kdo_fkey ON public.aktuality USING btree (at_kdo);
CREATE INDEX idx_23753_at_timestamp_add ON public.aktuality USING btree (at_timestamp_add);

CREATE FUNCTION public.on_update_current_timestamp_aktuality() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
   NEW.at_timestamp = now();
   RETURN NEW;
END;
$$;
CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.aktuality FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_aktuality();

CREATE VIEW public.aktuality_admin AS
 SELECT aktuality.at_id,
    aktuality.at_kdo,
    aktuality.at_kat,
    aktuality.at_jmeno,
    aktuality.at_text,
    aktuality.at_preview,
    aktuality.at_foto,
    aktuality.at_foto_main,
    aktuality.at_timestamp,
    aktuality.at_timestamp_add
   FROM public.aktuality;


CREATE TABLE public.dokumenty (
    d_id bigint NOT NULL,
    d_path text NOT NULL,
    d_name text NOT NULL,
    d_filename text NOT NULL,
    d_kategorie smallint NOT NULL,
    d_kdo bigint NOT NULL,
    d_timestamp timestamp with time zone
);

CREATE SEQUENCE public.dokumenty_d_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.dokumenty_d_id_seq OWNED BY public.dokumenty.d_id;
ALTER TABLE ONLY public.dokumenty ALTER COLUMN d_id SET DEFAULT nextval('public.dokumenty_d_id_seq'::regclass);
ALTER TABLE ONLY public.dokumenty ADD CONSTRAINT idx_23771_primary PRIMARY KEY (d_id);
CREATE UNIQUE INDEX idx_23771_d_path ON public.dokumenty USING btree (d_path);
CREATE INDEX idx_23771_dokumenty_d_kdo_fkey ON public.dokumenty USING btree (d_kdo);

CREATE FUNCTION public.on_update_current_timestamp_dokumenty() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
   NEW.d_timestamp = now();
   RETURN NEW;
END;
$$;
CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.dokumenty FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_dokumenty();


CREATE TABLE public.galerie_dir (
    gd_id bigint NOT NULL,
    gd_id_rodic bigint NOT NULL,
    gd_name text NOT NULL,
    gd_level smallint DEFAULT '1'::smallint NOT NULL,
    gd_path text NOT NULL,
    gd_hidden boolean DEFAULT true NOT NULL
);

CREATE SEQUENCE public.galerie_dir_gd_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.galerie_dir_gd_id_seq OWNED BY public.galerie_dir.gd_id;
ALTER TABLE ONLY public.galerie_dir ALTER COLUMN gd_id SET DEFAULT nextval('public.galerie_dir_gd_id_seq'::regclass);
ALTER TABLE ONLY public.galerie_dir ADD CONSTRAINT idx_23780_primary PRIMARY KEY (gd_id);
CREATE INDEX idx_23780_gd_id_rodic ON public.galerie_dir USING btree (gd_id_rodic);


CREATE TABLE public.galerie_foto (
    gf_id bigint NOT NULL,
    gf_id_rodic bigint NOT NULL,
    gf_name text NOT NULL,
    gf_path text NOT NULL,
    gf_kdo bigint NOT NULL,
    gf_timestamp timestamp with time zone
);

CREATE SEQUENCE public.galerie_foto_gf_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.galerie_foto_gf_id_seq OWNED BY public.galerie_foto.gf_id;
ALTER TABLE ONLY public.galerie_foto ALTER COLUMN gf_id SET DEFAULT nextval('public.galerie_foto_gf_id_seq'::regclass);
ALTER TABLE ONLY public.galerie_foto ADD CONSTRAINT idx_23791_primary PRIMARY KEY (gf_id);
CREATE INDEX idx_23791_galerie_foto_gf_kdo_fkey ON public.galerie_foto USING btree (gf_kdo);
CREATE INDEX idx_23791_gf_id_rodic ON public.galerie_foto USING btree (gf_id_rodic);

CREATE FUNCTION public.on_update_current_timestamp_galerie_foto() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
   NEW.gf_timestamp = now();
   RETURN NEW;
END;
$$;
CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.galerie_foto FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_galerie_foto();


CREATE TABLE public.nabidka (
    n_id bigint NOT NULL,
    n_trener bigint NOT NULL,
    n_pocet_hod smallint DEFAULT '1'::smallint NOT NULL,
    n_max_pocet_hod bigint DEFAULT '0'::bigint NOT NULL,
    n_od date NOT NULL,
    n_do date NOT NULL,
    n_visible boolean DEFAULT true NOT NULL,
    n_lock boolean DEFAULT true NOT NULL,
    n_timestamp timestamp with time zone
);

CREATE SEQUENCE public.nabidka_n_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.nabidka_n_id_seq OWNED BY public.nabidka.n_id;
ALTER TABLE ONLY public.nabidka ALTER COLUMN n_id SET DEFAULT nextval('public.nabidka_n_id_seq'::regclass);
ALTER TABLE ONLY public.nabidka ADD CONSTRAINT idx_23800_primary PRIMARY KEY (n_id);
CREATE INDEX idx_23800_n_trener ON public.nabidka USING btree (n_trener);

CREATE FUNCTION public.on_update_current_timestamp_nabidka() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
   NEW.n_timestamp = now();
   RETURN NEW;
END;
$$;
CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.nabidka FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_nabidka();

CREATE VIEW public.nabidka_admin AS
 SELECT nabidka.n_id,
    nabidka.n_trener,
    nabidka.n_pocet_hod,
    nabidka.n_max_pocet_hod,
    nabidka.n_od,
    nabidka.n_do,
    nabidka.n_visible,
    nabidka.n_lock,
    nabidka.n_timestamp
   FROM public.nabidka;


CREATE TABLE public.nabidka_item (
    ni_id bigint NOT NULL,
    ni_id_rodic bigint NOT NULL,
    ni_partner bigint NOT NULL,
    ni_pocet_hod smallint DEFAULT '1'::smallint NOT NULL,
    ni_lock boolean DEFAULT true NOT NULL
);

CREATE SEQUENCE public.nabidka_item_ni_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.nabidka_item_ni_id_seq OWNED BY public.nabidka_item.ni_id;
ALTER TABLE ONLY public.nabidka_item ALTER COLUMN ni_id SET DEFAULT nextval('public.nabidka_item_ni_id_seq'::regclass);
ALTER TABLE ONLY public.nabidka_item ADD CONSTRAINT idx_23810_primary PRIMARY KEY (ni_id);
CREATE INDEX idx_23810_nabidka_item_ni_partner_fkey ON public.nabidka_item USING btree (ni_partner);
CREATE UNIQUE INDEX idx_23810_ni_id_rodic ON public.nabidka_item USING btree (ni_id_rodic, ni_partner);


CREATE TABLE public.parameters (
    pa_name character varying(40) NOT NULL,
    pa_value text NOT NULL
);
ALTER TABLE ONLY public.parameters ADD CONSTRAINT idx_23816_primary PRIMARY KEY (pa_name);


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
    p_timestamp_archive timestamp with time zone
);

CREATE SEQUENCE public.pary_p_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.pary_p_id_seq OWNED BY public.pary.p_id;
ALTER TABLE ONLY public.pary ALTER COLUMN p_id SET DEFAULT nextval('public.pary_p_id_seq'::regclass);
ALTER TABLE ONLY public.pary ADD CONSTRAINT idx_23824_primary PRIMARY KEY (p_id);
CREATE INDEX idx_23824_p_hodnoceni ON public.pary USING btree (p_hodnoceni);
CREATE INDEX idx_23824_pary_p_id_partner_fkey ON public.pary USING btree (p_id_partner);
CREATE INDEX idx_23824_pary_p_id_partnerka_fkey ON public.pary USING btree (p_id_partnerka);


CREATE TABLE public.pary_navrh (
    pn_id bigint NOT NULL,
    pn_navrhl bigint NOT NULL,
    pn_partner bigint NOT NULL,
    pn_partnerka bigint NOT NULL
);

CREATE SEQUENCE public.pary_navrh_pn_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.pary_navrh_pn_id_seq OWNED BY public.pary_navrh.pn_id;
ALTER TABLE ONLY public.pary_navrh ALTER COLUMN pn_id SET DEFAULT nextval('public.pary_navrh_pn_id_seq'::regclass);
ALTER TABLE ONLY public.pary_navrh ADD CONSTRAINT idx_23840_primary PRIMARY KEY (pn_id);
CREATE INDEX idx_23840_pary_navrh_pn_navrhl_fkey ON public.pary_navrh USING btree (pn_navrhl);
CREATE INDEX idx_23840_pary_navrh_pn_partner_fkey ON public.pary_navrh USING btree (pn_partner);
CREATE INDEX idx_23840_pary_navrh_pn_partnerka_fkey ON public.pary_navrh USING btree (pn_partnerka);


CREATE TABLE public.permissions (
    pe_id bigint NOT NULL,
    pe_name text NOT NULL,
    pe_description text NOT NULL,
    pe_akce integer NOT NULL,
    pe_aktuality integer NOT NULL,
    pe_ankety integer NOT NULL,
    pe_dokumenty integer NOT NULL,
    pe_galerie integer NOT NULL,
    pe_inzerce integer NOT NULL,
    pe_konzole integer NOT NULL,
    pe_nabidka integer NOT NULL,
    pe_nastenka integer NOT NULL,
    pe_novinky integer NOT NULL,
    pe_pary integer NOT NULL,
    pe_platby integer NOT NULL,
    pe_permissions integer NOT NULL,
    pe_rozpis integer NOT NULL,
    pe_skupiny integer NOT NULL,
    pe_users integer NOT NULL,
    pe_main integer NOT NULL
);

CREATE SEQUENCE public.permissions_pe_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.permissions_pe_id_seq OWNED BY public.permissions.pe_id;
ALTER TABLE ONLY public.permissions ALTER COLUMN pe_id SET DEFAULT nextval('public.permissions_pe_id_seq'::regclass);
ALTER TABLE ONLY public.permissions ADD CONSTRAINT idx_23846_primary PRIMARY KEY (pe_id);


CREATE TABLE public.platby_category (
    pc_id bigint NOT NULL,
    pc_name text NOT NULL,
    pc_symbol bigint NOT NULL,
    pc_amount numeric(10,2) NOT NULL,
    pc_date_due date NOT NULL,
    pc_valid_from date NOT NULL,
    pc_valid_to date NOT NULL,
    pc_use_base boolean DEFAULT false NOT NULL,
    pc_use_prefix boolean DEFAULT false NOT NULL,
    pc_archive boolean DEFAULT false NOT NULL,
    pc_visible boolean DEFAULT true NOT NULL
);

CREATE SEQUENCE public.platby_category_pc_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.platby_category_pc_id_seq OWNED BY public.platby_category.pc_id;
ALTER TABLE ONLY public.platby_category ALTER COLUMN pc_id SET DEFAULT nextval('public.platby_category_pc_id_seq'::regclass);
ALTER TABLE ONLY public.platby_category ADD CONSTRAINT idx_23855_primary PRIMARY KEY (pc_id);
CREATE UNIQUE INDEX idx_23855_pc_symbol ON public.platby_category USING btree (pc_symbol);


CREATE TABLE public.platby_category_group (
    pcg_id bigint NOT NULL,
    pcg_id_group bigint NOT NULL,
    pcg_id_category bigint NOT NULL
);

CREATE SEQUENCE public.platby_category_group_pcg_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.platby_category_group_pcg_id_seq OWNED BY public.platby_category_group.pcg_id;
ALTER TABLE ONLY public.platby_category_group ALTER COLUMN pcg_id SET DEFAULT nextval('public.platby_category_group_pcg_id_seq'::regclass);
ALTER TABLE ONLY public.platby_category_group ADD CONSTRAINT idx_23868_primary PRIMARY KEY (pcg_id);
CREATE UNIQUE INDEX idx_23868_pcg_id_group ON public.platby_category_group USING btree (pcg_id_group, pcg_id_category);
CREATE INDEX idx_23868_platby_category_group_pcg_id_category_fkey ON public.platby_category_group USING btree (pcg_id_category);


CREATE TABLE public.platby_group (
    pg_id bigint NOT NULL,
    pg_type numeric DEFAULT '1'::numeric NOT NULL,
    pg_name text NOT NULL,
    pg_description text NOT NULL,
    pg_base bigint DEFAULT '0'::bigint NOT NULL
);

CREATE SEQUENCE public.platby_group_pg_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.platby_group_pg_id_seq OWNED BY public.platby_group.pg_id;
ALTER TABLE ONLY public.platby_group ALTER COLUMN pg_id SET DEFAULT nextval('public.platby_group_pg_id_seq'::regclass);
ALTER TABLE ONLY public.platby_group ADD CONSTRAINT idx_23874_primary PRIMARY KEY (pg_id);


CREATE TABLE public.platby_group_skupina (
    pgs_id bigint NOT NULL,
    pgs_id_skupina bigint NOT NULL,
    pgs_id_group bigint NOT NULL
);

CREATE SEQUENCE public.platby_group_skupina_pgs_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.platby_group_skupina_pgs_id_seq OWNED BY public.platby_group_skupina.pgs_id;
ALTER TABLE ONLY public.platby_group_skupina ALTER COLUMN pgs_id SET DEFAULT nextval('public.platby_group_skupina_pgs_id_seq'::regclass);
ALTER TABLE ONLY public.platby_group_skupina ADD CONSTRAINT idx_23885_primary PRIMARY KEY (pgs_id);
CREATE UNIQUE INDEX idx_23885_pgs_id_skupina ON public.platby_group_skupina USING btree (pgs_id_skupina, pgs_id_group);
CREATE INDEX idx_23885_platby_group_skupina_pgs_id_group_fkey ON public.platby_group_skupina USING btree (pgs_id_group);
CREATE UNIQUE INDEX idx_23886_pgs_id_skupina ON public.platby_group_skupina USING btree (pgs_id_skupina, pgs_id_group);


CREATE TABLE public.platby_item (
    pi_id bigint NOT NULL,
    pi_id_user bigint,
    pi_id_category bigint NOT NULL,
    pi_id_raw bigint,
    pi_amount numeric(10,2) NOT NULL,
    pi_date date NOT NULL,
    pi_prefix integer DEFAULT 2000 NOT NULL
);

CREATE SEQUENCE public.platby_item_pi_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.platby_item_pi_id_seq OWNED BY public.platby_item.pi_id;
ALTER TABLE ONLY public.platby_item ALTER COLUMN pi_id SET DEFAULT nextval('public.platby_item_pi_id_seq'::regclass);
ALTER TABLE ONLY public.platby_item ADD CONSTRAINT idx_23891_primary PRIMARY KEY (pi_id);
CREATE UNIQUE INDEX idx_23891_pi_id_raw ON public.platby_item USING btree (pi_id_raw);
CREATE INDEX idx_23891_platby_item_pi_id_category_fkey ON public.platby_item USING btree (pi_id_category);
CREATE INDEX idx_23891_platby_item_pi_id_user_fkey ON public.platby_item USING btree (pi_id_user);


CREATE TABLE public.platby_raw (
    pr_id bigint NOT NULL,
    pr_raw bytea NOT NULL,
    pr_hash text NOT NULL,
    pr_sorted boolean DEFAULT true NOT NULL,
    pr_discarded boolean DEFAULT true NOT NULL
);

CREATE SEQUENCE public.platby_raw_pr_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.platby_raw_pr_id_seq OWNED BY public.platby_raw.pr_id;
ALTER TABLE ONLY public.platby_raw ALTER COLUMN pr_id SET DEFAULT nextval('public.platby_raw_pr_id_seq'::regclass);
ALTER TABLE ONLY public.platby_raw ADD CONSTRAINT idx_23898_primary PRIMARY KEY (pr_id);
CREATE UNIQUE INDEX idx_23898_pr_hash ON public.platby_raw USING btree (pr_hash);


CREATE TABLE public.rozpis (
    r_id bigint NOT NULL,
    r_trener bigint NOT NULL,
    r_kde text NOT NULL,
    r_datum date NOT NULL,
    r_visible boolean DEFAULT true NOT NULL,
    r_lock boolean DEFAULT true NOT NULL,
    r_timestamp timestamp with time zone
);

CREATE SEQUENCE public.rozpis_r_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.rozpis_r_id_seq OWNED BY public.rozpis.r_id;
ALTER TABLE ONLY public.rozpis ALTER COLUMN r_id SET DEFAULT nextval('public.rozpis_r_id_seq'::regclass);
ALTER TABLE ONLY public.rozpis ADD CONSTRAINT idx_23909_primary PRIMARY KEY (r_id);
CREATE INDEX idx_23909_r_trener ON public.rozpis USING btree (r_trener);

CREATE FUNCTION public.on_update_current_timestamp_rozpis() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
   NEW.r_timestamp = now();
   RETURN NEW;
END;
$$;
CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.rozpis FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_rozpis();

CREATE VIEW public.rozpis_admin AS
 SELECT rozpis.r_id,
    rozpis.r_trener,
    rozpis.r_kde,
    rozpis.r_datum,
    rozpis.r_visible,
    rozpis.r_lock,
    rozpis.r_timestamp
   FROM public.rozpis;


CREATE TABLE public.rozpis_item (
    ri_id bigint NOT NULL,
    ri_id_rodic bigint NOT NULL,
    ri_partner bigint,
    ri_od time without time zone NOT NULL,
    ri_do time without time zone NOT NULL,
    ri_lock boolean DEFAULT true NOT NULL
);

CREATE SEQUENCE public.rozpis_item_ri_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.rozpis_item_ri_id_seq OWNED BY public.rozpis_item.ri_id;
ALTER TABLE ONLY public.rozpis_item ALTER COLUMN ri_id SET DEFAULT nextval('public.rozpis_item_ri_id_seq'::regclass);
ALTER TABLE ONLY public.rozpis_item ADD CONSTRAINT idx_23920_primary PRIMARY KEY (ri_id);
CREATE INDEX idx_23920_rozpis_item_ri_id_rodic_fkey ON public.rozpis_item USING btree (ri_id_rodic);
CREATE INDEX idx_23920_rozpis_item_ri_partner_fkey ON public.rozpis_item USING btree (ri_partner);


CREATE TABLE public.session (
    ss_id character varying(128) NOT NULL,
    ss_data bytea NOT NULL,
    ss_updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ss_lifetime bigint NOT NULL
);
ALTER TABLE ONLY public.session ADD CONSTRAINT idx_23925_primary PRIMARY KEY (ss_id);


CREATE TABLE public.skupiny (
    s_id bigint NOT NULL,
    s_name text NOT NULL,
    s_description text NOT NULL,
    s_color_rgb text NOT NULL,
    s_color_text text NOT NULL
);

CREATE SEQUENCE public.skupiny_s_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.skupiny_s_id_seq OWNED BY public.skupiny.s_id;
ALTER TABLE ONLY public.skupiny ALTER COLUMN s_id SET DEFAULT nextval('public.skupiny_s_id_seq'::regclass);
ALTER TABLE ONLY public.skupiny ADD CONSTRAINT idx_23934_primary PRIMARY KEY (s_id);


CREATE TABLE public.upozorneni (
    up_id bigint NOT NULL,
    up_kdo bigint NOT NULL,
    up_nadpis text NOT NULL,
    up_text text NOT NULL,
    up_barvy bigint DEFAULT '0'::bigint NOT NULL,
    up_lock boolean DEFAULT false NOT NULL,
    up_timestamp timestamp with time zone,
    up_timestamp_add timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE SEQUENCE public.upozorneni_up_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.upozorneni_up_id_seq OWNED BY public.upozorneni.up_id;
ALTER TABLE ONLY public.upozorneni ALTER COLUMN up_id SET DEFAULT nextval('public.upozorneni_up_id_seq'::regclass);
ALTER TABLE ONLY public.upozorneni ADD CONSTRAINT idx_23943_primary PRIMARY KEY (up_id);
CREATE INDEX idx_23943_up_kdo ON public.upozorneni USING btree (up_kdo);
CREATE INDEX idx_23943_up_timestamp_add ON public.upozorneni USING btree (up_timestamp_add);

CREATE FUNCTION public.on_update_current_timestamp_upozorneni() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
   NEW.up_timestamp = now();
   RETURN NEW;
END;
$$;
CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.upozorneni FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_upozorneni();


CREATE TABLE public.upozorneni_skupiny (
    ups_id bigint NOT NULL,
    ups_id_rodic bigint NOT NULL,
    ups_id_skupina bigint NOT NULL,
    ups_color text NOT NULL,
    ups_popis text NOT NULL
);

CREATE SEQUENCE public.upozorneni_skupiny_ups_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.upozorneni_skupiny_ups_id_seq OWNED BY public.upozorneni_skupiny.ups_id;
ALTER TABLE ONLY public.upozorneni_skupiny ALTER COLUMN ups_id SET DEFAULT nextval('public.upozorneni_skupiny_ups_id_seq'::regclass);
ALTER TABLE ONLY public.upozorneni_skupiny ADD CONSTRAINT idx_23955_primary PRIMARY KEY (ups_id);
CREATE INDEX idx_23955_upozorneni_skupiny_ups_id_rodic_fkey ON public.upozorneni_skupiny USING btree (ups_id_rodic);
CREATE INDEX idx_23955_upozorneni_skupiny_ups_id_skupina_fkey ON public.upozorneni_skupiny USING btree (ups_id_skupina);


CREATE TABLE public.users (
    u_id bigint NOT NULL,
    u_login text NOT NULL,
    u_pass character(40) NOT NULL,
    u_jmeno text NOT NULL,
    u_prijmeni text NOT NULL,
    u_pohlavi text NOT NULL,
    u_email text NOT NULL,
    u_telefon text NOT NULL,
    u_narozeni date NOT NULL,
    u_rodne_cislo text,
    u_poznamky text DEFAULT ''::text NOT NULL,
    u_timestamp timestamp with time zone DEFAULT now() NOT NULL,
    u_level smallint DEFAULT '0'::smallint NOT NULL,
    u_group bigint NOT NULL,
    u_skupina bigint DEFAULT '1'::bigint NOT NULL,
    u_dancer boolean DEFAULT true NOT NULL,
    u_ban boolean DEFAULT true NOT NULL,
    u_lock boolean DEFAULT true NOT NULL,
    u_confirmed boolean DEFAULT true NOT NULL,
    u_system boolean DEFAULT true NOT NULL,
    u_street text NOT NULL,
    u_conscription_number text DEFAULT ''::text NOT NULL,
    u_orientation_number text DEFAULT ''::text NOT NULL,
    u_district text DEFAULT ''::text NOT NULL,
    u_city text NOT NULL,
    u_postal_code text NOT NULL,
    u_nationality text NOT NULL,
    u_member_since timestamp with time zone,
    u_member_until timestamp with time zone,
    u_created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    u_teacher boolean DEFAULT true NOT NULL,
    u_gdpr_signed_at timestamp with time zone
);

CREATE SEQUENCE public.users_u_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.users_u_id_seq OWNED BY public.users.u_id;
ALTER TABLE ONLY public.users ALTER COLUMN u_id SET DEFAULT nextval('public.users_u_id_seq'::regclass);
ALTER TABLE ONLY public.users ADD CONSTRAINT idx_23964_primary PRIMARY KEY (u_id);
CREATE UNIQUE INDEX idx_23964_u_login ON public.users USING btree (u_login);
CREATE INDEX idx_23964_u_narozeni ON public.users USING btree (u_narozeni);
CREATE INDEX idx_23964_users_u_group_fkey ON public.users USING btree (u_group);
CREATE INDEX idx_23964_users_u_skupina_fkey ON public.users USING btree (u_skupina);

CREATE FUNCTION public.on_update_current_timestamp_users() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
   NEW.u_timestamp = now();
   RETURN NEW;
END;
$$;
CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.users FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_users();


CREATE TABLE public.users_skupiny (
    us_id bigint NOT NULL,
    us_color character varying(255) DEFAULT 'white'::character varying NOT NULL,
    us_platba_mesic bigint DEFAULT '0'::bigint NOT NULL,
    us_platba_ctvrtrok bigint DEFAULT '0'::bigint NOT NULL,
    us_platba_pulrok bigint DEFAULT '0'::bigint NOT NULL,
    us_popis text NOT NULL
);

CREATE SEQUENCE public.users_skupiny_us_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.users_skupiny_us_id_seq OWNED BY public.users_skupiny.us_id;
ALTER TABLE ONLY public.users_skupiny ALTER COLUMN us_id SET DEFAULT nextval('public.users_skupiny_us_id_seq'::regclass);
ALTER TABLE ONLY public.users_skupiny ADD CONSTRAINT idx_23986_primary PRIMARY KEY (us_id);


CREATE TABLE public.video (
    v_id bigint NOT NULL,
    v_uri text NOT NULL,
    v_title text NOT NULL,
    v_author text NOT NULL,
    v_description text NOT NULL,
    v_playlist text,
    v_created_at timestamp with time zone NOT NULL,
    v_updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE SEQUENCE public.video_v_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.video_v_id_seq OWNED BY public.video.v_id;
ALTER TABLE ONLY public.video ALTER COLUMN v_id SET DEFAULT nextval('public.video_v_id_seq'::regclass);
ALTER TABLE ONLY public.video ADD CONSTRAINT idx_23999_primary PRIMARY KEY (v_id);


CREATE TABLE public.video_list (
    vl_id bigint NOT NULL,
    vl_url text NOT NULL,
    vl_title text NOT NULL,
    vl_description text NOT NULL,
    vl_count bigint NOT NULL,
    vl_created_at timestamp with time zone NOT NULL,
    vl_last_checked timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);

CREATE SEQUENCE public.video_list_vl_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.video_list_vl_id_seq OWNED BY public.video_list.vl_id;
ALTER TABLE ONLY public.video_list ALTER COLUMN vl_id SET DEFAULT nextval('public.video_list_vl_id_seq'::regclass);
ALTER TABLE ONLY public.video_list ADD CONSTRAINT idx_24009_primary PRIMARY KEY (vl_id);


CREATE TABLE public.video_source (
    vs_id bigint NOT NULL,
    vs_url text NOT NULL,
    vs_title text,
    vs_description text,
    vs_created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    vs_last_checked timestamp with time zone
);

CREATE SEQUENCE public.video_source_vs_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.video_source_vs_id_seq OWNED BY public.video_source.vs_id;
ALTER TABLE ONLY public.video_source ALTER COLUMN vs_id SET DEFAULT nextval('public.video_source_vs_id_seq'::regclass);
ALTER TABLE ONLY public.video_source ADD CONSTRAINT idx_24019_primary PRIMARY KEY (vs_id);


ALTER TABLE ONLY public.akce_item
    ADD CONSTRAINT akce_item_ai_id_rodic_fkey FOREIGN KEY (ai_id_rodic) REFERENCES public.akce(a_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.akce_item
    ADD CONSTRAINT akce_item_ai_user_fkey FOREIGN KEY (ai_user) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_at_foto_main_fkey FOREIGN KEY (at_foto_main) REFERENCES public.galerie_foto(gf_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_at_kdo_fkey FOREIGN KEY (at_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.dokumenty
    ADD CONSTRAINT dokumenty_d_kdo_fkey FOREIGN KEY (d_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT galerie_foto_gf_id_rodic_fkey FOREIGN KEY (gf_id_rodic) REFERENCES public.galerie_dir(gd_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT galerie_foto_gf_kdo_fkey FOREIGN KEY (gf_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.nabidka_item
    ADD CONSTRAINT nabidka_item_ni_id_rodic_fkey FOREIGN KEY (ni_id_rodic) REFERENCES public.nabidka(n_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.nabidka_item
    ADD CONSTRAINT nabidka_item_ni_partner_fkey FOREIGN KEY (ni_partner) REFERENCES public.pary(p_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.nabidka
    ADD CONSTRAINT nabidka_n_trener_fkey FOREIGN KEY (n_trener) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_navrhl_fkey FOREIGN KEY (pn_navrhl) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_partner_fkey FOREIGN KEY (pn_partner) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_partnerka_fkey FOREIGN KEY (pn_partnerka) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.pary
    ADD CONSTRAINT pary_p_id_partner_fkey FOREIGN KEY (p_id_partner) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.platby_category_group
    ADD CONSTRAINT platby_category_group_pcg_id_category_fkey FOREIGN KEY (pcg_id_category) REFERENCES public.platby_category(pc_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.platby_category_group
    ADD CONSTRAINT platby_category_group_pcg_id_group_fkey FOREIGN KEY (pcg_id_group) REFERENCES public.platby_group(pg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_pgs_id_group_fkey FOREIGN KEY (pgs_id_group) REFERENCES public.platby_group(pg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_pgs_id_skupina_fkey FOREIGN KEY (pgs_id_skupina) REFERENCES public.skupiny(s_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.platby_item
    ADD CONSTRAINT platby_item_pi_id_category_fkey FOREIGN KEY (pi_id_category) REFERENCES public.platby_category(pc_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.platby_item
    ADD CONSTRAINT platby_item_pi_id_raw_fkey FOREIGN KEY (pi_id_raw) REFERENCES public.platby_raw(pr_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.platby_item
    ADD CONSTRAINT platby_item_pi_id_user_fkey FOREIGN KEY (pi_id_user) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.rozpis_item
    ADD CONSTRAINT rozpis_item_ri_id_rodic_fkey FOREIGN KEY (ri_id_rodic) REFERENCES public.rozpis(r_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.rozpis_item
    ADD CONSTRAINT rozpis_item_ri_partner_fkey FOREIGN KEY (ri_partner) REFERENCES public.pary(p_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.rozpis
    ADD CONSTRAINT rozpis_r_trener_fkey FOREIGN KEY (r_trener) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT upozorneni_skupiny_ups_id_rodic_fkey FOREIGN KEY (ups_id_rodic) REFERENCES public.upozorneni(up_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT upozorneni_skupiny_ups_id_skupina_fkey FOREIGN KEY (ups_id_skupina) REFERENCES public.skupiny(s_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.upozorneni
    ADD CONSTRAINT upozorneni_up_kdo_fkey FOREIGN KEY (up_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_u_group_fkey FOREIGN KEY (u_group) REFERENCES public.permissions(pe_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_u_skupina_fkey FOREIGN KEY (u_skupina) REFERENCES public.skupiny(s_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
