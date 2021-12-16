--
-- PostgreSQL database dump
--

-- Dumped from database version 11.13
-- Dumped by pg_dump version 11.13

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

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- Name: pary_p_lat_trida; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.pary_p_lat_trida AS ENUM (
    'Z',
    'H',
    'D',
    'C',
    'B',
    'A',
    'M'
);


--
-- Name: pary_p_stt_trida; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.pary_p_stt_trida AS ENUM (
    'Z',
    'H',
    'D',
    'C',
    'B',
    'A',
    'M'
);


--
-- Name: current_user_id(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_user_id() RETURNS text
    LANGUAGE sql STABLE
    AS $$
  SELECT current_setting('jwt.claims.user_id', true);
$$;


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

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


--
-- Name: get_current_user(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.get_current_user() RETURNS public.users
    LANGUAGE sql STABLE
    AS $$
  SELECT * FROM users WHERE u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$;


--
-- Name: on_update_current_timestamp_akce(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.on_update_current_timestamp_akce() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.a_timestamp = now();
   RETURN NEW;
END;
$$;


--
-- Name: on_update_current_timestamp_aktuality(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.on_update_current_timestamp_aktuality() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.at_timestamp = now();
   RETURN NEW;
END;
$$;


--
-- Name: on_update_current_timestamp_dokumenty(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.on_update_current_timestamp_dokumenty() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.d_timestamp = now();
   RETURN NEW;
END;
$$;


--
-- Name: on_update_current_timestamp_galerie_foto(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.on_update_current_timestamp_galerie_foto() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.gf_timestamp = now();
   RETURN NEW;
END;
$$;


--
-- Name: on_update_current_timestamp_nabidka(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.on_update_current_timestamp_nabidka() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.n_timestamp = now();
   RETURN NEW;
END;
$$;


--
-- Name: on_update_current_timestamp_rozpis(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.on_update_current_timestamp_rozpis() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.r_timestamp = now();
   RETURN NEW;
END;
$$;


--
-- Name: on_update_current_timestamp_upozorneni(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.on_update_current_timestamp_upozorneni() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.up_timestamp = now();
   RETURN NEW;
END;
$$;


--
-- Name: on_update_current_timestamp_users(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.on_update_current_timestamp_users() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.u_timestamp = now();
   RETURN NEW;
END;
$$;


--
-- Name: akce; Type: TABLE; Schema: public; Owner: -
--

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


--
-- Name: akce_a_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.akce_a_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: akce_a_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.akce_a_id_seq OWNED BY public.akce.a_id;


--
-- Name: akce_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.akce_item (
    ai_id bigint NOT NULL,
    ai_id_rodic bigint NOT NULL,
    ai_user bigint NOT NULL,
    ai_rok_narozeni smallint NOT NULL
);


--
-- Name: akce_item_ai_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.akce_item_ai_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: akce_item_ai_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.akce_item_ai_id_seq OWNED BY public.akce_item.ai_id;


--
-- Name: aktuality; Type: TABLE; Schema: public; Owner: -
--

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


--
-- Name: aktuality_admin; Type: VIEW; Schema: public; Owner: -
--

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


--
-- Name: aktuality_at_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.aktuality_at_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: aktuality_at_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.aktuality_at_id_seq OWNED BY public.aktuality.at_id;


--
-- Name: dokumenty; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.dokumenty (
    d_id bigint NOT NULL,
    d_path text NOT NULL,
    d_name text NOT NULL,
    d_filename text NOT NULL,
    d_kategorie smallint NOT NULL,
    d_kdo bigint NOT NULL,
    d_timestamp timestamp with time zone
);


--
-- Name: dokumenty_d_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.dokumenty_d_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: dokumenty_d_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.dokumenty_d_id_seq OWNED BY public.dokumenty.d_id;


--
-- Name: galerie_dir; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.galerie_dir (
    gd_id bigint NOT NULL,
    gd_id_rodic bigint NOT NULL,
    gd_name text NOT NULL,
    gd_level smallint DEFAULT '1'::smallint NOT NULL,
    gd_path text NOT NULL,
    gd_hidden boolean DEFAULT true NOT NULL
);


--
-- Name: galerie_dir_gd_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.galerie_dir_gd_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: galerie_dir_gd_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.galerie_dir_gd_id_seq OWNED BY public.galerie_dir.gd_id;


--
-- Name: galerie_foto; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.galerie_foto (
    gf_id bigint NOT NULL,
    gf_id_rodic bigint NOT NULL,
    gf_name text NOT NULL,
    gf_path text NOT NULL,
    gf_kdo bigint NOT NULL,
    gf_timestamp timestamp with time zone
);


--
-- Name: galerie_foto_gf_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.galerie_foto_gf_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: galerie_foto_gf_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.galerie_foto_gf_id_seq OWNED BY public.galerie_foto.gf_id;


--
-- Name: platby_category; Type: TABLE; Schema: public; Owner: -
--

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


--
-- Name: platby_category_group; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.platby_category_group (
    pcg_id bigint NOT NULL,
    pcg_id_group bigint NOT NULL,
    pcg_id_category bigint NOT NULL
);


--
-- Name: platby_group; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.platby_group (
    pg_id bigint NOT NULL,
    pg_type numeric DEFAULT '1'::numeric NOT NULL,
    pg_name text NOT NULL,
    pg_description text NOT NULL,
    pg_base bigint DEFAULT '0'::bigint NOT NULL
);


--
-- Name: platby_group_skupina; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.platby_group_skupina (
    pgs_id bigint NOT NULL,
    pgs_id_skupina bigint NOT NULL,
    pgs_id_group bigint NOT NULL
);


--
-- Name: platby_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.platby_item (
    pi_id bigint NOT NULL,
    pi_id_user bigint,
    pi_id_category bigint NOT NULL,
    pi_id_raw bigint,
    pi_amount numeric(10,2) NOT NULL,
    pi_date date NOT NULL,
    pi_prefix integer DEFAULT 2000 NOT NULL
);


--
-- Name: skupiny; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.skupiny (
    s_id bigint NOT NULL,
    s_name text NOT NULL,
    s_description text NOT NULL,
    s_color_rgb text NOT NULL,
    s_color_text text NOT NULL
);


--
-- Name: members; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.members AS
 SELECT DISTINCT ON (users.u_id) users.u_email
   FROM ((((((public.platby_item
     JOIN public.platby_category ON ((platby_category.pc_id = platby_item.pi_id_category)))
     JOIN public.platby_category_group ON ((platby_category_group.pcg_id_category = platby_category.pc_id)))
     JOIN public.platby_group ON ((platby_group.pg_id = platby_category_group.pcg_id_group)))
     JOIN public.platby_group_skupina ON ((platby_group_skupina.pgs_id_group = platby_group.pg_id)))
     JOIN public.skupiny ON ((platby_group_skupina.pgs_id_skupina = skupiny.s_id)))
     JOIN public.users ON (((platby_item.pi_id_user = users.u_id) AND (users.u_skupina = skupiny.s_id))))
  WHERE ((platby_group.pg_type = '1'::numeric) AND (CURRENT_DATE >= platby_category.pc_valid_from) AND (CURRENT_DATE <= platby_category.pc_valid_to) AND (users.u_confirmed = true) AND (users.u_ban = false) AND (users.u_system = false));


--
-- Name: nabidka; Type: TABLE; Schema: public; Owner: -
--

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


--
-- Name: nabidka_admin; Type: VIEW; Schema: public; Owner: -
--

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


--
-- Name: nabidka_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.nabidka_item (
    ni_id bigint NOT NULL,
    ni_id_rodic bigint NOT NULL,
    ni_partner bigint NOT NULL,
    ni_pocet_hod smallint DEFAULT '1'::smallint NOT NULL,
    ni_lock boolean DEFAULT true NOT NULL
);


--
-- Name: nabidka_item_ni_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.nabidka_item_ni_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: nabidka_item_ni_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.nabidka_item_ni_id_seq OWNED BY public.nabidka_item.ni_id;


--
-- Name: nabidka_n_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.nabidka_n_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: nabidka_n_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.nabidka_n_id_seq OWNED BY public.nabidka.n_id;


--
-- Name: parameters; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.parameters (
    pa_name character varying(40) NOT NULL,
    pa_value text NOT NULL
);


--
-- Name: pary; Type: TABLE; Schema: public; Owner: -
--

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


--
-- Name: pary_navrh; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pary_navrh (
    pn_id bigint NOT NULL,
    pn_navrhl bigint NOT NULL,
    pn_partner bigint NOT NULL,
    pn_partnerka bigint NOT NULL
);


--
-- Name: pary_navrh_pn_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.pary_navrh_pn_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: pary_navrh_pn_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.pary_navrh_pn_id_seq OWNED BY public.pary_navrh.pn_id;


--
-- Name: pary_p_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.pary_p_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: pary_p_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.pary_p_id_seq OWNED BY public.pary.p_id;


--
-- Name: permissions; Type: TABLE; Schema: public; Owner: -
--

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


--
-- Name: permissions_pe_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.permissions_pe_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: permissions_pe_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.permissions_pe_id_seq OWNED BY public.permissions.pe_id;


--
-- Name: platby_category_group_pcg_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.platby_category_group_pcg_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: platby_category_group_pcg_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.platby_category_group_pcg_id_seq OWNED BY public.platby_category_group.pcg_id;


--
-- Name: platby_category_pc_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.platby_category_pc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: platby_category_pc_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.platby_category_pc_id_seq OWNED BY public.platby_category.pc_id;


--
-- Name: platby_group_pg_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.platby_group_pg_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: platby_group_pg_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.platby_group_pg_id_seq OWNED BY public.platby_group.pg_id;


--
-- Name: platby_group_skupina_pgs_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.platby_group_skupina_pgs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: platby_group_skupina_pgs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.platby_group_skupina_pgs_id_seq OWNED BY public.platby_group_skupina.pgs_id;


--
-- Name: platby_item_pi_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.platby_item_pi_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: platby_item_pi_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.platby_item_pi_id_seq OWNED BY public.platby_item.pi_id;


--
-- Name: platby_raw; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.platby_raw (
    pr_id bigint NOT NULL,
    pr_raw bytea NOT NULL,
    pr_hash text NOT NULL,
    pr_sorted boolean DEFAULT true NOT NULL,
    pr_discarded boolean DEFAULT true NOT NULL
);


--
-- Name: platby_raw_pr_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.platby_raw_pr_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: platby_raw_pr_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.platby_raw_pr_id_seq OWNED BY public.platby_raw.pr_id;


--
-- Name: rozpis; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.rozpis (
    r_id bigint NOT NULL,
    r_trener bigint NOT NULL,
    r_kde text NOT NULL,
    r_datum date NOT NULL,
    r_visible boolean DEFAULT true NOT NULL,
    r_lock boolean DEFAULT true NOT NULL,
    r_timestamp timestamp with time zone
);


--
-- Name: rozpis_admin; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.rozpis_admin AS
 SELECT rozpis.r_id,
    rozpis.r_trener,
    rozpis.r_kde,
    rozpis.r_datum,
    rozpis.r_visible,
    rozpis.r_lock,
    rozpis.r_timestamp
   FROM public.rozpis;


--
-- Name: rozpis_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.rozpis_item (
    ri_id bigint NOT NULL,
    ri_id_rodic bigint NOT NULL,
    ri_partner bigint,
    ri_od time without time zone NOT NULL,
    ri_do time without time zone NOT NULL,
    ri_lock boolean DEFAULT true NOT NULL
);


--
-- Name: rozpis_item_ri_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.rozpis_item_ri_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: rozpis_item_ri_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.rozpis_item_ri_id_seq OWNED BY public.rozpis_item.ri_id;


--
-- Name: rozpis_r_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.rozpis_r_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: rozpis_r_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.rozpis_r_id_seq OWNED BY public.rozpis.r_id;


--
-- Name: session; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.session (
    ss_id character varying(128) NOT NULL,
    ss_data bytea NOT NULL,
    ss_updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ss_lifetime bigint NOT NULL
);


--
-- Name: skupiny_s_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.skupiny_s_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: skupiny_s_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.skupiny_s_id_seq OWNED BY public.skupiny.s_id;


--
-- Name: upozorneni; Type: TABLE; Schema: public; Owner: -
--

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


--
-- Name: upozorneni_skupiny; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.upozorneni_skupiny (
    ups_id bigint NOT NULL,
    ups_id_rodic bigint NOT NULL,
    ups_id_skupina bigint NOT NULL,
    ups_color text NOT NULL,
    ups_popis text NOT NULL
);


--
-- Name: upozorneni_skupiny_ups_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.upozorneni_skupiny_ups_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: upozorneni_skupiny_ups_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.upozorneni_skupiny_ups_id_seq OWNED BY public.upozorneni_skupiny.ups_id;


--
-- Name: upozorneni_up_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.upozorneni_up_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: upozorneni_up_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.upozorneni_up_id_seq OWNED BY public.upozorneni.up_id;


--
-- Name: users_skupiny; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users_skupiny (
    us_id bigint NOT NULL,
    us_color character varying(255) DEFAULT 'white'::character varying NOT NULL,
    us_platba_mesic bigint DEFAULT '0'::bigint NOT NULL,
    us_platba_ctvrtrok bigint DEFAULT '0'::bigint NOT NULL,
    us_platba_pulrok bigint DEFAULT '0'::bigint NOT NULL,
    us_popis text NOT NULL
);


--
-- Name: users_skupiny_us_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.users_skupiny_us_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: users_skupiny_us_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.users_skupiny_us_id_seq OWNED BY public.users_skupiny.us_id;


--
-- Name: users_u_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.users_u_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: users_u_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.users_u_id_seq OWNED BY public.users.u_id;


--
-- Name: video; Type: TABLE; Schema: public; Owner: -
--

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


--
-- Name: video_list; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.video_list (
    vl_id bigint NOT NULL,
    vl_url text NOT NULL,
    vl_title text NOT NULL,
    vl_description text NOT NULL,
    vl_count bigint NOT NULL,
    vl_created_at timestamp with time zone NOT NULL,
    vl_last_checked timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: video_list_vl_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.video_list_vl_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: video_list_vl_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.video_list_vl_id_seq OWNED BY public.video_list.vl_id;


--
-- Name: video_source; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.video_source (
    vs_id bigint NOT NULL,
    vs_url text NOT NULL,
    vs_title text,
    vs_description text,
    vs_created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    vs_last_checked timestamp with time zone
);


--
-- Name: video_source_vs_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.video_source_vs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: video_source_vs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.video_source_vs_id_seq OWNED BY public.video_source.vs_id;


--
-- Name: video_v_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.video_v_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: video_v_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.video_v_id_seq OWNED BY public.video.v_id;


--
-- Name: akce a_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.akce ALTER COLUMN a_id SET DEFAULT nextval('public.akce_a_id_seq'::regclass);


--
-- Name: akce_item ai_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.akce_item ALTER COLUMN ai_id SET DEFAULT nextval('public.akce_item_ai_id_seq'::regclass);


--
-- Name: aktuality at_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality ALTER COLUMN at_id SET DEFAULT nextval('public.aktuality_at_id_seq'::regclass);


--
-- Name: dokumenty d_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dokumenty ALTER COLUMN d_id SET DEFAULT nextval('public.dokumenty_d_id_seq'::regclass);


--
-- Name: galerie_dir gd_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.galerie_dir ALTER COLUMN gd_id SET DEFAULT nextval('public.galerie_dir_gd_id_seq'::regclass);


--
-- Name: galerie_foto gf_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.galerie_foto ALTER COLUMN gf_id SET DEFAULT nextval('public.galerie_foto_gf_id_seq'::regclass);


--
-- Name: nabidka n_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nabidka ALTER COLUMN n_id SET DEFAULT nextval('public.nabidka_n_id_seq'::regclass);


--
-- Name: nabidka_item ni_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nabidka_item ALTER COLUMN ni_id SET DEFAULT nextval('public.nabidka_item_ni_id_seq'::regclass);


--
-- Name: pary p_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pary ALTER COLUMN p_id SET DEFAULT nextval('public.pary_p_id_seq'::regclass);


--
-- Name: pary_navrh pn_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pary_navrh ALTER COLUMN pn_id SET DEFAULT nextval('public.pary_navrh_pn_id_seq'::regclass);


--
-- Name: permissions pe_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.permissions ALTER COLUMN pe_id SET DEFAULT nextval('public.permissions_pe_id_seq'::regclass);


--
-- Name: platby_category pc_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_category ALTER COLUMN pc_id SET DEFAULT nextval('public.platby_category_pc_id_seq'::regclass);


--
-- Name: platby_category_group pcg_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_category_group ALTER COLUMN pcg_id SET DEFAULT nextval('public.platby_category_group_pcg_id_seq'::regclass);


--
-- Name: platby_group pg_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group ALTER COLUMN pg_id SET DEFAULT nextval('public.platby_group_pg_id_seq'::regclass);


--
-- Name: platby_group_skupina pgs_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group_skupina ALTER COLUMN pgs_id SET DEFAULT nextval('public.platby_group_skupina_pgs_id_seq'::regclass);


--
-- Name: platby_item pi_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_item ALTER COLUMN pi_id SET DEFAULT nextval('public.platby_item_pi_id_seq'::regclass);


--
-- Name: platby_raw pr_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_raw ALTER COLUMN pr_id SET DEFAULT nextval('public.platby_raw_pr_id_seq'::regclass);


--
-- Name: rozpis r_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rozpis ALTER COLUMN r_id SET DEFAULT nextval('public.rozpis_r_id_seq'::regclass);


--
-- Name: rozpis_item ri_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rozpis_item ALTER COLUMN ri_id SET DEFAULT nextval('public.rozpis_item_ri_id_seq'::regclass);


--
-- Name: skupiny s_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.skupiny ALTER COLUMN s_id SET DEFAULT nextval('public.skupiny_s_id_seq'::regclass);


--
-- Name: upozorneni up_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni ALTER COLUMN up_id SET DEFAULT nextval('public.upozorneni_up_id_seq'::regclass);


--
-- Name: upozorneni_skupiny ups_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni_skupiny ALTER COLUMN ups_id SET DEFAULT nextval('public.upozorneni_skupiny_ups_id_seq'::regclass);


--
-- Name: users u_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users ALTER COLUMN u_id SET DEFAULT nextval('public.users_u_id_seq'::regclass);


--
-- Name: users_skupiny us_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users_skupiny ALTER COLUMN us_id SET DEFAULT nextval('public.users_skupiny_us_id_seq'::regclass);


--
-- Name: video v_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.video ALTER COLUMN v_id SET DEFAULT nextval('public.video_v_id_seq'::regclass);


--
-- Name: video_list vl_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.video_list ALTER COLUMN vl_id SET DEFAULT nextval('public.video_list_vl_id_seq'::regclass);


--
-- Name: video_source vs_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.video_source ALTER COLUMN vs_id SET DEFAULT nextval('public.video_source_vs_id_seq'::regclass);


--
-- Name: akce idx_23735_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.akce
    ADD CONSTRAINT idx_23735_primary PRIMARY KEY (a_id);


--
-- Name: akce_item idx_23747_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.akce_item
    ADD CONSTRAINT idx_23747_primary PRIMARY KEY (ai_id);


--
-- Name: aktuality idx_23753_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT idx_23753_primary PRIMARY KEY (at_id);


--
-- Name: dokumenty idx_23771_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dokumenty
    ADD CONSTRAINT idx_23771_primary PRIMARY KEY (d_id);


--
-- Name: galerie_dir idx_23780_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.galerie_dir
    ADD CONSTRAINT idx_23780_primary PRIMARY KEY (gd_id);


--
-- Name: galerie_foto idx_23791_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT idx_23791_primary PRIMARY KEY (gf_id);


--
-- Name: nabidka idx_23800_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nabidka
    ADD CONSTRAINT idx_23800_primary PRIMARY KEY (n_id);


--
-- Name: nabidka_item idx_23810_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nabidka_item
    ADD CONSTRAINT idx_23810_primary PRIMARY KEY (ni_id);


--
-- Name: parameters idx_23816_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameters
    ADD CONSTRAINT idx_23816_primary PRIMARY KEY (pa_name);


--
-- Name: pary idx_23824_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pary
    ADD CONSTRAINT idx_23824_primary PRIMARY KEY (p_id);


--
-- Name: pary_navrh idx_23840_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pary_navrh
    ADD CONSTRAINT idx_23840_primary PRIMARY KEY (pn_id);


--
-- Name: permissions idx_23846_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.permissions
    ADD CONSTRAINT idx_23846_primary PRIMARY KEY (pe_id);


--
-- Name: platby_category idx_23855_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_category
    ADD CONSTRAINT idx_23855_primary PRIMARY KEY (pc_id);


--
-- Name: platby_category_group idx_23868_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_category_group
    ADD CONSTRAINT idx_23868_primary PRIMARY KEY (pcg_id);


--
-- Name: platby_group idx_23874_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group
    ADD CONSTRAINT idx_23874_primary PRIMARY KEY (pg_id);


--
-- Name: platby_group_skupina idx_23885_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group_skupina
    ADD CONSTRAINT idx_23885_primary PRIMARY KEY (pgs_id);


--
-- Name: platby_item idx_23891_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_item
    ADD CONSTRAINT idx_23891_primary PRIMARY KEY (pi_id);


--
-- Name: platby_raw idx_23898_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_raw
    ADD CONSTRAINT idx_23898_primary PRIMARY KEY (pr_id);


--
-- Name: rozpis idx_23909_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rozpis
    ADD CONSTRAINT idx_23909_primary PRIMARY KEY (r_id);


--
-- Name: rozpis_item idx_23920_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rozpis_item
    ADD CONSTRAINT idx_23920_primary PRIMARY KEY (ri_id);


--
-- Name: session idx_23925_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session
    ADD CONSTRAINT idx_23925_primary PRIMARY KEY (ss_id);


--
-- Name: skupiny idx_23934_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.skupiny
    ADD CONSTRAINT idx_23934_primary PRIMARY KEY (s_id);


--
-- Name: upozorneni idx_23943_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni
    ADD CONSTRAINT idx_23943_primary PRIMARY KEY (up_id);


--
-- Name: upozorneni_skupiny idx_23955_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT idx_23955_primary PRIMARY KEY (ups_id);


--
-- Name: users idx_23964_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT idx_23964_primary PRIMARY KEY (u_id);


--
-- Name: users_skupiny idx_23986_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users_skupiny
    ADD CONSTRAINT idx_23986_primary PRIMARY KEY (us_id);


--
-- Name: video idx_23999_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.video
    ADD CONSTRAINT idx_23999_primary PRIMARY KEY (v_id);


--
-- Name: video_list idx_24009_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.video_list
    ADD CONSTRAINT idx_24009_primary PRIMARY KEY (vl_id);


--
-- Name: video_source idx_24019_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.video_source
    ADD CONSTRAINT idx_24019_primary PRIMARY KEY (vs_id);


--
-- Name: idx_23747_akce_item_ai_id_rodic_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23747_akce_item_ai_id_rodic_fkey ON public.akce_item USING btree (ai_id_rodic);


--
-- Name: idx_23747_akce_item_ai_user_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23747_akce_item_ai_user_fkey ON public.akce_item USING btree (ai_user);


--
-- Name: idx_23753_aktuality_at_foto_main_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23753_aktuality_at_foto_main_fkey ON public.aktuality USING btree (at_foto_main);


--
-- Name: idx_23753_aktuality_at_kdo_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23753_aktuality_at_kdo_fkey ON public.aktuality USING btree (at_kdo);


--
-- Name: idx_23753_at_timestamp_add; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23753_at_timestamp_add ON public.aktuality USING btree (at_timestamp_add);


--
-- Name: idx_23771_d_path; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_23771_d_path ON public.dokumenty USING btree (d_path);


--
-- Name: idx_23771_dokumenty_d_kdo_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23771_dokumenty_d_kdo_fkey ON public.dokumenty USING btree (d_kdo);


--
-- Name: idx_23780_gd_id_rodic; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23780_gd_id_rodic ON public.galerie_dir USING btree (gd_id_rodic);


--
-- Name: idx_23791_galerie_foto_gf_kdo_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23791_galerie_foto_gf_kdo_fkey ON public.galerie_foto USING btree (gf_kdo);


--
-- Name: idx_23791_gf_id_rodic; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23791_gf_id_rodic ON public.galerie_foto USING btree (gf_id_rodic);


--
-- Name: idx_23800_n_trener; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23800_n_trener ON public.nabidka USING btree (n_trener);


--
-- Name: idx_23810_nabidka_item_ni_partner_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23810_nabidka_item_ni_partner_fkey ON public.nabidka_item USING btree (ni_partner);


--
-- Name: idx_23810_ni_id_rodic; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_23810_ni_id_rodic ON public.nabidka_item USING btree (ni_id_rodic, ni_partner);


--
-- Name: idx_23824_p_hodnoceni; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23824_p_hodnoceni ON public.pary USING btree (p_hodnoceni);


--
-- Name: idx_23824_pary_p_id_partner_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23824_pary_p_id_partner_fkey ON public.pary USING btree (p_id_partner);


--
-- Name: idx_23824_pary_p_id_partnerka_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23824_pary_p_id_partnerka_fkey ON public.pary USING btree (p_id_partnerka);


--
-- Name: idx_23840_pary_navrh_pn_navrhl_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23840_pary_navrh_pn_navrhl_fkey ON public.pary_navrh USING btree (pn_navrhl);


--
-- Name: idx_23840_pary_navrh_pn_partner_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23840_pary_navrh_pn_partner_fkey ON public.pary_navrh USING btree (pn_partner);


--
-- Name: idx_23840_pary_navrh_pn_partnerka_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23840_pary_navrh_pn_partnerka_fkey ON public.pary_navrh USING btree (pn_partnerka);


--
-- Name: idx_23855_pc_symbol; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_23855_pc_symbol ON public.platby_category USING btree (pc_symbol);


--
-- Name: idx_23868_pcg_id_group; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_23868_pcg_id_group ON public.platby_category_group USING btree (pcg_id_group, pcg_id_category);


--
-- Name: idx_23868_platby_category_group_pcg_id_category_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23868_platby_category_group_pcg_id_category_fkey ON public.platby_category_group USING btree (pcg_id_category);


--
-- Name: idx_23885_pgs_id_skupina; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_23885_pgs_id_skupina ON public.platby_group_skupina USING btree (pgs_id_skupina, pgs_id_group);


--
-- Name: idx_23885_platby_group_skupina_pgs_id_group_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23885_platby_group_skupina_pgs_id_group_fkey ON public.platby_group_skupina USING btree (pgs_id_group);


--
-- Name: idx_23886_pgs_id_skupina; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_23886_pgs_id_skupina ON public.platby_group_skupina USING btree (pgs_id_skupina, pgs_id_group);


--
-- Name: idx_23891_pi_id_raw; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_23891_pi_id_raw ON public.platby_item USING btree (pi_id_raw);


--
-- Name: idx_23891_platby_item_pi_id_category_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23891_platby_item_pi_id_category_fkey ON public.platby_item USING btree (pi_id_category);


--
-- Name: idx_23891_platby_item_pi_id_user_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23891_platby_item_pi_id_user_fkey ON public.platby_item USING btree (pi_id_user);


--
-- Name: idx_23898_pr_hash; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_23898_pr_hash ON public.platby_raw USING btree (pr_hash);


--
-- Name: idx_23909_r_trener; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23909_r_trener ON public.rozpis USING btree (r_trener);


--
-- Name: idx_23920_rozpis_item_ri_id_rodic_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23920_rozpis_item_ri_id_rodic_fkey ON public.rozpis_item USING btree (ri_id_rodic);


--
-- Name: idx_23920_rozpis_item_ri_partner_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23920_rozpis_item_ri_partner_fkey ON public.rozpis_item USING btree (ri_partner);


--
-- Name: idx_23943_up_kdo; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23943_up_kdo ON public.upozorneni USING btree (up_kdo);


--
-- Name: idx_23943_up_timestamp_add; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23943_up_timestamp_add ON public.upozorneni USING btree (up_timestamp_add);


--
-- Name: idx_23955_upozorneni_skupiny_ups_id_rodic_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23955_upozorneni_skupiny_ups_id_rodic_fkey ON public.upozorneni_skupiny USING btree (ups_id_rodic);


--
-- Name: idx_23955_upozorneni_skupiny_ups_id_skupina_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23955_upozorneni_skupiny_ups_id_skupina_fkey ON public.upozorneni_skupiny USING btree (ups_id_skupina);


--
-- Name: idx_23964_u_login; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_23964_u_login ON public.users USING btree (u_login);


--
-- Name: idx_23964_u_narozeni; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23964_u_narozeni ON public.users USING btree (u_narozeni);


--
-- Name: idx_23964_users_u_group_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23964_users_u_group_fkey ON public.users USING btree (u_group);


--
-- Name: idx_23964_users_u_skupina_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23964_users_u_skupina_fkey ON public.users USING btree (u_skupina);


--
-- Name: akce on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.akce FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_akce();


--
-- Name: aktuality on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.aktuality FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_aktuality();


--
-- Name: dokumenty on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.dokumenty FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_dokumenty();


--
-- Name: galerie_foto on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.galerie_foto FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_galerie_foto();


--
-- Name: nabidka on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.nabidka FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_nabidka();


--
-- Name: rozpis on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.rozpis FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_rozpis();


--
-- Name: upozorneni on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.upozorneni FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_upozorneni();


--
-- Name: users on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.users FOR EACH ROW EXECUTE PROCEDURE public.on_update_current_timestamp_users();


--
-- Name: akce_item akce_item_ai_id_rodic_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.akce_item
    ADD CONSTRAINT akce_item_ai_id_rodic_fkey FOREIGN KEY (ai_id_rodic) REFERENCES public.akce(a_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: akce_item akce_item_ai_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.akce_item
    ADD CONSTRAINT akce_item_ai_user_fkey FOREIGN KEY (ai_user) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: aktuality aktuality_at_foto_main_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_at_foto_main_fkey FOREIGN KEY (at_foto_main) REFERENCES public.galerie_foto(gf_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: aktuality aktuality_at_kdo_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_at_kdo_fkey FOREIGN KEY (at_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: dokumenty dokumenty_d_kdo_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dokumenty
    ADD CONSTRAINT dokumenty_d_kdo_fkey FOREIGN KEY (d_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: galerie_foto galerie_foto_gf_id_rodic_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT galerie_foto_gf_id_rodic_fkey FOREIGN KEY (gf_id_rodic) REFERENCES public.galerie_dir(gd_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: galerie_foto galerie_foto_gf_kdo_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT galerie_foto_gf_kdo_fkey FOREIGN KEY (gf_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: nabidka_item nabidka_item_ni_id_rodic_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nabidka_item
    ADD CONSTRAINT nabidka_item_ni_id_rodic_fkey FOREIGN KEY (ni_id_rodic) REFERENCES public.nabidka(n_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: nabidka_item nabidka_item_ni_partner_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nabidka_item
    ADD CONSTRAINT nabidka_item_ni_partner_fkey FOREIGN KEY (ni_partner) REFERENCES public.pary(p_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: nabidka nabidka_n_trener_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nabidka
    ADD CONSTRAINT nabidka_n_trener_fkey FOREIGN KEY (n_trener) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: pary_navrh pary_navrh_pn_navrhl_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_navrhl_fkey FOREIGN KEY (pn_navrhl) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: pary_navrh pary_navrh_pn_partner_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_partner_fkey FOREIGN KEY (pn_partner) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: pary_navrh pary_navrh_pn_partnerka_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_partnerka_fkey FOREIGN KEY (pn_partnerka) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: pary pary_p_id_partner_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pary
    ADD CONSTRAINT pary_p_id_partner_fkey FOREIGN KEY (p_id_partner) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_category_group platby_category_group_pcg_id_category_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_category_group
    ADD CONSTRAINT platby_category_group_pcg_id_category_fkey FOREIGN KEY (pcg_id_category) REFERENCES public.platby_category(pc_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_category_group platby_category_group_pcg_id_group_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_category_group
    ADD CONSTRAINT platby_category_group_pcg_id_group_fkey FOREIGN KEY (pcg_id_group) REFERENCES public.platby_group(pg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_group_skupina platby_group_skupina_pgs_id_group_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_pgs_id_group_fkey FOREIGN KEY (pgs_id_group) REFERENCES public.platby_group(pg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_group_skupina platby_group_skupina_pgs_id_skupina_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_pgs_id_skupina_fkey FOREIGN KEY (pgs_id_skupina) REFERENCES public.skupiny(s_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_item platby_item_pi_id_category_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_item
    ADD CONSTRAINT platby_item_pi_id_category_fkey FOREIGN KEY (pi_id_category) REFERENCES public.platby_category(pc_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_item platby_item_pi_id_raw_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_item
    ADD CONSTRAINT platby_item_pi_id_raw_fkey FOREIGN KEY (pi_id_raw) REFERENCES public.platby_raw(pr_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_item platby_item_pi_id_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_item
    ADD CONSTRAINT platby_item_pi_id_user_fkey FOREIGN KEY (pi_id_user) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: rozpis_item rozpis_item_ri_id_rodic_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rozpis_item
    ADD CONSTRAINT rozpis_item_ri_id_rodic_fkey FOREIGN KEY (ri_id_rodic) REFERENCES public.rozpis(r_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: rozpis_item rozpis_item_ri_partner_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rozpis_item
    ADD CONSTRAINT rozpis_item_ri_partner_fkey FOREIGN KEY (ri_partner) REFERENCES public.pary(p_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: rozpis rozpis_r_trener_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rozpis
    ADD CONSTRAINT rozpis_r_trener_fkey FOREIGN KEY (r_trener) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: upozorneni_skupiny upozorneni_skupiny_ups_id_rodic_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT upozorneni_skupiny_ups_id_rodic_fkey FOREIGN KEY (ups_id_rodic) REFERENCES public.upozorneni(up_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: upozorneni_skupiny upozorneni_skupiny_ups_id_skupina_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT upozorneni_skupiny_ups_id_skupina_fkey FOREIGN KEY (ups_id_skupina) REFERENCES public.skupiny(s_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: upozorneni upozorneni_up_kdo_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni
    ADD CONSTRAINT upozorneni_up_kdo_fkey FOREIGN KEY (up_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: users users_u_group_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_u_group_fkey FOREIGN KEY (u_group) REFERENCES public.permissions(pe_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: users users_u_skupina_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_u_skupina_fkey FOREIGN KEY (u_skupina) REFERENCES public.skupiny(s_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: -
--

REVOKE ALL ON SCHEMA public FROM postgres;
REVOKE ALL ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO olymp;
GRANT USAGE ON SCHEMA public TO olympuser;


--
-- Name: FUNCTION on_update_current_timestamp_akce(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_akce() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_akce() TO olympuser;


--
-- Name: FUNCTION on_update_current_timestamp_aktuality(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_aktuality() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_aktuality() TO olympuser;


--
-- Name: FUNCTION on_update_current_timestamp_dokumenty(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_dokumenty() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_dokumenty() TO olympuser;


--
-- Name: FUNCTION on_update_current_timestamp_galerie_foto(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_galerie_foto() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_galerie_foto() TO olympuser;


--
-- Name: FUNCTION on_update_current_timestamp_nabidka(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_nabidka() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_nabidka() TO olympuser;


--
-- Name: FUNCTION on_update_current_timestamp_rozpis(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_rozpis() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_rozpis() TO olympuser;


--
-- Name: FUNCTION on_update_current_timestamp_upozorneni(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_upozorneni() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_upozorneni() TO olympuser;


--
-- Name: FUNCTION on_update_current_timestamp_users(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_users() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_users() TO olympuser;


--
-- Name: SEQUENCE akce_a_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.akce_a_id_seq TO olympuser;


--
-- Name: SEQUENCE akce_item_ai_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.akce_item_ai_id_seq TO olympuser;


--
-- Name: SEQUENCE aktuality_at_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.aktuality_at_id_seq TO olympuser;


--
-- Name: SEQUENCE dokumenty_d_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.dokumenty_d_id_seq TO olympuser;


--
-- Name: SEQUENCE galerie_dir_gd_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.galerie_dir_gd_id_seq TO olympuser;


--
-- Name: SEQUENCE galerie_foto_gf_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.galerie_foto_gf_id_seq TO olympuser;


--
-- Name: SEQUENCE nabidka_item_ni_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.nabidka_item_ni_id_seq TO olympuser;


--
-- Name: SEQUENCE nabidka_n_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.nabidka_n_id_seq TO olympuser;


--
-- Name: SEQUENCE pary_navrh_pn_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.pary_navrh_pn_id_seq TO olympuser;


--
-- Name: SEQUENCE pary_p_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.pary_p_id_seq TO olympuser;


--
-- Name: SEQUENCE permissions_pe_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.permissions_pe_id_seq TO olympuser;


--
-- Name: SEQUENCE platby_category_group_pcg_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_category_group_pcg_id_seq TO olympuser;


--
-- Name: SEQUENCE platby_category_pc_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_category_pc_id_seq TO olympuser;


--
-- Name: SEQUENCE platby_group_pg_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_group_pg_id_seq TO olympuser;


--
-- Name: SEQUENCE platby_group_skupina_pgs_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_group_skupina_pgs_id_seq TO olympuser;


--
-- Name: SEQUENCE platby_item_pi_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_item_pi_id_seq TO olympuser;


--
-- Name: SEQUENCE platby_raw_pr_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_raw_pr_id_seq TO olympuser;


--
-- Name: SEQUENCE rozpis_item_ri_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.rozpis_item_ri_id_seq TO olympuser;


--
-- Name: SEQUENCE rozpis_r_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.rozpis_r_id_seq TO olympuser;


--
-- Name: SEQUENCE skupiny_s_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.skupiny_s_id_seq TO olympuser;


--
-- Name: SEQUENCE upozorneni_skupiny_ups_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.upozorneni_skupiny_ups_id_seq TO olympuser;


--
-- Name: SEQUENCE upozorneni_up_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.upozorneni_up_id_seq TO olympuser;


--
-- Name: SEQUENCE users_skupiny_us_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.users_skupiny_us_id_seq TO olympuser;


--
-- Name: SEQUENCE users_u_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.users_u_id_seq TO olympuser;


--
-- Name: SEQUENCE video_list_vl_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.video_list_vl_id_seq TO olympuser;


--
-- Name: SEQUENCE video_source_vs_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.video_source_vs_id_seq TO olympuser;


--
-- Name: SEQUENCE video_v_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.video_v_id_seq TO olympuser;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: public; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON SEQUENCES  FROM postgres;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT SELECT,USAGE ON SEQUENCES  TO olympuser;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: public; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON FUNCTIONS  FROM PUBLIC;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON FUNCTIONS  FROM postgres;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT ALL ON FUNCTIONS  TO olympuser;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: -; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres REVOKE ALL ON FUNCTIONS  FROM PUBLIC;


--
-- PostgreSQL database dump complete
--

