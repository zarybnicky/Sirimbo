--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 13.5

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
-- Name: app_private; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA app_private;


--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- Name: plpgsql_check; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql_check WITH SCHEMA public;


--
-- Name: EXTENSION plpgsql_check; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql_check IS 'extended check for plpgsql functions';


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
-- Name: drop_policies(text); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.drop_policies(tbl text) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
   rec record;
begin
   for rec in (
     select policyname from pg_policies
     where schemaname = split_part(tbl, '.', 1) and tablename = split_part(tbl, '.', 2)
   ) loop
     execute 'drop policy "' || rec.policyname || '" on ' || tbl;
   end loop;
end;
$$;


--
-- Name: insert_revision(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.insert_revision() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $_$
DECLARE
    _op CHAR(1);
    _record RECORD;
    _rev_number INTEGER;
    _rev_table VARCHAR := TG_TABLE_NAME || '_revision';
    _where VARCHAR := '';
    _pk VARCHAR;
BEGIN
    IF TG_OP = 'INSERT' THEN
        _op := 'I'; _record := NEW;
    ELSIF TG_OP = 'UPDATE' THEN
        _op := 'U'; _record := NEW;
    ELSE
        _op := 'D'; _record := OLD;
    END IF;
    IF TG_NARGS = 0 THEN
        _where := '_rev_table.id = $1.id';
    ELSE
        _where := format('_rev_table.%1$s = $1.%1$s', TG_ARGV[0]);
        FOREACH _pk IN ARRAY TG_ARGV[1:] LOOP
            _where := _where || format(' AND _rev_table.%1$s = $1.%1$s', _pk);
        END LOOP;
    END IF;
    EXECUTE format('SELECT coalesce(max(rev_number), 0) FROM %s _rev_table WHERE %s', _rev_table, _where)
        INTO _rev_number
        USING _record;
    EXECUTE format('INSERT INTO %s VALUES ($1, $2, $3, $4.*)', _rev_table)
        USING _rev_number + 1, _op, now(), _record;
    RETURN _record;
END;
$_$;


--
-- Name: tg__timestamps(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg__timestamps() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  NEW.created_at = (case when TG_OP = 'INSERT' then NOW() else OLD.created_at end);
  NEW.updated_at = (case when TG_OP = 'UPDATE' and OLD.updated_at >= NOW() then OLD.updated_at + interval '1 millisecond' else NOW() end);
  return NEW;
end;
$$;


--
-- Name: FUNCTION tg__timestamps(); Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON FUNCTION app_private.tg__timestamps() IS 'This trigger should be called on all tables with created_at, updated_at - it ensures that they cannot be manipulated and that updated_at will always be larger than the previous updated_at.';


--
-- Name: current_couple_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_couple_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  select distinct p_id_partner
  from public.pary
  where p_id_partner = current_user_id() and p_archiv = false
  UNION
  select distinct p_id_partnerka
  from public.pary
  where p_id_partnerka = current_user_id() and p_archiv = false;
$$;


--
-- Name: current_session_id(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_session_id() RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select current_setting('jwt.claims.session_id', true);
$$;


--
-- Name: current_user_id(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_user_id() RETURNS bigint
    LANGUAGE sql STABLE
    AS $$
  SELECT current_setting('jwt.claims.user_id', true)::bigint;
$$;


SET default_tablespace = '';

SET default_table_access_method = heap;

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
-- Name: session; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.session (
    ss_id character varying(128) NOT NULL,
    ss_data bytea NOT NULL,
    ss_updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ss_lifetime bigint NOT NULL,
    ss_user bigint
);


--
-- Name: login(character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.login(login character varying, passwd character varying, OUT sess public.session, OUT usr public.users) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_salt varchar;
begin
  if login like '%@%' then
    select users.* into usr from users where u_email = login limit 1;
  else
    select users.* into usr from users where u_login = login limit 1;
  end if;

  if usr is null then
    raise exception 'ACCOUNT_NOT_FOUND' using errcode = '28000';
  end if;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  if usr.u_pass != encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') then
    raise exception 'INVALID_PASSWORD' using errcode = '28P01';
  end if;

  if usr.u_ban then
    raise exception 'ACCOUNT_DISABLED' using errcode = '42501';
  end if;
  if not usr.u_confirmed then
    raise exception 'ACCOUNT_NOT_CONFIRMED' using errcode = '42501';
  end if;

  insert into session
    (ss_id, ss_user, ss_data, ss_lifetime)
    values (gen_random_uuid(), usr.u_id, ('{"id":' || usr.u_id || '}')::bytea, 86400)
    returning * into sess;
end;
$$;


--
-- Name: logout(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.logout() RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
begin
  delete from session where ss_id=current_session_id();
end;
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
-- Name: page; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.page (
    id integer NOT NULL,
    url character varying NOT NULL,
    content json NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE page; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.page IS '@omit delete';


--
-- Name: page_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.page_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: page_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.page_id_seq OWNED BY public.page.id;


--
-- Name: page_revision; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.page_revision (
    rev_number integer NOT NULL,
    rev_operation character(1) NOT NULL,
    rev_timestamp timestamp without time zone,
    id integer NOT NULL,
    url character varying NOT NULL,
    content json NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    CONSTRAINT base_revision_rev_operation_check CHECK ((rev_operation = ANY (ARRAY['I'::bpchar, 'U'::bpchar, 'D'::bpchar])))
);


--
-- Name: TABLE page_revision; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.page_revision IS '@omit create,update,delete';


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
-- Name: page id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.page ALTER COLUMN id SET DEFAULT nextval('public.page_id_seq'::regclass);


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
-- Name: page page_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.page
    ADD CONSTRAINT page_pkey PRIMARY KEY (id);


--
-- Name: page_revision page_revision_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.page_revision
    ADD CONSTRAINT page_revision_pkey PRIMARY KEY (rev_number, id);


--
-- Name: page page_url_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.page
    ADD CONSTRAINT page_url_key UNIQUE (url);


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
-- Name: page _100_page_revision; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_page_revision AFTER INSERT OR DELETE OR UPDATE ON public.page FOR EACH ROW EXECUTE FUNCTION app_private.insert_revision();


--
-- Name: page _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.page FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: akce on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.akce FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_akce();


--
-- Name: aktuality on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.aktuality FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_aktuality();


--
-- Name: dokumenty on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.dokumenty FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_dokumenty();


--
-- Name: galerie_foto on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.galerie_foto FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_galerie_foto();


--
-- Name: nabidka on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.nabidka FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_nabidka();


--
-- Name: rozpis on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.rozpis FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_rozpis();


--
-- Name: upozorneni on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.upozorneni FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_upozorneni();


--
-- Name: users on_update_current_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_users();


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
-- Name: session session_ss_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session
    ADD CONSTRAINT session_ss_user_fkey FOREIGN KEY (ss_user) REFERENCES public.users(u_id) ON DELETE CASCADE;


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
-- Name: akce_item admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.akce_item TO administrator USING (true) WITH CHECK (true);


--
-- Name: aktuality admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.aktuality TO administrator USING (true) WITH CHECK (true);


--
-- Name: dokumenty admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.dokumenty TO administrator USING (true) WITH CHECK (true);


--
-- Name: galerie_dir admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.galerie_dir TO administrator USING (true) WITH CHECK (true);


--
-- Name: galerie_foto admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.galerie_foto TO administrator USING (true) WITH CHECK (true);


--
-- Name: nabidka admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.nabidka TO administrator USING (true) WITH CHECK (true);


--
-- Name: nabidka_item admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.nabidka_item TO administrator USING (true) WITH CHECK (true);


--
-- Name: page admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.page TO administrator USING (true) WITH CHECK (true);


--
-- Name: parameters admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.parameters TO administrator USING (true) WITH CHECK (true);


--
-- Name: pary admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.pary TO administrator USING (true) WITH CHECK (true);


--
-- Name: permissions admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.permissions TO administrator USING (true) WITH CHECK (true);


--
-- Name: platby_category admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.platby_category TO administrator USING (true) WITH CHECK (true);


--
-- Name: platby_category_group admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.platby_category_group TO administrator USING (true) WITH CHECK (true);


--
-- Name: platby_group admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.platby_group TO administrator USING (true) WITH CHECK (true);


--
-- Name: platby_group_skupina admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.platby_group_skupina TO administrator USING (true) WITH CHECK (true);


--
-- Name: platby_item admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.platby_item TO administrator USING (true) WITH CHECK (true);


--
-- Name: platby_raw admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.platby_raw TO administrator USING (true) WITH CHECK (true);


--
-- Name: rozpis admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.rozpis TO administrator USING (true) WITH CHECK (true);


--
-- Name: rozpis_item admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.rozpis_item TO administrator USING (true) WITH CHECK (true);


--
-- Name: session admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.session TO administrator USING (true) WITH CHECK (true);


--
-- Name: skupiny admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.skupiny TO administrator USING (true) WITH CHECK (true);


--
-- Name: upozorneni admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.upozorneni TO administrator USING (true) WITH CHECK (true);


--
-- Name: upozorneni_skupiny admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.upozorneni_skupiny TO administrator USING (true) WITH CHECK (true);


--
-- Name: users admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.users TO administrator USING (true) WITH CHECK (true);


--
-- Name: users_skupiny admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.users_skupiny TO administrator USING (true) WITH CHECK (true);


--
-- Name: video admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.video TO administrator USING (true) WITH CHECK (true);


--
-- Name: video_list admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.video_list TO administrator USING (true) WITH CHECK (true);


--
-- Name: video_source admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.video_source TO administrator USING (true) WITH CHECK (true);


--
-- Name: akce; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.akce ENABLE ROW LEVEL SECURITY;

--
-- Name: akce_item; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.akce_item ENABLE ROW LEVEL SECURITY;

--
-- Name: aktuality; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.aktuality ENABLE ROW LEVEL SECURITY;

--
-- Name: akce_item all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.akce_item FOR SELECT USING (true);


--
-- Name: aktuality all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.aktuality FOR SELECT USING (true);


--
-- Name: dokumenty all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.dokumenty FOR SELECT USING (true);


--
-- Name: galerie_dir all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.galerie_dir FOR SELECT USING (true);


--
-- Name: galerie_foto all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.galerie_foto FOR SELECT USING (true);


--
-- Name: nabidka all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.nabidka FOR SELECT USING (true);


--
-- Name: nabidka_item all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.nabidka_item FOR SELECT USING (true);


--
-- Name: page all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.page FOR SELECT USING (true);


--
-- Name: page_revision all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.page_revision FOR SELECT USING (true);


--
-- Name: parameters all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.parameters FOR SELECT USING (true);


--
-- Name: pary all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.pary FOR SELECT USING (true);


--
-- Name: permissions all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.permissions FOR SELECT USING (true);


--
-- Name: platby_category all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.platby_category FOR SELECT USING (true);


--
-- Name: platby_category_group all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.platby_category_group FOR SELECT USING (true);


--
-- Name: platby_group all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.platby_group FOR SELECT USING (true);


--
-- Name: platby_group_skupina all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.platby_group_skupina FOR SELECT USING (true);


--
-- Name: platby_item all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.platby_item FOR SELECT USING (true);


--
-- Name: platby_raw all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.platby_raw FOR SELECT USING (true);


--
-- Name: rozpis all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.rozpis FOR SELECT USING (true);


--
-- Name: rozpis_item all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.rozpis_item FOR SELECT USING (true);


--
-- Name: skupiny all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.skupiny FOR SELECT USING (true);


--
-- Name: upozorneni all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.upozorneni FOR SELECT USING (true);


--
-- Name: upozorneni_skupiny all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.upozorneni_skupiny FOR SELECT USING (true);


--
-- Name: users all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.users FOR SELECT USING (true);


--
-- Name: users_skupiny all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.users_skupiny FOR SELECT USING (true);


--
-- Name: video all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.video FOR SELECT USING (true);


--
-- Name: video_list all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.video_list FOR SELECT USING (true);


--
-- Name: video_source all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.video_source FOR SELECT USING (true);


--
-- Name: dokumenty; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.dokumenty ENABLE ROW LEVEL SECURITY;

--
-- Name: galerie_dir; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.galerie_dir ENABLE ROW LEVEL SECURITY;

--
-- Name: galerie_foto; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.galerie_foto ENABLE ROW LEVEL SECURITY;

--
-- Name: akce manage_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_all ON public.akce TO administrator USING (true) WITH CHECK (true);


--
-- Name: akce_item manage_own; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_own ON public.akce_item TO member USING ((ai_user = public.current_user_id())) WITH CHECK ((ai_user = public.current_user_id()));


--
-- Name: nabidka_item manage_own; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_own ON public.nabidka_item TO member USING ((ni_partner IN ( SELECT public.current_couple_ids() AS current_couple_ids))) WITH CHECK ((ni_partner IN ( SELECT public.current_couple_ids() AS current_couple_ids)));


--
-- Name: pary_navrh manage_own; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_own ON public.pary_navrh USING (((pn_navrhl = public.current_user_id()) OR (pn_partner = public.current_user_id()) OR (pn_partnerka = public.current_user_id()))) WITH CHECK (((pn_navrhl = public.current_user_id()) AND ((pn_partner = public.current_user_id()) OR (pn_partnerka = public.current_user_id()))));


--
-- Name: rozpis_item manage_own; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_own ON public.rozpis_item TO member USING ((ri_partner IN ( SELECT public.current_couple_ids() AS current_couple_ids))) WITH CHECK ((ri_partner IN ( SELECT public.current_couple_ids() AS current_couple_ids)));


--
-- Name: session manage_own; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_own ON public.session USING ((ss_user = public.current_user_id())) WITH CHECK ((ss_user = public.current_user_id()));


--
-- Name: users manage_own; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_own ON public.users USING ((u_id = public.current_user_id())) WITH CHECK ((u_id = public.current_user_id()));


--
-- Name: nabidka; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.nabidka ENABLE ROW LEVEL SECURITY;

--
-- Name: nabidka_item; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.nabidka_item ENABLE ROW LEVEL SECURITY;

--
-- Name: page; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.page ENABLE ROW LEVEL SECURITY;

--
-- Name: page_revision; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.page_revision ENABLE ROW LEVEL SECURITY;

--
-- Name: parameters; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.parameters ENABLE ROW LEVEL SECURITY;

--
-- Name: pary; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.pary ENABLE ROW LEVEL SECURITY;

--
-- Name: permissions; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.permissions ENABLE ROW LEVEL SECURITY;

--
-- Name: platby_category; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.platby_category ENABLE ROW LEVEL SECURITY;

--
-- Name: platby_category_group; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.platby_category_group ENABLE ROW LEVEL SECURITY;

--
-- Name: platby_group; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.platby_group ENABLE ROW LEVEL SECURITY;

--
-- Name: platby_group_skupina; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.platby_group_skupina ENABLE ROW LEVEL SECURITY;

--
-- Name: platby_item; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.platby_item ENABLE ROW LEVEL SECURITY;

--
-- Name: platby_raw; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.platby_raw ENABLE ROW LEVEL SECURITY;

--
-- Name: rozpis; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.rozpis ENABLE ROW LEVEL SECURITY;

--
-- Name: rozpis_item; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.rozpis_item ENABLE ROW LEVEL SECURITY;

--
-- Name: akce select_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY select_all ON public.akce FOR SELECT USING (true);


--
-- Name: session; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.session ENABLE ROW LEVEL SECURITY;

--
-- Name: skupiny; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.skupiny ENABLE ROW LEVEL SECURITY;

--
-- Name: upozorneni; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.upozorneni ENABLE ROW LEVEL SECURITY;

--
-- Name: upozorneni_skupiny; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.upozorneni_skupiny ENABLE ROW LEVEL SECURITY;

--
-- Name: users; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.users ENABLE ROW LEVEL SECURITY;

--
-- Name: users_skupiny; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.users_skupiny ENABLE ROW LEVEL SECURITY;

--
-- Name: video; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.video ENABLE ROW LEVEL SECURITY;

--
-- Name: video_list; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.video_list ENABLE ROW LEVEL SECURITY;

--
-- Name: video_source; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.video_source ENABLE ROW LEVEL SECURITY;

--
-- Name: SCHEMA app_private; Type: ACL; Schema: -; Owner: -
--

GRANT ALL ON SCHEMA app_private TO postgres;


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: -
--

REVOKE ALL ON SCHEMA public FROM postgres;
REVOKE ALL ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO olymp;
GRANT ALL ON SCHEMA public TO postgres;
GRANT USAGE ON SCHEMA public TO olympuser;
GRANT ALL ON SCHEMA public TO anonymous;


--
-- Name: TABLE users; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.users TO member;


--
-- Name: FUNCTION on_update_current_timestamp_akce(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_akce() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_akce() TO olympuser;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_akce() TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_aktuality(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_aktuality() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_aktuality() TO olympuser;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_aktuality() TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_dokumenty(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_dokumenty() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_dokumenty() TO olympuser;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_dokumenty() TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_galerie_foto(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_galerie_foto() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_galerie_foto() TO olympuser;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_galerie_foto() TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_nabidka(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_nabidka() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_nabidka() TO olympuser;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_nabidka() TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_rozpis(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_rozpis() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_rozpis() TO olympuser;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_rozpis() TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_upozorneni(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_upozorneni() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_upozorneni() TO olympuser;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_upozorneni() TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_users(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_users() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_users() TO olympuser;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_users() TO anonymous;


--
-- Name: TABLE akce; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.akce TO anonymous;


--
-- Name: SEQUENCE akce_a_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.akce_a_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.akce_a_id_seq TO anonymous;


--
-- Name: TABLE akce_item; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.akce_item TO anonymous;


--
-- Name: SEQUENCE akce_item_ai_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.akce_item_ai_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.akce_item_ai_id_seq TO anonymous;


--
-- Name: TABLE aktuality; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.aktuality TO anonymous;


--
-- Name: SEQUENCE aktuality_at_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.aktuality_at_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.aktuality_at_id_seq TO anonymous;


--
-- Name: TABLE dokumenty; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.dokumenty TO anonymous;


--
-- Name: SEQUENCE dokumenty_d_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.dokumenty_d_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.dokumenty_d_id_seq TO anonymous;


--
-- Name: TABLE galerie_dir; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.galerie_dir TO anonymous;


--
-- Name: SEQUENCE galerie_dir_gd_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.galerie_dir_gd_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.galerie_dir_gd_id_seq TO anonymous;


--
-- Name: TABLE galerie_foto; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.galerie_foto TO anonymous;


--
-- Name: SEQUENCE galerie_foto_gf_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.galerie_foto_gf_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.galerie_foto_gf_id_seq TO anonymous;


--
-- Name: TABLE platby_category; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.platby_category TO member;


--
-- Name: TABLE platby_category_group; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.platby_category_group TO member;


--
-- Name: TABLE platby_group; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.platby_group TO member;


--
-- Name: TABLE platby_group_skupina; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.platby_group_skupina TO member;


--
-- Name: TABLE platby_item; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.platby_item TO member;


--
-- Name: TABLE skupiny; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.skupiny TO anonymous;


--
-- Name: TABLE nabidka; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.nabidka TO member;


--
-- Name: TABLE nabidka_item; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.nabidka_item TO member;


--
-- Name: SEQUENCE nabidka_item_ni_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.nabidka_item_ni_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.nabidka_item_ni_id_seq TO anonymous;


--
-- Name: SEQUENCE nabidka_n_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.nabidka_n_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.nabidka_n_id_seq TO anonymous;


--
-- Name: TABLE page; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.page TO anonymous;


--
-- Name: SEQUENCE page_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.page_id_seq TO administrator;


--
-- Name: TABLE page_revision; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.page_revision TO anonymous;


--
-- Name: TABLE parameters; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.parameters TO anonymous;


--
-- Name: TABLE pary; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.pary TO anonymous;


--
-- Name: TABLE pary_navrh; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.pary_navrh TO member;


--
-- Name: SEQUENCE pary_navrh_pn_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.pary_navrh_pn_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.pary_navrh_pn_id_seq TO anonymous;


--
-- Name: SEQUENCE pary_p_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.pary_p_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.pary_p_id_seq TO anonymous;


--
-- Name: TABLE permissions; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.permissions TO anonymous;


--
-- Name: SEQUENCE permissions_pe_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.permissions_pe_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.permissions_pe_id_seq TO anonymous;


--
-- Name: SEQUENCE platby_category_group_pcg_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_category_group_pcg_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.platby_category_group_pcg_id_seq TO anonymous;


--
-- Name: SEQUENCE platby_category_pc_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_category_pc_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.platby_category_pc_id_seq TO anonymous;


--
-- Name: SEQUENCE platby_group_pg_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_group_pg_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.platby_group_pg_id_seq TO anonymous;


--
-- Name: SEQUENCE platby_group_skupina_pgs_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_group_skupina_pgs_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.platby_group_skupina_pgs_id_seq TO anonymous;


--
-- Name: SEQUENCE platby_item_pi_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_item_pi_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.platby_item_pi_id_seq TO anonymous;


--
-- Name: TABLE platby_raw; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.platby_raw TO member;


--
-- Name: SEQUENCE platby_raw_pr_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_raw_pr_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.platby_raw_pr_id_seq TO anonymous;


--
-- Name: TABLE rozpis; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.rozpis TO member;


--
-- Name: TABLE rozpis_item; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.rozpis_item TO member;


--
-- Name: SEQUENCE rozpis_item_ri_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.rozpis_item_ri_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.rozpis_item_ri_id_seq TO anonymous;


--
-- Name: SEQUENCE rozpis_r_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.rozpis_r_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.rozpis_r_id_seq TO anonymous;


--
-- Name: SEQUENCE skupiny_s_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.skupiny_s_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.skupiny_s_id_seq TO anonymous;


--
-- Name: TABLE upozorneni; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.upozorneni TO member;


--
-- Name: TABLE upozorneni_skupiny; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.upozorneni_skupiny TO member;


--
-- Name: SEQUENCE upozorneni_skupiny_ups_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.upozorneni_skupiny_ups_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.upozorneni_skupiny_ups_id_seq TO anonymous;


--
-- Name: SEQUENCE upozorneni_up_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.upozorneni_up_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.upozorneni_up_id_seq TO anonymous;


--
-- Name: TABLE users_skupiny; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.users_skupiny TO member;


--
-- Name: SEQUENCE users_skupiny_us_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.users_skupiny_us_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.users_skupiny_us_id_seq TO anonymous;


--
-- Name: SEQUENCE users_u_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.users_u_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.users_u_id_seq TO anonymous;


--
-- Name: TABLE video; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.video TO anonymous;


--
-- Name: TABLE video_list; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.video_list TO anonymous;


--
-- Name: SEQUENCE video_list_vl_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.video_list_vl_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.video_list_vl_id_seq TO anonymous;


--
-- Name: TABLE video_source; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.video_source TO anonymous;


--
-- Name: SEQUENCE video_source_vs_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.video_source_vs_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.video_source_vs_id_seq TO anonymous;


--
-- Name: SEQUENCE video_v_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.video_v_id_seq TO olympuser;
GRANT SELECT,USAGE ON SEQUENCE public.video_v_id_seq TO anonymous;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: public; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT SELECT,USAGE ON SEQUENCES  TO olympuser;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT SELECT,USAGE ON SEQUENCES  TO anonymous;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: public; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT ALL ON FUNCTIONS  TO olympuser;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT ALL ON FUNCTIONS  TO anonymous;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: -; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres REVOKE ALL ON FUNCTIONS  FROM PUBLIC;


--
-- PostgreSQL database dump complete
--

