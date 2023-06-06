--
-- PostgreSQL database dump
--

-- Dumped from database version 13.10
-- Dumped by pg_dump version 13.10

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
-- Name: crm_cohort; Type: TYPE; Schema: app_private; Owner: -
--

CREATE TYPE app_private.crm_cohort AS ENUM (
    'dancer',
    'hobbyist',
    'showdance',
    'free-lesson',
    'contact-me-later'
);


--
-- Name: gender_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.gender_type AS ENUM (
    'men',
    'woman',
    'unspecified'
);


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
-- Name: prospect_data; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.prospect_data AS (
	name text,
	surname text,
	email text,
	phone text,
	yearofbirth text
);


--
-- Name: tenant_attachment_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.tenant_attachment_type AS ENUM (
    'logo',
    'photo',
    'map'
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
    _rev_table VARCHAR := TG_TABLE_SCHEMA || '.' || TG_TABLE_NAME || '_revision';
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
-- Name: tg__tenant(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg__tenant() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  NEW.tenant = (case when TG_OP = 'INSERT' then get_current_tenant() else OLD.tenant end);
  return NEW;
end;
$$;


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
-- Name: tg_users__encrypt_password(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_users__encrypt_password() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare
  v_salt varchar;
begin
  if length(NEW.u_pass) <> 40 then
      select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
      NEW.u_pass := encode(digest(v_salt || NEW.u_pass || v_salt, 'sha1'), 'hex');
  end if;
  return NEW;
end;
$$;


--
-- Name: tg_users__notify_admin(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_users__notify_admin() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  perform graphile_worker.add_job('notify_admin_registration', json_build_object('id', NEW.u_id));
  return NEW;
end;
$$;


SET default_tablespace = '';

SET default_table_access_method = heap;

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
    p_timestamp_archive timestamp with time zone,
    id bigint GENERATED ALWAYS AS (p_id) STORED
);


--
-- Name: TABLE pary; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.pary IS '@foreignKey (p_id_partnerka) references users (u_id)';


--
-- Name: active_couples(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.active_couples() RETURNS SETOF public.pary
    LANGUAGE sql STABLE
    AS $$
  select p.*
  from pary as p
      left join users as m on p.p_id_partner=m.u_id
      left join users as f on p.p_id_partnerka=f.u_id
  where p.p_archiv = false
      and p.p_id_partner is not null and p.p_id_partner <> 0
      and p.p_id_partnerka is not null and p.p_id_partnerka <> 0
      and m.u_id is not null and f.u_id is not null
  order by m.u_prijmeni asc
$$;


--
-- Name: current_tenant_id(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_tenant_id() RETURNS bigint
    LANGUAGE sql STABLE
    AS $$
  select nullif(current_setting('jwt.claims.tenant_id', '1')::bigint, 1);
$$;


--
-- Name: event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event (
    id bigint NOT NULL,
    name text NOT NULL,
    location_text text NOT NULL,
    description text NOT NULL,
    since date NOT NULL,
    until date NOT NULL,
    capacity bigint DEFAULT '0'::bigint NOT NULL,
    files_legacy text DEFAULT ''::text NOT NULL,
    updated_at timestamp with time zone,
    is_locked boolean DEFAULT false NOT NULL,
    is_visible boolean DEFAULT false NOT NULL,
    summary text DEFAULT '[]'::jsonb NOT NULL,
    is_public boolean DEFAULT false NOT NULL,
    enable_notes boolean DEFAULT false NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: akce_free_slots(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.akce_free_slots(a public.event) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select a.a_kapacita - (select count(*) from akce_item where ai_id_rodic = a.a_id);
$$;


--
-- Name: akce_has_capacity(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.akce_has_capacity(a public.event) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select count(*) < a.a_kapacita from akce_item where ai_id_rodic = a.a_id;
$$;


--
-- Name: akce_my_notes(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.akce_my_notes(a public.event) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select notes from akce_item where ai_id_rodic=a.a_id and ai_user=current_user_id();
$$;


--
-- Name: akce_signed_up(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.akce_signed_up(a public.event) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select exists (select ai_id from akce_item where ai_id_rodic=a.a_id and ai_user=current_user_id());
$$;


--
-- Name: rozpis_item; Type: TABLE; Schema: public; Owner: -
--

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


--
-- Name: book_lesson(bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.book_lesson(lesson_id bigint) RETURNS SETOF public.rozpis_item
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  schedule rozpis;
  lesson rozpis_item;
  couple_id bigint;
begin
  select * into lesson from rozpis_item where ri_id=lesson_id;
  select * into schedule from rozpis where r_id=lesson.ri_id_rodic;
  select * into couple_id from current_couple_ids() limit 1;

  if schedule is null or lesson is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;

  if schedule.r_lock or lesson.ri_lock then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  return query update rozpis_item set ri_partner = couple_id where ri_id = lesson_id
    returning *;
end;
$$;


--
-- Name: cancel_lesson(bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.cancel_lesson(lesson_id bigint) RETURNS SETOF public.rozpis_item
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  schedule rozpis;
  lesson rozpis_item;
begin
  select * into lesson from rozpis_item where ri_id=lesson_id;
  select * into schedule from rozpis where r_id=lesson.ri_id_rodic;

  if schedule is null or lesson is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;

  if schedule.r_lock or lesson.ri_lock then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  return query update rozpis_item set ri_partner = null where ri_id = lesson_id
    returning *;
end;
$$;


--
-- Name: cancel_participation(bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.cancel_participation(event_id bigint) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  event akce;
begin
  select * into event from akce where a_id=event_id;
  if event is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;

  if event.a_lock then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  delete from akce_item where ai_id_rodic=event.a_id and ai_user=current_user_id();
end;
$$;


--
-- Name: change_password(character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.change_password(old_pass character varying, new_pass character varying) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  usr users;
  v_salt varchar;
begin
  select users.* into usr from users where u_id = current_user_id() limit 1;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  if usr.u_pass != encode(digest(v_salt || old_pass || v_salt, 'sha1'), 'hex') then
    raise exception 'INVALID_PASSWORD' using errcode = '28P01';
  end if;

  update users set u_pass = new_pass where u_id = usr.u_id;
end;
$$;


--
-- Name: confirm_user(bigint, bigint, bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.confirm_user(id bigint, grp bigint, cohort bigint) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  usr users;
begin
  select * into usr from users where u_id=id;
  if usr is null then
    raise exception 'ACCOUNT_NOT_FOUND' using errcode = '28000';
  end if;
  update users set u_confirmed=true, u_group=grp, u_skupina=cohort, u_system=false where u_id=id;
  perform graphile_worker.add_job('notify_confirmed_user', json_build_object('id', id));
end;
$$;


--
-- Name: create_couple(bigint, bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_couple(man bigint, woman bigint) RETURNS SETOF public.pary
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  couple_man pary;
  couple_woman pary;
begin
  select * into couple_man from pary
  where p_archiv=false and p_id_partner=man;

  select * into couple_woman from pary
  where p_archiv=false and (p_id_partnerka=woman or (p_id_partnerka is null and p_id_partner=woman));

  if couple_man.p_id_partnerka = woman then
     return next couple_man;
  end if;

  if couple_man.p_id_partnerka is not null and couple_man.p_id_partnerka<>0 then
    insert into pary (p_id_partner, p_id_partnerka) VALUES (couple_man.p_id_partnerka, 0);
  end if;
  update pary set p_archiv=true where p_id = couple_man.p_id;

  if couple_woman.p_id_partnerka is not null and couple_woman.p_id_partnerka<>0 then
    insert into pary (p_id_partner, p_id_partnerka) VALUES (couple_woman.p_id_partner, 0);
  end if;
  update pary set p_archiv=true where p_id = couple_woman.p_id;

  return query insert into pary (p_id_partner, p_id_partnerka) VALUES (man, woman) returning *;
end;
$$;


--
-- Name: create_participation(bigint, integer, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_participation(event_id bigint, year_of_birth integer, my_notes text) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  event akce;
begin
  select * into event from akce where a_id=event_id;
  if event is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;

  if event.a_lock then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  INSERT INTO akce_item
    (ai_id_rodic, ai_user, ai_rok_narozeni, notes)
  values
    (event_id, current_user_id(), year_of_birth, my_notes)
  ON CONFLICT (ai_id_rodic, ai_user)
  DO UPDATE SET notes = my_notes, ai_rok_narozeni=year_of_birth;
end;
$$;


--
-- Name: create_participation_external(bigint, text, text, text, text, text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_participation_external(event_id bigint, first_name text, last_name text, guardian_name text, email text, phone text, notes text, birth_number text) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  event akce;
begin
  select * into event from akce where a_id=event_id;
  if event is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;

  if event.a_lock then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  INSERT INTO attendee_external (event_id, first_name, last_name, guardian_name, email, phone, notes, birth_number)
  values (event_id, first_name, last_name, guardian_name, email, phone, notes, birth_number);
end;
$$;


--
-- Name: crm_copy_to_form_responses(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.crm_copy_to_form_responses() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare
  prospect app_private.crm_prospect;
begin
  select * into prospect from app_private.crm_prospect where id = NEW.prospect;
  insert into form_responses (type, url, data)
    values ('Přijď tančit!', NEW.origin, row_to_json(prospect.data), NEW.created_at);
  return NEW;
end;
$$;


--
-- Name: current_couple_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_couple_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  select distinct p_id
  from public.pary
  where p_id_partner = current_user_id() and p_archiv = false
  UNION
  select distinct p_id
  from public.pary
  where p_id_partnerka = current_user_id() and p_archiv = false;
$$;


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
    pe_main integer NOT NULL,
    id bigint GENERATED ALWAYS AS (pe_id) STORED
);


--
-- Name: current_permissions(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_permissions() RETURNS SETOF public.permissions
    LANGUAGE sql STABLE
    AS $$
  SELECT permissions.* from permissions
  inner join users on u_group=pe_id
  where u_id=current_setting('jwt.claims.user_id', true)::bigint;
$$;


--
-- Name: current_session_id(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_session_id() RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select nullif(current_setting('jwt.claims.session_id', true), '')::text;
$$;


--
-- Name: current_user_id(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_user_id() RETURNS bigint
    LANGUAGE sql STABLE
    AS $$
  SELECT current_setting('jwt.claims.user_id', true)::bigint;
$$;


--
-- Name: event_remaining_spots(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_remaining_spots(a public.event) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select a.capacity - (select count(*) from attendee_user where event_id = a.id) - (select count(*) from attendee_external where event_id = a.id);
$$;


--
-- Name: fix_unpaired_couples(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fix_unpaired_couples() RETURNS SETOF public.pary
    LANGUAGE sql STRICT SECURITY DEFINER
    AS $$
  insert into pary (p_id_partner, p_id_partnerka)
  select u_id, 0 from users
  where u_id not in (
    select u_id from users
    left join pary on p_id_partnerka=u_id or p_id_partner=u_id
    where p_archiv=false
  ) returning *;
$$;


--
-- Name: get_current_couple(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.get_current_couple() RETURNS public.pary
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  SELECT * FROM pary WHERE p_id in (select * from current_couple_ids()) limit 1;
$$;


--
-- Name: tenant; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tenant (
    id bigint NOT NULL,
    name text NOT NULL,
    member_info text NOT NULL,
    origins text[] DEFAULT ARRAY[]::text[] NOT NULL
);


--
-- Name: get_current_tenant(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.get_current_tenant() RETURNS public.tenant
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  SELECT * FROM tenant WHERE id = current_tenant_id();
$$;


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
    u_group bigint DEFAULT 0 NOT NULL,
    u_skupina bigint DEFAULT '1'::bigint NOT NULL,
    u_dancer boolean DEFAULT true NOT NULL,
    u_ban boolean DEFAULT true NOT NULL,
    u_lock boolean DEFAULT true NOT NULL,
    u_confirmed boolean DEFAULT false NOT NULL,
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
    u_teacher boolean DEFAULT false NOT NULL,
    u_gdpr_signed_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    id bigint GENERATED ALWAYS AS (u_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: get_current_user(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.get_current_user() RETURNS public.users
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  SELECT * FROM users WHERE u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$;


--
-- Name: session; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.session (
    ss_id character varying(128) NOT NULL,
    ss_updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ss_lifetime bigint NOT NULL,
    ss_user bigint
);


--
-- Name: TABLE session; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.session IS '@omit create,update,delete';


--
-- Name: login(character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.login(login character varying, passwd character varying, OUT couple public.pary, OUT sess public.session, OUT usr public.users) RETURNS record
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
    (ss_id, ss_user, ss_lifetime)
    values (gen_random_uuid(), usr.u_id, 86400)
    returning * into sess;

  select * from pary where p_archiv=false and (p_id_partner=usr.u_id or p_id_partnerka=usr.u_id) into couple;
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
-- Name: upozorneni; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.upozorneni (
    up_id bigint NOT NULL,
    up_kdo bigint,
    up_nadpis text NOT NULL,
    up_text text NOT NULL,
    up_barvy bigint DEFAULT '0'::bigint NOT NULL,
    up_lock boolean DEFAULT false NOT NULL,
    up_timestamp timestamp with time zone,
    up_timestamp_add timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    scheduled_since timestamp with time zone,
    scheduled_until timestamp with time zone,
    is_visible boolean DEFAULT true,
    id bigint GENERATED ALWAYS AS (up_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: my_announcements(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.my_announcements() RETURNS SETOF public.upozorneni
    LANGUAGE sql STABLE
    AS $$
  select upozorneni.* from upozorneni
  where is_visible = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by up_timestamp_add desc;
$$;


--
-- Name: my_lessons(date, date); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.my_lessons(start_date date, end_date date) RETURNS SETOF public.rozpis_item
    LANGUAGE sql STABLE
    AS $$
  select rozpis_item.*
  from public.rozpis_item
  inner join public.rozpis on (rozpis.r_id = rozpis_item.ri_id_rodic)
  left join public.pary on (rozpis_item.ri_partner = pary.p_id)
  where (
        rozpis.r_trener = current_user_id()
     or pary.p_id_partner = current_user_id()
     or pary.p_id_partnerka = current_user_id()
  ) and rozpis.r_visible = true and r_datum >= start_date and r_datum <= end_date
  order by rozpis.r_datum, rozpis_item.ri_od
$$;


--
-- Name: nabidka; Type: TABLE; Schema: public; Owner: -
--

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


--
-- Name: nabidka_free_lessons(public.nabidka); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.nabidka_free_lessons(n public.nabidka) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select n.n_pocet_hod - (select sum(ni_pocet_hod) from nabidka_item where ni_id_rodic = n.n_id);
$$;


--
-- Name: nabidka_my_lessons(public.nabidka); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.nabidka_my_lessons(n public.nabidka) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select COALESCE(ni_pocet_hod, 0) from nabidka_item where n.n_id = ni_id_rodic
  and ni_partner in (select * from current_couple_ids());
$$;


--
-- Name: on_delete_file_dokumenty(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.on_delete_file_dokumenty() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    perform graphile_worker.add_job('delete_file', json_build_object('path', OLD.d_path));
    return old;
END;
$$;


--
-- Name: on_update_author_aktuality(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.on_update_author_aktuality() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.at_kdo = current_user_id();
   NEW.at_timestamp = now();
   RETURN NEW;
END;
$$;


--
-- Name: on_update_author_upozorneni(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.on_update_author_upozorneni() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.up_kdo = current_user_id();
   NEW.up_timestamp = now();
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
-- Name: on_update_event_timestamp(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.on_update_event_timestamp() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.updated_at = now();
   RETURN NEW;
END;
$$;


--
-- Name: prospect_form_dancer(app_private.crm_cohort, public.prospect_data, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.prospect_form_dancer(cohort app_private.crm_cohort, prospect_data public.prospect_data, origin text, note text) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  prospect app_private.crm_prospect;
begin
  select * from app_private.crm_prospect where (data).email=prospect_data.email or (data).phone=prospect_data.phone into prospect;
  if prospect is null then
    insert into app_private.crm_prospect (cohort, data) values (cohort, prospect_data) returning * into prospect;
  end if;

  insert into app_private.crm_activity (prospect, origin, note) values (prospect.id, origin, note);
end;
$$;


--
-- Name: reservation_set_desired_lessons(bigint, smallint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.reservation_set_desired_lessons(reservation_id bigint, lesson_count smallint, OUT reservation public.nabidka) RETURNS public.nabidka
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
begin
  select * into reservation from nabidka where n_id = reservation_id;

  if lesson_count = 0 then
    delete from nabidka_item where ni_id_rodic = reservation_id and ni_partner in (select * from current_couple_ids());
    return;
  end if;

  if lesson_count > (nabidka_my_lessons(reservation) + nabidka_free_lessons(reservation)) then
    select (nabidka_my_lessons(reservation) + nabidka_free_lessons(reservation))::smallint into lesson_count;
  end if;
  if reservation.n_max_pocet_hod > 0 and lesson_count > reservation.n_max_pocet_hod then
    select reservation.n_max_pocet_hod into lesson_count;
  end if;

  INSERT INTO nabidka_item
    (ni_id_rodic, ni_partner, ni_pocet_hod)
  values
    (reservation_id, (select current_couple_ids() limit 1), lesson_count)
  ON CONFLICT (ni_id_rodic, ni_partner)
  DO UPDATE SET ni_pocet_hod = lesson_count;

  select * into reservation from nabidka where n_id = reservation_id;
end;
$$;


--
-- Name: reservations_for_range(date, date); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.reservations_for_range(start_date date, end_date date) RETURNS SETOF public.nabidka
    LANGUAGE sql STABLE
    AS $$
  select * from nabidka
  where n_visible=true
  and n_do >= start_date and n_od <= end_date
  order by n_od asc;
$$;


--
-- Name: reset_password(character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.reset_password(login character varying, email character varying) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_salt varchar;
  usr users;
begin
  select * into usr from users where u_login=login and u_email=email;
  if usr is null then
    raise exception 'ACCOUNT_NOT_FOUND' using errcode = '28000';
  end if;
  perform graphile_worker.add_job('forgotten_password_generate', json_build_object('id', usr.u_id));
end;
$$;


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
    r_timestamp timestamp with time zone,
    id bigint GENERATED ALWAYS AS (r_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: schedules_for_range(date, date); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.schedules_for_range(start_date date, end_date date) RETURNS SETOF public.rozpis
    LANGUAGE sql STABLE
    AS $$
  select * from rozpis
  where r_visible=true
  and r_datum >= start_date and r_datum <= end_date
  order by r_datum asc;
$$;


--
-- Name: submit_form(text, jsonb, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.submit_form(type text, data jsonb, url text) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
begin
  insert into form_responses (type, data, url) values (type, data, url);
end;
$$;


--
-- Name: video; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.video (
    v_id bigint NOT NULL,
    v_uri text NOT NULL,
    v_title text NOT NULL,
    v_author text NOT NULL,
    v_description text NOT NULL,
    v_playlist text,
    v_created_at timestamp with time zone DEFAULT now() NOT NULL,
    v_updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: title_videos(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.title_videos() RETURNS SETOF app_private.video
    LANGUAGE sql STABLE
    AS $$
  select * from video where v_id in (
    select pa_value::bigint from parameters where pa_name in (
      'title_video1', 'title_video2', 'title_video3', 'title_video4'
    )
  );
$$;


--
-- Name: trainers(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.trainers() RETURNS SETOF public.users
    LANGUAGE sql STABLE
    AS $$
  SELECT users.*
    FROM users INNER JOIN permissions on u_group=pe_id
  WHERE u_confirmed='1'
    AND u_system='0'
    AND u_ban='0'
    AND CASE
      WHEN (select pe_rozpis > 8 from current_permissions()) THEN pe_rozpis >= 8
      ELSE u_id = current_user_id()
    END
  ORDER BY u_prijmeni, u_jmeno;
$$;


--
-- Name: users_date_of_newest_payment(public.users); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.users_date_of_newest_payment(a public.users) RETURNS date
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  SELECT max(pi_date)
  FROM platby_item
  where pi_id_user = a.u_id
$$;


--
-- Name: users_date_of_oldest_payment(public.users); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.users_date_of_oldest_payment(a public.users) RETURNS date
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  SELECT min(pi_date)
  FROM platby_item
  where pi_id_user = a.u_id
$$;


--
-- Name: users_full_name(public.users); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.users_full_name(u public.users) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select trim(both from COALESCE(u.u_jmeno, '') || ' ' || COALESCE(u.u_prijmeni, ''));
$$;


--
-- Name: users_has_valid_payment(public.users); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.users_has_valid_payment(a public.users) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  SELECT EXISTS (
    SELECT pi_id
    FROM platby_item
      INNER JOIN platby_category ON pi_id_category=pc_id
      INNER JOIN platby_category_group ON pcg_id_category=pc_id
      INNER JOIN platby_group ON pg_id=pcg_id_group
    WHERE pg_type='1'
      AND CURRENT_DATE >= pc_valid_from
      AND CURRENT_DATE <= pc_valid_to
      AND pi_id_user = a.u_id
  )
$$;


--
-- Name: users_in_public_cohort(public.users); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.users_in_public_cohort(a public.users) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  SELECT s_visible
  FROM skupiny
  inner join users on s_id = u_skupina
  where u_id = a.u_id
$$;


--
-- Name: FUNCTION users_in_public_cohort(a public.users); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.users_in_public_cohort(a public.users) IS '@filterable';


--
-- Name: verify_function(regproc, regclass); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.verify_function(f regproc, relid regclass DEFAULT 0) RETURNS void
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
declare
  error text[];
  count int;
begin
  select array_agg(plpgsql_check_function) into error
  from plpgsql_check_function(
    funcoid => f,
    relid => relid,
    performance_warnings => true,
    extra_warnings => true,
    security_warnings => true
  );
  if array_length(error, 1) > 0 then
    raise exception 'Error when checking function %', f using detail = array_to_string(error, E'\n');
  end if;
end;
$$;


--
-- Name: app_table_overview; Type: VIEW; Schema: app_private; Owner: -
--

CREATE VIEW app_private.app_table_overview AS
 SELECT c.relname,
        CASE c.relrowsecurity
            WHEN true THEN ''::text
            ELSE 'NO RLS'::text
        END AS rls,
        CASE (EXISTS ( SELECT 1
               FROM information_schema.columns
              WHERE (((columns.table_name)::name = c.relname) AND ((columns.table_schema)::name = n.nspname) AND ((columns.column_name)::name = 'id'::name))))
            WHEN true THEN ''::text
            ELSE 'NO ID'::text
        END AS has_id,
        CASE (EXISTS ( SELECT 1
               FROM information_schema.columns
              WHERE (((columns.table_name)::name = c.relname) AND ((columns.table_schema)::name = n.nspname) AND ((columns.column_name)::name = 'tenant_id'::name))))
            WHEN true THEN ''::text
            ELSE 'NO TENANT'::text
        END AS has_tenant,
        CASE ( SELECT (array_agg(role_table_grants.grantee ORDER BY role_table_grants.grantee))::text[] AS array_agg
               FROM information_schema.role_table_grants
              WHERE (((role_table_grants.table_name)::name = c.relname) AND ((role_table_grants.table_schema)::name = 'public'::name) AND ((role_table_grants.privilege_type)::text = 'SELECT'::text))
              GROUP BY role_table_grants.table_name)
            WHEN ARRAY['anonymous'::text, 'olymp'::text] THEN NULL::information_schema.sql_identifier[]
            ELSE ( SELECT array_agg(role_table_grants.grantee ORDER BY role_table_grants.grantee) AS array_agg
               FROM information_schema.role_table_grants
              WHERE (((role_table_grants.table_name)::name = c.relname) AND ((role_table_grants.table_schema)::name = 'public'::name) AND ((role_table_grants.privilege_type)::text = 'SELECT'::text))
              GROUP BY role_table_grants.table_name)
        END AS wrong_acl,
    ARRAY( SELECT p.polname
           FROM pg_policy p
          WHERE (p.polrelid = c.oid)) AS policies
   FROM (pg_class c
     JOIN pg_namespace n ON ((n.oid = c.relnamespace)))
  WHERE ((c.relkind = 'r'::"char") AND (n.nspname = 'public'::name))
  ORDER BY c.relname;


--
-- Name: crm_activity; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.crm_activity (
    id integer NOT NULL,
    prospect integer,
    origin text NOT NULL,
    note text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE crm_activity; Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON TABLE app_private.crm_activity IS '@omit delete';


--
-- Name: crm_activity_id_seq; Type: SEQUENCE; Schema: app_private; Owner: -
--

CREATE SEQUENCE app_private.crm_activity_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: crm_activity_id_seq; Type: SEQUENCE OWNED BY; Schema: app_private; Owner: -
--

ALTER SEQUENCE app_private.crm_activity_id_seq OWNED BY app_private.crm_activity.id;


--
-- Name: crm_prospect; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.crm_prospect (
    id integer NOT NULL,
    cohort app_private.crm_cohort,
    data public.prospect_data,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE crm_prospect; Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON TABLE app_private.crm_prospect IS '@omit create';


--
-- Name: crm_prospect_id_seq; Type: SEQUENCE; Schema: app_private; Owner: -
--

CREATE SEQUENCE app_private.crm_prospect_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: crm_prospect_id_seq; Type: SEQUENCE OWNED BY; Schema: app_private; Owner: -
--

ALTER SEQUENCE app_private.crm_prospect_id_seq OWNED BY app_private.crm_prospect.id;


--
-- Name: video_list; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.video_list (
    vl_id bigint NOT NULL,
    vl_url text NOT NULL,
    vl_title text NOT NULL,
    vl_description text NOT NULL,
    vl_count bigint NOT NULL,
    vl_created_at timestamp with time zone NOT NULL,
    vl_last_checked timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: video_list_vl_id_seq; Type: SEQUENCE; Schema: app_private; Owner: -
--

CREATE SEQUENCE app_private.video_list_vl_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: video_list_vl_id_seq; Type: SEQUENCE OWNED BY; Schema: app_private; Owner: -
--

ALTER SEQUENCE app_private.video_list_vl_id_seq OWNED BY app_private.video_list.vl_id;


--
-- Name: video_source; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.video_source (
    vs_id bigint NOT NULL,
    vs_url text NOT NULL,
    vs_title text,
    vs_description text,
    vs_created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    vs_last_checked timestamp with time zone
);


--
-- Name: video_source_vs_id_seq; Type: SEQUENCE; Schema: app_private; Owner: -
--

CREATE SEQUENCE app_private.video_source_vs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: video_source_vs_id_seq; Type: SEQUENCE OWNED BY; Schema: app_private; Owner: -
--

ALTER SEQUENCE app_private.video_source_vs_id_seq OWNED BY app_private.video_source.vs_id;


--
-- Name: video_v_id_seq; Type: SEQUENCE; Schema: app_private; Owner: -
--

CREATE SEQUENCE app_private.video_v_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: video_v_id_seq; Type: SEQUENCE OWNED BY; Schema: app_private; Owner: -
--

ALTER SEQUENCE app_private.video_v_id_seq OWNED BY app_private.video.v_id;


--
-- Name: akce; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.akce AS
 SELECT event.id AS a_id,
    event.name AS a_jmeno,
    event.location_text AS a_kde,
    event.description AS a_info,
    event.since AS a_od,
    event.until AS a_do,
    event.capacity AS a_kapacita,
    event.files_legacy AS a_dokumenty,
    event.updated_at AS a_timestamp,
    event.is_locked AS a_lock,
    event.is_visible AS a_visible,
    event.summary,
    event.is_public,
    event.enable_notes
   FROM public.event;


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

ALTER SEQUENCE public.akce_a_id_seq OWNED BY public.event.id;


--
-- Name: attendee_user; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.attendee_user (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    user_id bigint NOT NULL,
    birth_year smallint NOT NULL,
    notes text DEFAULT ''::text NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: akce_item; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.akce_item AS
 SELECT attendee_user.id AS ai_id,
    attendee_user.event_id AS ai_id_rodic,
    attendee_user.user_id AS ai_user,
    attendee_user.birth_year AS ai_rok_narozeni,
    attendee_user.notes
   FROM public.attendee_user;


--
-- Name: VIEW akce_item; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.akce_item IS '@foreignKey (ai_id_rodic) references akce (a_id)
@foreignKey (ai_user) references users (u_id)';


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

ALTER SEQUENCE public.akce_item_ai_id_seq OWNED BY public.attendee_user.id;


--
-- Name: aktuality; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.aktuality (
    at_id bigint NOT NULL,
    at_kdo bigint,
    at_kat text DEFAULT '1'::text NOT NULL,
    at_jmeno text NOT NULL,
    at_text text NOT NULL,
    at_preview text NOT NULL,
    at_foto bigint,
    at_foto_main bigint,
    at_timestamp timestamp with time zone,
    at_timestamp_add timestamp with time zone DEFAULT now(),
    id bigint GENERATED ALWAYS AS (at_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
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
-- Name: attachment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.attachment (
    object_name text NOT NULL,
    preview_object_name text,
    uploaded_by bigint,
    uploaded_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: attendee_external; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.attendee_external (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    first_name text NOT NULL,
    last_name text NOT NULL,
    email text NOT NULL,
    phone text NOT NULL,
    notes text DEFAULT ''::text NOT NULL,
    birth_number text,
    guardian_name text DEFAULT ''::text NOT NULL,
    managed_by bigint,
    confirmed_by bigint,
    confirmed_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: attendee_external_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.attendee_external ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.attendee_external_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: cohort_group; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.cohort_group (
    id bigint NOT NULL,
    name text NOT NULL,
    description text DEFAULT '[]'::jsonb NOT NULL,
    ordering integer DEFAULT 1 NOT NULL,
    is_public boolean DEFAULT true NOT NULL,
    tenant bigint,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: cohort_group_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.cohort_group ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.cohort_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


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
    d_timestamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    id bigint GENERATED ALWAYS AS (d_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
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
-- Name: form_responses; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.form_responses (
    id bigint NOT NULL,
    type text NOT NULL,
    data jsonb NOT NULL,
    url text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: form_responses_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.form_responses ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.form_responses_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: galerie_dir; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.galerie_dir (
    gd_id bigint NOT NULL,
    gd_id_rodic bigint NOT NULL,
    gd_name text NOT NULL,
    gd_level smallint DEFAULT '1'::smallint NOT NULL,
    gd_path text NOT NULL,
    gd_hidden boolean DEFAULT true NOT NULL,
    id bigint GENERATED ALWAYS AS (gd_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
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
    gf_timestamp timestamp with time zone,
    id bigint GENERATED ALWAYS AS (gf_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
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
-- Name: location; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.location (
    id bigint NOT NULL,
    name text NOT NULL,
    description jsonb NOT NULL
);


--
-- Name: location_attachment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.location_attachment (
    location_id bigint NOT NULL,
    object_name text NOT NULL
);


--
-- Name: location_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.location ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.location_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: nabidka_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.nabidka_item (
    ni_id bigint NOT NULL,
    ni_id_rodic bigint NOT NULL,
    ni_partner bigint NOT NULL,
    ni_pocet_hod smallint DEFAULT '1'::smallint NOT NULL,
    ni_lock boolean DEFAULT true NOT NULL,
    id bigint GENERATED ALWAYS AS (ni_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
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
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    title text DEFAULT ''::text NOT NULL
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
    title text DEFAULT ''::text NOT NULL,
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
-- Name: pary_navrh; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pary_navrh (
    pn_id bigint NOT NULL,
    pn_navrhl bigint NOT NULL,
    pn_partner bigint NOT NULL,
    pn_partnerka bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pn_id) STORED
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
-- Name: person; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.person (
    id bigint NOT NULL,
    first_name text NOT NULL,
    last_name text NOT NULL,
    gender public.gender_type NOT NULL
);


--
-- Name: person_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.person ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.person_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


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
    pc_visible boolean DEFAULT true NOT NULL,
    id bigint GENERATED ALWAYS AS (pc_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: platby_category_group; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.platby_category_group (
    pcg_id bigint NOT NULL,
    pcg_id_group bigint NOT NULL,
    pcg_id_category bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pcg_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


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
-- Name: platby_group; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.platby_group (
    pg_id bigint NOT NULL,
    pg_type numeric DEFAULT '1'::numeric NOT NULL,
    pg_name text NOT NULL,
    pg_description text NOT NULL,
    pg_base bigint DEFAULT '0'::bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pg_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


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
-- Name: platby_group_skupina; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.platby_group_skupina (
    pgs_id bigint NOT NULL,
    pgs_id_skupina bigint NOT NULL,
    pgs_id_group bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pgs_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


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
-- Name: platby_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.platby_item (
    pi_id bigint NOT NULL,
    pi_id_user bigint,
    pi_id_category bigint NOT NULL,
    pi_id_raw bigint,
    pi_amount numeric(10,2) NOT NULL,
    pi_date date NOT NULL,
    pi_prefix integer DEFAULT 2000 NOT NULL,
    id bigint GENERATED ALWAYS AS (pi_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


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
    pr_discarded boolean DEFAULT true NOT NULL,
    id bigint GENERATED ALWAYS AS (pr_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
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
-- Name: room; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.room (
    id bigint NOT NULL,
    name text NOT NULL,
    description jsonb NOT NULL,
    location bigint
);


--
-- Name: room_attachment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.room_attachment (
    room_id bigint NOT NULL,
    object_name text NOT NULL
);


--
-- Name: room_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.room ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.room_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
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
-- Name: skupiny; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.skupiny (
    s_id bigint NOT NULL,
    s_name text NOT NULL,
    s_description text NOT NULL,
    s_color_rgb text NOT NULL,
    s_color_text text DEFAULT ''::text NOT NULL,
    s_location text DEFAULT ''::text NOT NULL,
    s_visible boolean DEFAULT true NOT NULL,
    ordering integer DEFAULT 1 NOT NULL,
    internal_info text DEFAULT '[]'::jsonb NOT NULL,
    cohort_group bigint,
    id bigint GENERATED ALWAYS AS (s_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
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
-- Name: tenant_attachment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tenant_attachment (
    tenant_id bigint NOT NULL,
    object_name text NOT NULL,
    type public.tenant_attachment_type
);


--
-- Name: tenant_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.tenant ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.tenant_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: tenant_location; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tenant_location (
    tenant_id bigint NOT NULL,
    location_id bigint NOT NULL
);


--
-- Name: tenant_person; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tenant_person (
    tenant_id bigint NOT NULL,
    person_id bigint NOT NULL
);


--
-- Name: upozorneni_skupiny; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.upozorneni_skupiny (
    ups_id bigint NOT NULL,
    ups_id_rodic bigint NOT NULL,
    ups_id_skupina bigint NOT NULL,
    ups_color text NOT NULL,
    ups_popis text NOT NULL,
    id bigint GENERATED ALWAYS AS (ups_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
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
-- Name: crm_activity id; Type: DEFAULT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.crm_activity ALTER COLUMN id SET DEFAULT nextval('app_private.crm_activity_id_seq'::regclass);


--
-- Name: crm_prospect id; Type: DEFAULT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.crm_prospect ALTER COLUMN id SET DEFAULT nextval('app_private.crm_prospect_id_seq'::regclass);


--
-- Name: video v_id; Type: DEFAULT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.video ALTER COLUMN v_id SET DEFAULT nextval('app_private.video_v_id_seq'::regclass);


--
-- Name: video_list vl_id; Type: DEFAULT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.video_list ALTER COLUMN vl_id SET DEFAULT nextval('app_private.video_list_vl_id_seq'::regclass);


--
-- Name: video_source vs_id; Type: DEFAULT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.video_source ALTER COLUMN vs_id SET DEFAULT nextval('app_private.video_source_vs_id_seq'::regclass);


--
-- Name: aktuality at_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality ALTER COLUMN at_id SET DEFAULT nextval('public.aktuality_at_id_seq'::regclass);


--
-- Name: attendee_user id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendee_user ALTER COLUMN id SET DEFAULT nextval('public.akce_item_ai_id_seq'::regclass);


--
-- Name: dokumenty d_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dokumenty ALTER COLUMN d_id SET DEFAULT nextval('public.dokumenty_d_id_seq'::regclass);


--
-- Name: event id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event ALTER COLUMN id SET DEFAULT nextval('public.akce_a_id_seq'::regclass);


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
-- Name: crm_activity crm_activity_pkey; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.crm_activity
    ADD CONSTRAINT crm_activity_pkey PRIMARY KEY (id);


--
-- Name: crm_prospect crm_prospect_pkey; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.crm_prospect
    ADD CONSTRAINT crm_prospect_pkey PRIMARY KEY (id);


--
-- Name: video idx_23999_primary; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.video
    ADD CONSTRAINT idx_23999_primary PRIMARY KEY (v_id);


--
-- Name: video_list idx_24009_primary; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.video_list
    ADD CONSTRAINT idx_24009_primary PRIMARY KEY (vl_id);


--
-- Name: video_source idx_24019_primary; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.video_source
    ADD CONSTRAINT idx_24019_primary PRIMARY KEY (vs_id);


--
-- Name: attendee_user akce_item_unique_user_event_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendee_user
    ADD CONSTRAINT akce_item_unique_user_event_key UNIQUE (user_id, event_id);


--
-- Name: attachment attachment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT attachment_pkey PRIMARY KEY (object_name);


--
-- Name: attendee_external attendee_external_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendee_external
    ADD CONSTRAINT attendee_external_pkey PRIMARY KEY (id);


--
-- Name: cohort_group cohort_group_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_group
    ADD CONSTRAINT cohort_group_pkey PRIMARY KEY (id);


--
-- Name: form_responses form_responses_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.form_responses
    ADD CONSTRAINT form_responses_pkey PRIMARY KEY (id);


--
-- Name: event idx_23735_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event
    ADD CONSTRAINT idx_23735_primary PRIMARY KEY (id);


--
-- Name: attendee_user idx_23747_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendee_user
    ADD CONSTRAINT idx_23747_primary PRIMARY KEY (id);


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
-- Name: location_attachment location_attachment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location_attachment
    ADD CONSTRAINT location_attachment_pkey PRIMARY KEY (location_id, object_name);


--
-- Name: location location_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location
    ADD CONSTRAINT location_pkey PRIMARY KEY (id);


--
-- Name: nabidka_item nabidka_item_unique_user_nabidka_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nabidka_item
    ADD CONSTRAINT nabidka_item_unique_user_nabidka_key UNIQUE (ni_partner, ni_id_rodic);


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
-- Name: person person_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person
    ADD CONSTRAINT person_pkey PRIMARY KEY (id);


--
-- Name: room_attachment room_attachment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room_attachment
    ADD CONSTRAINT room_attachment_pkey PRIMARY KEY (room_id, object_name);


--
-- Name: room room_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room
    ADD CONSTRAINT room_pkey PRIMARY KEY (id);


--
-- Name: tenant_attachment tenant_attachment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_attachment
    ADD CONSTRAINT tenant_attachment_pkey PRIMARY KEY (tenant_id, object_name);


--
-- Name: tenant_location tenant_location_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_location
    ADD CONSTRAINT tenant_location_pkey PRIMARY KEY (tenant_id, location_id);


--
-- Name: tenant_person tenant_person_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_person
    ADD CONSTRAINT tenant_person_pkey PRIMARY KEY (tenant_id, person_id);


--
-- Name: tenant tenant_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant
    ADD CONSTRAINT tenant_pkey PRIMARY KEY (id);


--
-- Name: confirmed_by; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX confirmed_by ON public.attendee_external USING btree (confirmed_by);


--
-- Name: d_kategorie; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX d_kategorie ON public.dokumenty USING btree (d_kategorie);


--
-- Name: d_timestamp; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX d_timestamp ON public.dokumenty USING btree (d_timestamp);


--
-- Name: event_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_id ON public.attendee_external USING btree (event_id);


--
-- Name: idx_23747_akce_item_ai_id_rodic_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23747_akce_item_ai_id_rodic_fkey ON public.attendee_user USING btree (event_id);


--
-- Name: idx_23747_akce_item_ai_user_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23747_akce_item_ai_user_fkey ON public.attendee_user USING btree (user_id);


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
-- Name: is_public; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX is_public ON public.cohort_group USING btree (is_public);


--
-- Name: is_visible; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX is_visible ON public.event USING btree (is_visible);


--
-- Name: managed_by; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX managed_by ON public.attendee_external USING btree (managed_by);


--
-- Name: n_od; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX n_od ON public.nabidka USING btree (n_od);


--
-- Name: object_name; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX object_name ON public.location_attachment USING btree (object_name);


--
-- Name: ordering; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ordering ON public.cohort_group USING btree (ordering);


--
-- Name: person_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX person_id ON public.tenant_person USING btree (person_id);


--
-- Name: r_datum; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX r_datum ON public.rozpis USING btree (r_datum);


--
-- Name: ri_od; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ri_od ON public.rozpis_item USING btree (ri_od);


--
-- Name: s_visible; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX s_visible ON public.skupiny USING btree (s_visible);


--
-- Name: since; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX since ON public.event USING btree (since);


--
-- Name: ss_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ss_user ON public.session USING btree (ss_user);


--
-- Name: tenant_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_id ON public.aktuality USING btree (tenant_id);


--
-- Name: tenant_location_location_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_location_location_id_idx ON public.tenant_location USING btree (location_id);


--
-- Name: type; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX type ON public.form_responses USING btree (type);


--
-- Name: u_ban; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX u_ban ON public.users USING btree (u_ban);


--
-- Name: u_confirmed; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX u_confirmed ON public.users USING btree (u_confirmed);


--
-- Name: u_jmeno; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX u_jmeno ON public.users USING btree (u_jmeno);


--
-- Name: u_prijmeni; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX u_prijmeni ON public.users USING btree (u_prijmeni);


--
-- Name: u_system; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX u_system ON public.users USING btree (u_system);


--
-- Name: updated_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX updated_at ON public.form_responses USING btree (updated_at);


--
-- Name: uploaded_by; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX uploaded_by ON public.attachment USING btree (uploaded_by);


--
-- Name: crm_activity _100_timestamps; Type: TRIGGER; Schema: app_private; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON app_private.crm_activity FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: crm_prospect _100_timestamps; Type: TRIGGER; Schema: app_private; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON app_private.crm_prospect FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: crm_activity crm_copy_to_form_responses; Type: TRIGGER; Schema: app_private; Owner: -
--

CREATE TRIGGER crm_copy_to_form_responses AFTER INSERT OR UPDATE ON app_private.crm_activity FOR EACH ROW EXECUTE FUNCTION public.crm_copy_to_form_responses();


--
-- Name: page _100_page_revision; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_page_revision AFTER INSERT OR DELETE OR UPDATE ON public.page FOR EACH ROW EXECUTE FUNCTION app_private.insert_revision();


--
-- Name: form_responses _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.form_responses FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: page _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.page FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: users _200_encrypt_password; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_encrypt_password BEFORE INSERT OR UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg_users__encrypt_password();


--
-- Name: users _500_notify_admin; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_notify_admin AFTER INSERT ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg_users__notify_admin();


--
-- Name: dokumenty on_delete_file_dokumenty; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_delete_file_dokumenty AFTER DELETE ON public.dokumenty FOR EACH ROW EXECUTE FUNCTION public.on_delete_file_dokumenty();


--
-- Name: aktuality on_update_author; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_author BEFORE UPDATE ON public.aktuality FOR EACH ROW EXECUTE FUNCTION public.on_update_author_aktuality();


--
-- Name: upozorneni on_update_author_upozorneni; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_author_upozorneni BEFORE INSERT OR UPDATE ON public.upozorneni FOR EACH ROW EXECUTE FUNCTION public.on_update_author_upozorneni();


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
-- Name: event on_update_event_timestamp; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_event_timestamp BEFORE INSERT OR UPDATE ON public.event FOR EACH ROW EXECUTE FUNCTION public.on_update_event_timestamp();


--
-- Name: crm_activity crm_activity_prospect_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.crm_activity
    ADD CONSTRAINT crm_activity_prospect_fkey FOREIGN KEY (prospect) REFERENCES app_private.crm_prospect(id);


--
-- Name: attendee_user akce_item_ai_id_rodic_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendee_user
    ADD CONSTRAINT akce_item_ai_id_rodic_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: attendee_user akce_item_ai_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendee_user
    ADD CONSTRAINT akce_item_ai_user_fkey FOREIGN KEY (user_id) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


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
-- Name: attachment attachment_uploaded_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT attachment_uploaded_by_fkey FOREIGN KEY (uploaded_by) REFERENCES public.users(u_id);


--
-- Name: attendee_external attendee_external_confirmed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendee_external
    ADD CONSTRAINT attendee_external_confirmed_by_fkey FOREIGN KEY (confirmed_by) REFERENCES public.users(u_id);


--
-- Name: attendee_external attendee_external_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendee_external
    ADD CONSTRAINT attendee_external_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id);


--
-- Name: attendee_external attendee_external_managed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendee_external
    ADD CONSTRAINT attendee_external_managed_by_fkey FOREIGN KEY (managed_by) REFERENCES public.users(u_id);


--
-- Name: cohort_group cohort_group_tenant_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_group
    ADD CONSTRAINT cohort_group_tenant_fkey FOREIGN KEY (tenant) REFERENCES public.tenant(id);


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
-- Name: location_attachment location_attachment_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location_attachment
    ADD CONSTRAINT location_attachment_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.location(id);


--
-- Name: location_attachment location_attachment_object_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location_attachment
    ADD CONSTRAINT location_attachment_object_name_fkey FOREIGN KEY (object_name) REFERENCES public.attachment(object_name);


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
    ADD CONSTRAINT pary_p_id_partner_fkey FOREIGN KEY (p_id_partner) REFERENCES public.users(u_id) ON UPDATE CASCADE ON DELETE CASCADE;


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
-- Name: room_attachment room_attachment_object_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room_attachment
    ADD CONSTRAINT room_attachment_object_name_fkey FOREIGN KEY (object_name) REFERENCES public.attachment(object_name);


--
-- Name: room_attachment room_attachment_room_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room_attachment
    ADD CONSTRAINT room_attachment_room_id_fkey FOREIGN KEY (room_id) REFERENCES public.room(id);


--
-- Name: room room_location_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room
    ADD CONSTRAINT room_location_fkey FOREIGN KEY (location) REFERENCES public.location(id);


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
-- Name: skupiny skupiny_cohort_group_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.skupiny
    ADD CONSTRAINT skupiny_cohort_group_fkey FOREIGN KEY (cohort_group) REFERENCES public.cohort_group(id);


--
-- Name: tenant_attachment tenant_attachment_object_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_attachment
    ADD CONSTRAINT tenant_attachment_object_name_fkey FOREIGN KEY (object_name) REFERENCES public.attachment(object_name);


--
-- Name: tenant_attachment tenant_attachment_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_attachment
    ADD CONSTRAINT tenant_attachment_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: tenant_location tenant_location_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_location
    ADD CONSTRAINT tenant_location_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.location(id);


--
-- Name: tenant_location tenant_location_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_location
    ADD CONSTRAINT tenant_location_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: tenant_person tenant_person_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_person
    ADD CONSTRAINT tenant_person_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id);


--
-- Name: tenant_person tenant_person_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_person
    ADD CONSTRAINT tenant_person_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: upozorneni_skupiny upozorneni_skupiny_ups_id_rodic_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT upozorneni_skupiny_ups_id_rodic_fkey FOREIGN KEY (ups_id_rodic) REFERENCES public.upozorneni(up_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: upozorneni_skupiny upozorneni_skupiny_ups_id_skupina_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT upozorneni_skupiny_ups_id_skupina_fkey FOREIGN KEY (ups_id_skupina) REFERENCES public.skupiny(s_id) ON UPDATE CASCADE ON DELETE CASCADE;


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
-- Name: video admin_all; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY admin_all ON app_private.video TO administrator USING (true) WITH CHECK (true);


--
-- Name: video_list admin_all; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY admin_all ON app_private.video_list TO administrator USING (true) WITH CHECK (true);


--
-- Name: video_source admin_all; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY admin_all ON app_private.video_source TO administrator USING (true) WITH CHECK (true);


--
-- Name: video all_view; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY all_view ON app_private.video FOR SELECT USING (true);


--
-- Name: video_list all_view; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY all_view ON app_private.video_list FOR SELECT USING (true);


--
-- Name: video_source all_view; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY all_view ON app_private.video_source FOR SELECT USING (true);


--
-- Name: video; Type: ROW SECURITY; Schema: app_private; Owner: -
--

ALTER TABLE app_private.video ENABLE ROW LEVEL SECURITY;

--
-- Name: video_list; Type: ROW SECURITY; Schema: app_private; Owner: -
--

ALTER TABLE app_private.video_list ENABLE ROW LEVEL SECURITY;

--
-- Name: video_source; Type: ROW SECURITY; Schema: app_private; Owner: -
--

ALTER TABLE app_private.video_source ENABLE ROW LEVEL SECURITY;

--
-- Name: aktuality admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.aktuality TO administrator USING (true) WITH CHECK (true);


--
-- Name: attachment admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.attachment TO administrator USING (true) WITH CHECK (true);


--
-- Name: attendee_external admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.attendee_external TO administrator USING (true) WITH CHECK (true);


--
-- Name: attendee_user admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.attendee_user TO administrator USING (true) WITH CHECK (true);


--
-- Name: cohort_group admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.cohort_group TO administrator USING (true) WITH CHECK (true);


--
-- Name: dokumenty admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.dokumenty TO administrator USING (true) WITH CHECK (true);


--
-- Name: form_responses admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.form_responses TO administrator USING (true) WITH CHECK (true);


--
-- Name: galerie_dir admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.galerie_dir TO administrator USING (true) WITH CHECK (true);


--
-- Name: galerie_foto admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.galerie_foto TO administrator USING (true) WITH CHECK (true);


--
-- Name: location admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.location TO administrator USING (true) WITH CHECK (true);


--
-- Name: location_attachment admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.location_attachment TO administrator USING (true) WITH CHECK (true);


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
-- Name: pary_navrh admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.pary_navrh TO administrator USING (true) WITH CHECK (true);


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
-- Name: room admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.room TO administrator USING (true) WITH CHECK (true);


--
-- Name: room_attachment admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.room_attachment TO administrator USING (true) WITH CHECK (true);


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
-- Name: tenant admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant TO administrator USING (true) WITH CHECK (true);


--
-- Name: tenant_attachment admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant_attachment TO administrator USING (true) WITH CHECK (true);


--
-- Name: tenant_location admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant_location TO administrator USING (true) WITH CHECK (true);


--
-- Name: tenant_person admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant_person TO administrator USING (true) WITH CHECK (true);


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
-- Name: aktuality; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.aktuality ENABLE ROW LEVEL SECURITY;

--
-- Name: aktuality all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.aktuality FOR SELECT USING (true);


--
-- Name: galerie_dir all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.galerie_dir FOR SELECT USING (true);


--
-- Name: galerie_foto all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.galerie_foto FOR SELECT USING (true);


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
-- Name: skupiny all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.skupiny FOR SELECT USING (true);


--
-- Name: users all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.users FOR SELECT TO member USING (true);


--
-- Name: attachment; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.attachment ENABLE ROW LEVEL SECURITY;

--
-- Name: attendee_external; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.attendee_external ENABLE ROW LEVEL SECURITY;

--
-- Name: attendee_user; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.attendee_user ENABLE ROW LEVEL SECURITY;

--
-- Name: cohort_group; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.cohort_group ENABLE ROW LEVEL SECURITY;

--
-- Name: dokumenty; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.dokumenty ENABLE ROW LEVEL SECURITY;

--
-- Name: event; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event ENABLE ROW LEVEL SECURITY;

--
-- Name: form_responses; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.form_responses ENABLE ROW LEVEL SECURITY;

--
-- Name: galerie_dir; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.galerie_dir ENABLE ROW LEVEL SECURITY;

--
-- Name: galerie_foto; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.galerie_foto ENABLE ROW LEVEL SECURITY;

--
-- Name: location; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.location ENABLE ROW LEVEL SECURITY;

--
-- Name: location_attachment; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.location_attachment ENABLE ROW LEVEL SECURITY;

--
-- Name: person manage_admin; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_admin ON public.person TO administrator USING (true) WITH CHECK (true);


--
-- Name: event manage_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_all ON public.event TO administrator USING (true) WITH CHECK (true);


--
-- Name: attendee_user manage_own; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_own ON public.attendee_user TO member USING ((user_id = public.current_user_id())) WITH CHECK ((user_id = public.current_user_id()));


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
-- Name: nabidka member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.nabidka FOR SELECT TO member;


--
-- Name: nabidka_item member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.nabidka_item FOR SELECT TO member;


--
-- Name: platby_category member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.platby_category FOR SELECT TO member;


--
-- Name: platby_category_group member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.platby_category_group FOR SELECT TO member;


--
-- Name: platby_group member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.platby_group FOR SELECT TO member;


--
-- Name: platby_group_skupina member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.platby_group_skupina FOR SELECT TO member;


--
-- Name: platby_item member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.platby_item FOR SELECT TO member USING ((pi_id_user = public.current_user_id()));


--
-- Name: platby_raw member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.platby_raw FOR SELECT TO member USING ((EXISTS ( SELECT
   FROM public.platby_item
  WHERE ((platby_item.pi_id_raw = platby_raw.pr_id) AND (platby_item.pi_id_user = public.current_user_id())))));


--
-- Name: rozpis member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.rozpis FOR SELECT TO member;


--
-- Name: rozpis_item member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.rozpis_item FOR SELECT TO member;


--
-- Name: upozorneni member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.upozorneni FOR SELECT TO member;


--
-- Name: upozorneni_skupiny member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.upozorneni_skupiny FOR SELECT TO member;


--
-- Name: aktuality my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.aktuality AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: attendee_external my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.attendee_external AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: attendee_user my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.attendee_user AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: cohort_group my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.cohort_group AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: dokumenty my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.dokumenty AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: event my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.event AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: form_responses my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.form_responses AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: galerie_dir my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.galerie_dir AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: galerie_foto my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.galerie_foto AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: nabidka my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.nabidka AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: nabidka_item my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.nabidka_item AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: platby_category my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.platby_category AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: platby_category_group my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.platby_category_group AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: platby_group my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.platby_group AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: platby_group_skupina my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.platby_group_skupina AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: platby_item my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.platby_item AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: platby_raw my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.platby_raw AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: rozpis my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.rozpis AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: rozpis_item my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.rozpis_item AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: skupiny my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.skupiny AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: tenant my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.tenant AS RESTRICTIVE USING ((id = public.current_tenant_id())) WITH CHECK ((id = public.current_tenant_id()));


--
-- Name: tenant_attachment my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.tenant_attachment AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: tenant_person my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.tenant_person AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: upozorneni my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.upozorneni AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: upozorneni_skupiny my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.upozorneni_skupiny AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: users my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.users AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


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
-- Name: pary_navrh; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.pary_navrh ENABLE ROW LEVEL SECURITY;

--
-- Name: permissions; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.permissions ENABLE ROW LEVEL SECURITY;

--
-- Name: person; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.person ENABLE ROW LEVEL SECURITY;

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
-- Name: attachment public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.attachment FOR SELECT TO anonymous;


--
-- Name: cohort_group public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.cohort_group FOR SELECT TO anonymous;


--
-- Name: dokumenty public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.dokumenty FOR SELECT TO member;


--
-- Name: location public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.location FOR SELECT TO anonymous;


--
-- Name: location_attachment public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.location_attachment FOR SELECT TO anonymous;


--
-- Name: room public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.room FOR SELECT TO anonymous;


--
-- Name: room_attachment public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.room_attachment FOR SELECT TO anonymous;


--
-- Name: tenant public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant FOR SELECT TO anonymous;


--
-- Name: tenant_attachment public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant_attachment FOR SELECT TO anonymous;


--
-- Name: tenant_location public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant_location FOR SELECT TO anonymous;


--
-- Name: tenant_person public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant_person FOR SELECT TO anonymous;


--
-- Name: users register_anonymous; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY register_anonymous ON public.users FOR INSERT WITH CHECK ((u_confirmed = false));


--
-- Name: room; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.room ENABLE ROW LEVEL SECURITY;

--
-- Name: room_attachment; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.room_attachment ENABLE ROW LEVEL SECURITY;

--
-- Name: rozpis; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.rozpis ENABLE ROW LEVEL SECURITY;

--
-- Name: rozpis_item; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.rozpis_item ENABLE ROW LEVEL SECURITY;

--
-- Name: attendee_external select_member; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY select_member ON public.attendee_external FOR SELECT TO member USING (true);


--
-- Name: attendee_user select_member; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY select_member ON public.attendee_user FOR SELECT TO member USING (true);


--
-- Name: event select_member; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY select_member ON public.event FOR SELECT TO member USING (true);


--
-- Name: event select_public; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY select_public ON public.event FOR SELECT TO anonymous USING ((is_public = true));


--
-- Name: session; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.session ENABLE ROW LEVEL SECURITY;

--
-- Name: skupiny; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.skupiny ENABLE ROW LEVEL SECURITY;

--
-- Name: tenant; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant ENABLE ROW LEVEL SECURITY;

--
-- Name: tenant_attachment; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant_attachment ENABLE ROW LEVEL SECURITY;

--
-- Name: tenant_location; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant_location ENABLE ROW LEVEL SECURITY;

--
-- Name: tenant_person; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant_person ENABLE ROW LEVEL SECURITY;

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
-- Name: person view_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_all ON public.person FOR SELECT USING (true);


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
GRANT ALL ON SCHEMA public TO anonymous;


--
-- Name: TABLE pary; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.pary TO anonymous;


--
-- Name: FUNCTION active_couples(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.active_couples() TO member;


--
-- Name: FUNCTION current_tenant_id(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_tenant_id() TO anonymous;


--
-- Name: TABLE event; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event TO anonymous;


--
-- Name: FUNCTION akce_free_slots(a public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.akce_free_slots(a public.event) TO anonymous;


--
-- Name: FUNCTION akce_has_capacity(a public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.akce_has_capacity(a public.event) TO anonymous;


--
-- Name: FUNCTION akce_my_notes(a public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.akce_my_notes(a public.event) TO anonymous;


--
-- Name: FUNCTION akce_signed_up(a public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.akce_signed_up(a public.event) TO anonymous;


--
-- Name: TABLE rozpis_item; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.rozpis_item TO anonymous;


--
-- Name: FUNCTION book_lesson(lesson_id bigint); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.book_lesson(lesson_id bigint) TO member;


--
-- Name: FUNCTION cancel_lesson(lesson_id bigint); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.cancel_lesson(lesson_id bigint) TO member;


--
-- Name: FUNCTION cancel_participation(event_id bigint); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.cancel_participation(event_id bigint) TO member;


--
-- Name: FUNCTION change_password(old_pass character varying, new_pass character varying); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.change_password(old_pass character varying, new_pass character varying) TO member;


--
-- Name: FUNCTION confirm_user(id bigint, grp bigint, cohort bigint); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.confirm_user(id bigint, grp bigint, cohort bigint) TO administrator;


--
-- Name: FUNCTION create_couple(man bigint, woman bigint); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.create_couple(man bigint, woman bigint) TO administrator;


--
-- Name: FUNCTION create_participation(event_id bigint, year_of_birth integer, my_notes text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.create_participation(event_id bigint, year_of_birth integer, my_notes text) TO member;


--
-- Name: FUNCTION create_participation_external(event_id bigint, first_name text, last_name text, guardian_name text, email text, phone text, notes text, birth_number text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.create_participation_external(event_id bigint, first_name text, last_name text, guardian_name text, email text, phone text, notes text, birth_number text) TO anonymous;


--
-- Name: FUNCTION current_couple_ids(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_couple_ids() TO anonymous;


--
-- Name: TABLE permissions; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.permissions TO anonymous;


--
-- Name: FUNCTION current_session_id(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_session_id() TO anonymous;


--
-- Name: FUNCTION current_user_id(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_user_id() TO anonymous;


--
-- Name: FUNCTION event_remaining_spots(a public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_remaining_spots(a public.event) TO anonymous;


--
-- Name: FUNCTION fix_unpaired_couples(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.fix_unpaired_couples() TO administrator;


--
-- Name: FUNCTION get_current_couple(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.get_current_couple() TO anonymous;


--
-- Name: TABLE tenant; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant TO anonymous;


--
-- Name: FUNCTION get_current_tenant(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.get_current_tenant() TO anonymous;


--
-- Name: TABLE users; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_id; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_id) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_login; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_login) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_pass; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_pass) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_jmeno; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_jmeno) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_prijmeni; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_prijmeni) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_pohlavi; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_pohlavi) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_email; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_email) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_telefon; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_telefon) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_narozeni; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_narozeni) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_rodne_cislo; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_rodne_cislo) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_poznamky; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_poznamky) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_dancer; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_dancer) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_street; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_street) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_conscription_number; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_conscription_number) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_orientation_number; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_orientation_number) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_district; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_district) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_city; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_city) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_postal_code; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_postal_code) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_nationality; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_nationality) ON TABLE public.users TO anonymous;


--
-- Name: FUNCTION get_current_user(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.get_current_user() TO anonymous;


--
-- Name: TABLE session; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.session TO anonymous;


--
-- Name: FUNCTION login(login character varying, passwd character varying, OUT couple public.pary, OUT sess public.session, OUT usr public.users); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.login(login character varying, passwd character varying, OUT couple public.pary, OUT sess public.session, OUT usr public.users) TO anonymous;


--
-- Name: FUNCTION logout(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.logout() TO anonymous;


--
-- Name: TABLE upozorneni; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.upozorneni TO anonymous;


--
-- Name: FUNCTION my_announcements(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.my_announcements() TO member;


--
-- Name: FUNCTION my_lessons(start_date date, end_date date); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.my_lessons(start_date date, end_date date) TO member;


--
-- Name: TABLE nabidka; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.nabidka TO anonymous;


--
-- Name: FUNCTION nabidka_free_lessons(n public.nabidka); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.nabidka_free_lessons(n public.nabidka) TO anonymous;


--
-- Name: FUNCTION nabidka_my_lessons(n public.nabidka); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.nabidka_my_lessons(n public.nabidka) TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_aktuality(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_aktuality() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_aktuality() TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_dokumenty(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_dokumenty() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_dokumenty() TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_galerie_foto(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_galerie_foto() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_galerie_foto() TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_nabidka(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_nabidka() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_nabidka() TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_rozpis(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_rozpis() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_rozpis() TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_upozorneni(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_upozorneni() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_upozorneni() TO anonymous;


--
-- Name: FUNCTION on_update_current_timestamp_users(); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.on_update_current_timestamp_users() FROM PUBLIC;
GRANT ALL ON FUNCTION public.on_update_current_timestamp_users() TO anonymous;


--
-- Name: FUNCTION reservation_set_desired_lessons(reservation_id bigint, lesson_count smallint, OUT reservation public.nabidka); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.reservation_set_desired_lessons(reservation_id bigint, lesson_count smallint, OUT reservation public.nabidka) TO member;


--
-- Name: FUNCTION reservations_for_range(start_date date, end_date date); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.reservations_for_range(start_date date, end_date date) TO member;


--
-- Name: FUNCTION reset_password(login character varying, email character varying); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.reset_password(login character varying, email character varying) TO anonymous;


--
-- Name: TABLE rozpis; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.rozpis TO anonymous;


--
-- Name: FUNCTION schedules_for_range(start_date date, end_date date); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.schedules_for_range(start_date date, end_date date) TO member;


--
-- Name: FUNCTION submit_form(type text, data jsonb, url text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.submit_form(type text, data jsonb, url text) TO anonymous;


--
-- Name: TABLE video; Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON TABLE app_private.video TO anonymous;


--
-- Name: FUNCTION trainers(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.trainers() TO member;


--
-- Name: FUNCTION users_date_of_newest_payment(a public.users); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.users_date_of_newest_payment(a public.users) TO anonymous;


--
-- Name: FUNCTION users_date_of_oldest_payment(a public.users); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.users_date_of_oldest_payment(a public.users) TO anonymous;


--
-- Name: FUNCTION users_full_name(u public.users); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.users_full_name(u public.users) TO anonymous;


--
-- Name: FUNCTION users_has_valid_payment(a public.users); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.users_has_valid_payment(a public.users) TO anonymous;


--
-- Name: FUNCTION users_in_public_cohort(a public.users); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.users_in_public_cohort(a public.users) TO anonymous;


--
-- Name: TABLE video_list; Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON TABLE app_private.video_list TO anonymous;


--
-- Name: SEQUENCE video_list_vl_id_seq; Type: ACL; Schema: app_private; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE app_private.video_list_vl_id_seq TO anonymous;


--
-- Name: TABLE video_source; Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON TABLE app_private.video_source TO anonymous;


--
-- Name: SEQUENCE video_source_vs_id_seq; Type: ACL; Schema: app_private; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE app_private.video_source_vs_id_seq TO anonymous;


--
-- Name: SEQUENCE video_v_id_seq; Type: ACL; Schema: app_private; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE app_private.video_v_id_seq TO anonymous;


--
-- Name: TABLE akce; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.akce TO anonymous;


--
-- Name: SEQUENCE akce_a_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.akce_a_id_seq TO anonymous;


--
-- Name: TABLE attendee_user; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.attendee_user TO anonymous;


--
-- Name: TABLE akce_item; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.akce_item TO anonymous;


--
-- Name: SEQUENCE akce_item_ai_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.akce_item_ai_id_seq TO anonymous;


--
-- Name: TABLE aktuality; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.aktuality TO anonymous;


--
-- Name: SEQUENCE aktuality_at_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.aktuality_at_id_seq TO anonymous;


--
-- Name: TABLE attachment; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.attachment TO anonymous;


--
-- Name: TABLE attendee_external; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.attendee_external TO anonymous;


--
-- Name: TABLE cohort_group; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.cohort_group TO anonymous;


--
-- Name: TABLE dokumenty; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.dokumenty TO anonymous;


--
-- Name: SEQUENCE dokumenty_d_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.dokumenty_d_id_seq TO anonymous;


--
-- Name: TABLE form_responses; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.form_responses TO anonymous;


--
-- Name: TABLE galerie_dir; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.galerie_dir TO anonymous;


--
-- Name: SEQUENCE galerie_dir_gd_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.galerie_dir_gd_id_seq TO anonymous;


--
-- Name: TABLE galerie_foto; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.galerie_foto TO anonymous;


--
-- Name: SEQUENCE galerie_foto_gf_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.galerie_foto_gf_id_seq TO anonymous;


--
-- Name: TABLE location; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.location TO anonymous;


--
-- Name: TABLE location_attachment; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.location_attachment TO anonymous;


--
-- Name: TABLE nabidka_item; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.nabidka_item TO anonymous;


--
-- Name: SEQUENCE nabidka_item_ni_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.nabidka_item_ni_id_seq TO anonymous;


--
-- Name: SEQUENCE nabidka_n_id_seq; Type: ACL; Schema: public; Owner: -
--

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
-- Name: TABLE pary_navrh; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.pary_navrh TO anonymous;


--
-- Name: SEQUENCE pary_navrh_pn_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.pary_navrh_pn_id_seq TO anonymous;


--
-- Name: SEQUENCE pary_p_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.pary_p_id_seq TO anonymous;


--
-- Name: SEQUENCE permissions_pe_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.permissions_pe_id_seq TO anonymous;


--
-- Name: TABLE person; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.person TO anonymous;


--
-- Name: TABLE platby_category; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.platby_category TO anonymous;


--
-- Name: TABLE platby_category_group; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.platby_category_group TO anonymous;


--
-- Name: SEQUENCE platby_category_group_pcg_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_category_group_pcg_id_seq TO anonymous;


--
-- Name: SEQUENCE platby_category_pc_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_category_pc_id_seq TO anonymous;


--
-- Name: TABLE platby_group; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.platby_group TO anonymous;


--
-- Name: SEQUENCE platby_group_pg_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_group_pg_id_seq TO anonymous;


--
-- Name: TABLE platby_group_skupina; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.platby_group_skupina TO anonymous;


--
-- Name: SEQUENCE platby_group_skupina_pgs_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_group_skupina_pgs_id_seq TO anonymous;


--
-- Name: TABLE platby_item; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.platby_item TO anonymous;


--
-- Name: SEQUENCE platby_item_pi_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_item_pi_id_seq TO anonymous;


--
-- Name: TABLE platby_raw; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.platby_raw TO anonymous;


--
-- Name: SEQUENCE platby_raw_pr_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.platby_raw_pr_id_seq TO anonymous;


--
-- Name: TABLE room; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.room TO anonymous;


--
-- Name: TABLE room_attachment; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.room_attachment TO anonymous;


--
-- Name: SEQUENCE rozpis_item_ri_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.rozpis_item_ri_id_seq TO anonymous;


--
-- Name: SEQUENCE rozpis_r_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.rozpis_r_id_seq TO anonymous;


--
-- Name: TABLE skupiny; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.skupiny TO anonymous;


--
-- Name: SEQUENCE skupiny_s_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.skupiny_s_id_seq TO anonymous;


--
-- Name: TABLE tenant_attachment; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant_attachment TO anonymous;


--
-- Name: TABLE tenant_location; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant_location TO anonymous;


--
-- Name: TABLE tenant_person; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant_person TO anonymous;


--
-- Name: TABLE upozorneni_skupiny; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.upozorneni_skupiny TO anonymous;


--
-- Name: SEQUENCE upozorneni_skupiny_ups_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.upozorneni_skupiny_ups_id_seq TO anonymous;


--
-- Name: SEQUENCE upozorneni_up_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.upozorneni_up_id_seq TO anonymous;


--
-- Name: SEQUENCE users_u_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.users_u_id_seq TO anonymous;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: public; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT SELECT,USAGE ON SEQUENCES  TO anonymous;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: public; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT ALL ON FUNCTIONS  TO anonymous;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: -; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres REVOKE ALL ON FUNCTIONS  FROM PUBLIC;


--
-- PostgreSQL database dump complete
--

