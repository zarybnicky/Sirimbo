--
-- PostgreSQL database dump
--

-- Dumped from database version 15.3
-- Dumped by pg_dump version 15.3

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
-- Name: public; Type: SCHEMA; Schema: -; Owner: -
--

-- *not* creating schema, since initdb creates it


--
-- Name: btree_gist; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS btree_gist WITH SCHEMA public;


--
-- Name: EXTENSION btree_gist; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION btree_gist IS 'support for indexing common datatypes in GiST';


--
-- Name: citext; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS citext WITH SCHEMA public;


--
-- Name: EXTENSION citext; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION citext IS 'data type for case-insensitive character strings';


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
-- Name: address_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.address_type AS (
	street text,
	conscription_number text,
	orientation_number text,
	district text,
	city text,
	region text,
	postal_code text
);


--
-- Name: address_domain; Type: DOMAIN; Schema: public; Owner: -
--

CREATE DOMAIN public.address_domain AS public.address_type
	CONSTRAINT address_domain_check CHECK (((VALUE IS NULL) OR (((VALUE).street IS NOT NULL) AND ((VALUE).conscription_number IS NOT NULL) AND ((VALUE).orientation_number IS NOT NULL) AND ((VALUE).city IS NOT NULL) AND ((VALUE).region IS NOT NULL) AND ((VALUE).postal_code IS NOT NULL))));


--
-- Name: attendance_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.attendance_type AS ENUM (
    'unknown',
    'attended',
    'excused',
    'not-excused'
);


--
-- Name: event_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_type AS ENUM (
    'camp',
    'lesson',
    'reservation',
    'holiday'
);


--
-- Name: gender_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.gender_type AS ENUM (
    'man',
    'woman',
    'unspecified'
);


--
-- Name: jwt_token; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.jwt_token AS (
	exp integer,
	user_id bigint,
	tenant_id bigint,
	username text,
	email text,
	my_person_ids bigint[],
	my_tenant_ids bigint[],
	my_cohort_ids bigint[],
	my_couple_ids bigint[],
	is_member boolean,
	is_trainer boolean,
	is_admin boolean
);


--
-- Name: payment_status; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.payment_status AS ENUM (
    'tentative',
    'unpaid',
    'paid'
);


--
-- Name: price_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.price_type AS (
	amount numeric(19,4),
	currency text
);


--
-- Name: price; Type: DOMAIN; Schema: public; Owner: -
--

CREATE DOMAIN public.price AS public.price_type
	CONSTRAINT price_check CHECK (((VALUE IS NULL) OR (((VALUE).currency IS NOT NULL) AND (length((VALUE).currency) = 3) AND ((VALUE).amount IS NOT NULL))));


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
-- Name: registration_time; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.registration_time AS ENUM (
    'pre',
    'regular',
    'post'
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
-- Name: current_tenant_id(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_tenant_id() RETURNS bigint
    LANGUAGE sql STABLE
    AS $$
  select COALESCE(current_setting('jwt.claims.tenant_id', '1')::bigint, 1);
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
    u_email text NOT NULL,
    u_poznamky text DEFAULT ''::text NOT NULL,
    u_timestamp timestamp with time zone DEFAULT now() NOT NULL,
    u_ban boolean DEFAULT true NOT NULL,
    u_confirmed boolean DEFAULT false NOT NULL,
    u_system boolean DEFAULT true NOT NULL,
    u_nationality text NOT NULL,
    u_member_since timestamp with time zone,
    u_member_until timestamp with time zone,
    u_created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    u_gdpr_signed_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    id bigint GENERATED ALWAYS AS (u_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: TABLE users; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.users IS '@omit create,update,delete';


--
-- Name: COLUMN users.u_pass; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.users.u_pass IS '@omit';


--
-- Name: COLUMN users.u_poznamky; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.users.u_poznamky IS '@omit';


--
-- Name: COLUMN users.u_ban; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.users.u_ban IS '@omit';


--
-- Name: COLUMN users.u_confirmed; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.users.u_confirmed IS '@omit';


--
-- Name: COLUMN users.u_system; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.users.u_system IS '@omit';


--
-- Name: COLUMN users.u_member_since; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.users.u_member_since IS '@omit';


--
-- Name: COLUMN users.u_member_until; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.users.u_member_until IS '@omit';


--
-- Name: COLUMN users.u_gdpr_signed_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.users.u_gdpr_signed_at IS '@omit';


--
-- Name: create_jwt_token(public.users); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.create_jwt_token(u public.users) RETURNS public.jwt_token
    LANGUAGE sql STABLE
    AS $$
  with details as (
    SELECT
      user_id,
      person.id as person_id,
      (select array_agg(tenant_id) from tenant_membership where person_id=person.id) as my_tenant_ids,
      (select array_agg(cohort_id) from cohort_membership where person_id=person.id) as my_cohort_ids,
      (select array_agg(id) from couple where man_id=person.id or woman_id=person.id) as my_couple_ids,
      exists (select 1 from tenant_membership where tenant_membership.person_id=person.id and tenant_membership.tenant_id = current_tenant_id() and now() <@ active_range) as is_member,
      exists (select 1 from tenant_trainer where tenant_trainer.person_id=person.id and tenant_trainer.tenant_id = current_tenant_id() and now() <@ active_range) as is_trainer,
      exists (select 1 from tenant_administrator where tenant_administrator.person_id=person.id and tenant_administrator.tenant_id = current_tenant_id() and now() <@ active_range) as is_admin
    from user_proxy join person on person_id=person.id where user_id=u.u_id
  ) select
    extract(epoch from now() + interval '7 days')::integer,
    u.u_id,
    current_tenant_id(),
    u.u_login,
    u.u_email,
    array_agg(person_id) as my_person_ids,
    array_accum(my_tenant_ids) as my_tenant_ids,
    array_accum(my_cohort_ids) as my_cohort_ids,
    array_accum(my_couple_ids) as my_couple_ids,
    bool_or(is_member) as is_member,
    bool_or(is_trainer) as is_trainer,
    bool_or(is_admin) as is_admin
  from details group by user_id;
$$;


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
-- Name: is_current_tenant_member(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.is_current_tenant_member() RETURNS boolean
    LANGUAGE sql
    AS $$
  select exists (select * from tenant_membership where tenant_id = current_tenant_id() and person_id = any (current_person_ids()));
$$;


--
-- Name: event_registration; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_registration (
    id bigint NOT NULL,
    status_time public.registration_time DEFAULT 'regular'::public.registration_time NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    target_cohort_id bigint,
    couple_id bigint,
    person_id bigint,
    payment_id bigint,
    note text,
    is_confirmed boolean DEFAULT public.is_current_tenant_member(),
    confirmed_at timestamp with time zone DEFAULT 
CASE public.is_current_tenant_member()
    WHEN true THEN now()
    ELSE NULL::timestamp with time zone
END,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_registration_check CHECK ((((couple_id IS NOT NULL) AND (person_id IS NULL)) OR ((couple_id IS NULL) AND (person_id IS NOT NULL))))
);


--
-- Name: TABLE event_registration; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_registration IS '@omit create,update,delete
@simpleCollections only';


--
-- Name: event_registration_person_ids(public.event_registration); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.event_registration_person_ids(e public.event_registration) RETURNS SETOF bigint
    LANGUAGE sql
    AS $$
  select e.person_id as id where e.person_id is not null
  union
  select unnest(array[man_id, woman_id]) as id from couple where couple.id = e.couple_id and e.couple_id is not null
$$;


--
-- Name: tg__person_address_primary(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg__person_address_primary() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not exists (select * from person_address where person_address.person_id = person_id) then
    NEW.is_primary = true;
  end if;
  return new;
end;
$$;


--
-- Name: tg__person_email_primary(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg__person_email_primary() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not exists (select * from person_email where person_email.person_id = person_id) then
    NEW.is_primary = true;
  end if;
  return new;
end;
$$;


--
-- Name: tg__person_phone_primary(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg__person_phone_primary() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if not exists (select * from person_phone where person_phone.person_id = person_id) then
    NEW.is_primary = true;
  end if;
  return new;
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
-- Name: tg_event_instance__create_attendance(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event_instance__create_attendance() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  insert into event_attendance (registration_id, instance_id, person_id)
  select event_registration.id, NEW.id, app_private.event_registration_person_ids(event_registration) as id
  from event_registration where event_registration.event_id = NEW.event_id
  on conflict do nothing;
  return NEW;
end;
$$;


--
-- Name: tg_event_registration__create_attendance(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event_registration__create_attendance() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  insert into event_attendance (registration_id, instance_id, person_id)
  select NEW.id, event_instance.id, app_private.event_registration_person_ids(NEW) from event_instance where event_id = NEW.event_id
  on conflict do nothing;
  return NEW;
end;
$$;


--
-- Name: tg_event_target_cohort__register_members(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event_target_cohort__register_members() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  insert into event_registration (event_id, person_id, target_cohort_id)
  select NEW.event_id, person_id, NEW.id from cohort_membership where cohort_membership.cohort_id = NEW.cohort_id and now() <@ active_range
  on conflict do nothing;
  return NEW;
end;
$$;


--
-- Name: tg_event_target_cohort__unregister_members(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event_target_cohort__unregister_members() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  delete from event_registration where target_cohort_id = OLD.id;
  return OLD;
end;
$$;


--
-- Name: tg_person_invitation__send(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_person_invitation__send() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  perform graphile_worker.add_job('send_invitation', json_build_object('id', NEW.id));
  return NEW;
end;
$$;


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
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    sticky boolean DEFAULT false NOT NULL
);


--
-- Name: archived_announcements(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.archived_announcements() RETURNS SETOF public.upozorneni
    LANGUAGE sql STABLE
    AS $$
  select upozorneni.* from upozorneni
  where is_visible = false
    or (scheduled_until is null or scheduled_until >= now())
  order by up_timestamp_add desc;
$$;


--
-- Name: attachment_directories(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.attachment_directories() RETURNS SETOF text
    LANGUAGE sql STABLE
    AS $_$
  SELECT distinct regexp_replace(object_name, '/[^/]*$', '') from attachment;
$_$;


--
-- Name: FUNCTION attachment_directories(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.attachment_directories() IS '@sortable';


--
-- Name: current_user_id(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_user_id() RETURNS bigint
    LANGUAGE sql STABLE
    AS $$
  SELECT current_setting('jwt.claims.user_id', true)::bigint;
$$;


--
-- Name: attachment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.attachment (
    object_name text NOT NULL,
    preview_object_name text,
    uploaded_by bigint DEFAULT public.current_user_id(),
    uploaded_at timestamp with time zone DEFAULT now() NOT NULL,
    thumbhash text,
    width integer,
    height integer
);


--
-- Name: TABLE attachment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.attachment IS '@omit update';


--
-- Name: attachment_directory(public.attachment); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.attachment_directory(attachment public.attachment) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
  SELECT regexp_replace(attachment.object_name, '/[^/]*$', '');
$_$;


--
-- Name: FUNCTION attachment_directory(attachment public.attachment); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.attachment_directory(attachment public.attachment) IS '@filterable';


--
-- Name: cancel_registration(bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.cancel_registration(registration_id bigint) RETURNS void
    LANGUAGE plpgsql STRICT
    AS $$
#variable_conflict use_variable
declare
  event event;
  reg event_registration;
begin
  select * into reg from event_registration er where er.id = registration_id;
  select * into event from event where id = reg.event_id;

  if event is null or reg is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked = true or reg.payment_id is not null then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if reg.person_id not in (select my_person_ids()) and reg.couple_id not in (select my_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  delete from event_registration where id = reg.id;
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
-- Name: cohort_membership; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.cohort_membership (
    cohort_id bigint NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL
);


--
-- Name: TABLE cohort_membership; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.cohort_membership IS '@omit delete
@simpleCollections only';


--
-- Name: COLUMN cohort_membership.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.cohort_membership.active_range IS '@omit';


--
-- Name: cohort_membership_active(public.cohort_membership); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.cohort_membership_active(c public.cohort_membership) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select now() <@ c.active_range;
$$;


--
-- Name: FUNCTION cohort_membership_active(c public.cohort_membership); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.cohort_membership_active(c public.cohort_membership) IS '@filterable';


--
-- Name: couple; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.couple (
    id bigint NOT NULL,
    man_id bigint NOT NULL,
    woman_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    legacy_pary_id bigint,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL
);


--
-- Name: TABLE couple; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.couple IS '@omit delete';


--
-- Name: COLUMN couple.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.couple.active_range IS '@omit';


--
-- Name: couple_active(public.couple); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.couple_active(c public.couple) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select now() <@ c.active_range;
$$;


--
-- Name: FUNCTION couple_active(c public.couple); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.couple_active(c public.couple) IS '@filterable';


--
-- Name: event_attendance; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_attendance (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    instance_id bigint NOT NULL,
    person_id bigint NOT NULL,
    status public.attendance_type DEFAULT 'unknown'::public.attendance_type NOT NULL,
    note text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    registration_id bigint NOT NULL
);


--
-- Name: TABLE event_attendance; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_attendance IS '@omit create,update,delete
@simpleCollections only';


--
-- Name: couple_attendances(public.couple); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.couple_attendances(p public.couple) RETURNS SETOF public.event_attendance
    LANGUAGE sql STABLE
    AS $$
  select event_attendance.* from event_attendance
  where person_id = p.man_id or person_id = p.woman_id;
$$;


--
-- Name: FUNCTION couple_attendances(p public.couple); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.couple_attendances(p public.couple) IS '@simpleCollections only
@filterable
@sortable';


--
-- Name: person; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.person (
    id bigint NOT NULL,
    first_name text NOT NULL,
    middle_name text,
    last_name text NOT NULL,
    gender public.gender_type NOT NULL,
    birth_date date,
    nationality text NOT NULL,
    tax_identification_number text,
    national_id_number text,
    csts_id text,
    wdsf_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    legacy_user_id bigint,
    prefix_title text DEFAULT ''::text NOT NULL,
    suffix_title text DEFAULT ''::text NOT NULL,
    bio text DEFAULT ''::text NOT NULL,
    email public.citext,
    phone text
);


--
-- Name: TABLE person; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.person IS '@omit create,delete';


--
-- Name: create_person(public.person, boolean, boolean, boolean, boolean, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_person(INOUT p public.person, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamp with time zone) RETURNS public.person
    LANGUAGE plpgsql
    AS $$
begin
  insert into person overriding user value select p.* returning * into p;
  if is_member = true then
    insert into tenant_membership (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_trainer = true then
    insert into tenant_trainer (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_admin = true then
    insert into tenant_administrator (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if send_invitation = true then
    insert into person_invitation (person_id, tenant_id) values (p.id, current_tenant_id());
  end if;
end
$$;


--
-- Name: current_couple_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_couple_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  select my_couple_ids();
$$;


--
-- Name: FUNCTION current_couple_ids(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.current_couple_ids() IS '@simpleCollections only';


--
-- Name: current_person_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_person_ids() RETURNS bigint[]
    LANGUAGE sql
    AS $$
  select array_agg(person_id) from user_proxy where user_id = current_user_id();
$$;


--
-- Name: edit_registration(bigint, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.edit_registration(registration_id bigint, note text) RETURNS public.event_registration
    LANGUAGE plpgsql STRICT
    AS $$
#variable_conflict use_variable
declare
  event event;
  reg event_registration;
begin
  select * into reg from event_registration er where er.id = registration_id;
  select * into event from event where id = reg.event_id;

  if event is null or reg is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if reg.person_id not in (select my_person_ids()) and reg.couple_id not in (select my_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  update event_registration set note=note where id = reg.id returning * into reg;
  return reg;
end;
$$;


--
-- Name: event_instance; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_instance (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    location_id bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    since timestamp with time zone NOT NULL,
    until timestamp with time zone NOT NULL,
    range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL
);


--
-- Name: TABLE event_instance; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_instance IS '@omit create,update,delete
@simpleCollections both';


--
-- Name: event_instances_for_range(boolean, public.event_type, timestamp with time zone, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_instances_for_range(only_mine boolean, type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $$
  select event_instance.* from event_instance join event on event_id=event.id
  where event.is_visible = true and tstzrange(start_range, end_range, '[]') && range and (type is null or event.type = type)
  and case only_mine
    when false then true
    else exists (select 1 from event_registration where event_id=event.id and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()))) end
  order by range asc;
$$;


--
-- Name: FUNCTION event_instances_for_range(only_mine boolean, type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.event_instances_for_range(only_mine boolean, type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone) IS '@simpleCollections only';


--
-- Name: event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event (
    id bigint NOT NULL,
    name text NOT NULL,
    location_text text NOT NULL,
    description text NOT NULL,
    since date,
    until date,
    capacity bigint DEFAULT '0'::bigint NOT NULL,
    files_legacy text DEFAULT ''::text NOT NULL,
    updated_at timestamp with time zone,
    is_locked boolean DEFAULT false NOT NULL,
    is_visible boolean DEFAULT false NOT NULL,
    summary text DEFAULT ''::text NOT NULL,
    is_public boolean DEFAULT false NOT NULL,
    enable_notes boolean DEFAULT false NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    description_member text DEFAULT ''::text NOT NULL,
    title_image_legacy text,
    type public.event_type DEFAULT 'camp'::public.event_type NOT NULL,
    registration_price public.price DEFAULT NULL::public.price_type
);


--
-- Name: event_registrants(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_registrants(e public.event) RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
  select person.* from person where id in (
    select unnest(array[person_id, man_id, woman_id]) as id
    from event_registration left join couple on couple.id = couple_id
    where event_id=e.id
  )
$$;


--
-- Name: FUNCTION event_registrants(e public.event); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.event_registrants(e public.event) IS '@simpleCollections only';


--
-- Name: event_remaining_lessons(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_remaining_lessons(e public.event) RETURNS integer
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  select (select coalesce(sum(lessons_offered), 0) from event_trainer where event_id = e.id) - (select coalesce(sum(lesson_count), 0) from event_registration join event_lesson_demand on registration_id = event_registration.id where event_id = e.id);
$$;


--
-- Name: event_remaining_person_spots(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_remaining_person_spots(e public.event) RETURNS integer
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  select e.capacity - (select coalesce(sum(case when couple_id is not null then 2 else 1 end), 0) from event_registration where event_id = e.id);
$$;


--
-- Name: event_trainer; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_trainer (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    person_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    lessons_offered integer DEFAULT 0 NOT NULL,
    lesson_price public.price DEFAULT NULL::public.price_type
);


--
-- Name: TABLE event_trainer; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_trainer IS '@omit create,update,delete
@simpleCollections only';


--
-- Name: event_trainer_lessons_remaining(public.event_trainer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_trainer_lessons_remaining(e public.event_trainer) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select e.lessons_offered - (select coalesce(sum(lesson_count), 0) from event_lesson_demand where trainer_id = e.id);
$$;


--
-- Name: filtered_people(bigint[], bigint, boolean, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.filtered_people(in_tenants bigint[], in_cohort bigint, is_trainer boolean, is_admin boolean) RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
 select person.* from person
  where
    exists (select 1 from tenant_membership where tenant_id = any (in_tenants) and person_id=person.id and now() <@ active_range)
  and
    case when in_cohort is null then true
    else exists (select 1 from cohort_membership where cohort_id=in_cohort and person_id=person.id and now() <@ active_range) end
  and
    case when is_trainer is null then true
    else is_trainer = exists (select 1 from tenant_trainer where tenant_id = any (in_tenants) and person_id=person.id) end
  and
    case when is_admin is null then true
    else is_admin = exists (select 1 from tenant_administrator where tenant_id = any (in_tenants) and person_id=person.id) end
  order by last_name, first_name
$$;


--
-- Name: FUNCTION filtered_people(in_tenants bigint[], in_cohort bigint, is_trainer boolean, is_admin boolean); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.filtered_people(in_tenants bigint[], in_cohort bigint, is_trainer boolean, is_admin boolean) IS '@simpleCollections only';


--
-- Name: tenant; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tenant (
    id bigint NOT NULL,
    name text NOT NULL,
    origins text[] DEFAULT ARRAY[]::text[] NOT NULL,
    cz_ico text DEFAULT ''::text NOT NULL,
    cz_dic text DEFAULT ''::text NOT NULL,
    address public.address_domain,
    description text DEFAULT ''::text NOT NULL
);


--
-- Name: TABLE tenant; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant IS '@omit create,delete
@simpleCollections only';


--
-- Name: get_current_tenant(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.get_current_tenant() RETURNS public.tenant
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  SELECT * FROM tenant WHERE id = current_tenant_id();
$$;


--
-- Name: get_current_user(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.get_current_user() RETURNS public.users
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  SELECT * FROM users WHERE u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$;


--
-- Name: invitation_info(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.invitation_info(token uuid) RETURNS text
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  select person.email from person_invitation join person on person.id=person_id where access_token=token and used_at is null;
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

COMMENT ON TABLE public.session IS '@omit';


--
-- Name: login(character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.login(login character varying, passwd character varying, OUT sess public.session, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_salt varchar;
begin
  if login like '%@%' then
    select users.* into usr from users where lower(u_email) = lower(login) limit 1;
  else
    select users.* into usr from users where lower(u_login) = lower(login) limit 1;
  end if;

  if usr is null then
    raise exception 'ACCOUNT_NOT_FOUND' using errcode = '28000';
  end if;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  if usr.u_pass != encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') then
    raise exception 'INVALID_PASSWORD' using errcode = '28P01';
  end if;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', array_to_json(jwt.my_person_ids)::text, true);
  perform set_config('jwt.claims.my_tenant_ids', array_to_json(jwt.my_tenant_ids)::text, true);
  perform set_config('jwt.claims.my_cohort_ids', array_to_json(jwt.my_cohort_ids)::text, true);
  perform set_config('jwt.claims.my_couple_ids', array_to_json(jwt.my_couple_ids)::text, true);
  insert into session (ss_id, ss_user, ss_lifetime) values (gen_random_uuid(), usr.u_id, 86400)
  returning * into sess;
end;
$$;


--
-- Name: my_announcements(boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.my_announcements(archive boolean DEFAULT false) RETURNS SETOF public.upozorneni
    LANGUAGE sql STABLE
    AS $$
  select upozorneni.* from upozorneni
  where is_visible = not archive and sticky = false
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by up_timestamp_add desc;
$$;


--
-- Name: my_cohort_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.my_cohort_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_cohort_ids', true), '')::json)::bigint;
$$;


--
-- Name: FUNCTION my_cohort_ids(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.my_cohort_ids() IS '@omit';


--
-- Name: my_couple_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.my_couple_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_couple_ids', true), '')::json)::bigint;
$$;


--
-- Name: FUNCTION my_couple_ids(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.my_couple_ids() IS '@omit';


--
-- Name: my_person_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.my_person_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_person_ids', true), '')::json)::bigint;
$$;


--
-- Name: FUNCTION my_person_ids(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.my_person_ids() IS '@omit';


--
-- Name: my_tenant_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.my_tenant_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_tenant_ids', true), '')::json)::bigint;
$$;


--
-- Name: FUNCTION my_tenant_ids(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.my_tenant_ids() IS '@omit';


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
-- Name: person_active_couples(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_active_couples(p public.person) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    AS $$
  select couple.* from couple where (man_id = p.id or woman_id = p.id) and now() <@ active_range order by active_range;
$$;


--
-- Name: FUNCTION person_active_couples(p public.person); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.person_active_couples(p public.person) IS '@simpleCollections only';


--
-- Name: person_all_couples(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_all_couples(p public.person) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    AS $$
  select couple.* from couple where (man_id = p.id or woman_id = p.id) order by active_range;
$$;


--
-- Name: FUNCTION person_all_couples(p public.person); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.person_all_couples(p public.person) IS '@simpleCollections only';


--
-- Name: person_cohort_ids(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_cohort_ids(p public.person) RETURNS bigint[]
    LANGUAGE sql STABLE
    AS $$
  select array_agg(cohort_id) from cohort_membership where now() <@ active_range and person_id = p.id;
$$;


--
-- Name: person_couple_ids(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_couple_ids(p public.person) RETURNS bigint[]
    LANGUAGE sql STABLE
    AS $$
  select array_agg(couple.id)
  from couple
  where man_id = p.id or woman_id = p.id and now() <@ active_range;
$$;


--
-- Name: person_has_user(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_has_user(p public.person) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select exists (select 1 from user_proxy where person_id = p.id);
$$;


--
-- Name: person_is_admin(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_is_admin(p public.person) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select exists (select 1 from tenant_administrator where now() <@ active_range and person_id = p.id);
$$;


--
-- Name: person_is_trainer(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_is_trainer(p public.person) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select exists (select 1 from tenant_trainer where now() <@ active_range and person_id = p.id);
$$;


--
-- Name: person_name(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_name(p public.person) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select concat_ws(' ', p.prefix_title, p.first_name, p.last_name) || (case p.suffix_title when '' then '' else ', ' || p.suffix_title end);
$$;


--
-- Name: person_address; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.person_address (
    person_id bigint NOT NULL,
    is_primary boolean DEFAULT false,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    address public.address_domain
);


--
-- Name: TABLE person_address; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.person_address IS '@omit create,update,delete
@simpleCollections only';


--
-- Name: person_primary_address(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_primary_address(p public.person) RETURNS public.person_address
    LANGUAGE sql STABLE
    AS $$
  select person_address.* from person_address where person_id = p.id and is_primary = true;
$$;


--
-- Name: person_tenant_ids(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_tenant_ids(p public.person) RETURNS bigint[]
    LANGUAGE sql STABLE
    AS $$
  select array_agg(tenant_id) from tenant_membership where now() <@ active_range and person_id = p.id;
$$;


--
-- Name: event_lesson_demand; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_lesson_demand (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    trainer_id bigint NOT NULL,
    registration_id bigint NOT NULL,
    lesson_count integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_lesson_demand_lesson_count_check CHECK ((lesson_count > 0))
);


--
-- Name: TABLE event_lesson_demand; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_lesson_demand IS '@omit create,update,delete
@simpleCollections only';


--
-- Name: register_to_event(public.event_registration, public.event_lesson_demand[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.register_to_event(INOUT registration public.event_registration, lessons public.event_lesson_demand[]) RETURNS public.event_registration
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  event event;
  demand event_lesson_demand;
begin
  select * into event from event where id = registration.event_id;

  if event is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if registration.person_id not in (select my_person_ids()) and registration.couple_id not in (select my_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  insert into event_registration (event_id, person_id, couple_id, note) select registration.event_id, registration.person_id, registration.couple_id, registration.note returning * into registration;
  foreach demand in array lessons loop
    perform set_lesson_demand(registration.id, demand.trainer_id, demand.lesson_count);
  end loop;
end;
$$;


--
-- Name: FUNCTION register_to_event(INOUT registration public.event_registration, lessons public.event_lesson_demand[]); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.register_to_event(INOUT registration public.event_registration, lessons public.event_lesson_demand[]) IS '@arg0variant create
@arg1variant patch';


--
-- Name: register_using_invitation(text, text, text, uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.register_using_invitation(email text, login text, passwd text, token uuid, OUT usr public.users, OUT sess public.session, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  invitation person_invitation;
  v_salt text;
begin
  select * into invitation from person_invitation where access_token=token;

  if invitation is null then
    raise exception 'INVITATION_NOT_FOUND' using errcode = '28000';
  end if;
  if invitation.used_at is not null then
    raise exception 'INVITATION_ALREADY_USED' using errcode = '28P01';
  end if;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  insert into users (u_login, u_email, u_pass) values (login, email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  insert into user_proxy (user_id, person_id) values (usr.id, invitation.person_id);
  update person_invitation set used_at=now() where access_token=token;
  insert into session (ss_id, ss_user, ss_lifetime) values (gen_random_uuid(), usr.u_id, 86400)
  returning * into sess;
  jwt := app_private.create_jwt_token(usr);
end
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
-- Name: set_lesson_demand(bigint, bigint, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer) RETURNS public.event_lesson_demand
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
#variable_conflict use_variable
declare
  event event;
  registration event_registration;
  current_lessons int;
  lesson_demand event_lesson_demand;
begin
  select * into registration from event_registration where id = registration_id;
  select * into event from event where id = registration.event_id;
  select sum(lesson_count)::int into current_lessons from event_lesson_demand eld where eld.registration_id = registration_id;

  if lesson_count = 0 then
    delete from event_lesson_demand eld where eld.registration_id = registration.id and eld.trainer_id = trainer_id;
    return null;
  end if;

  if lesson_count > (current_lessons + event_remaining_lessons(event)) then
    select (current_lessons + event_remaining_lessons(event)) into lesson_count;
  end if;

  INSERT INTO event_lesson_demand (registration_id, trainer_id, lesson_count)
  values (registration.id, trainer_id, lesson_count)
  on conflict on constraint eld_unique_registration_trainer_key do update set lesson_count = lesson_count
  returning * into lesson_demand;

  return lesson_demand;
end;
$$;


--
-- Name: sticky_announcements(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.sticky_announcements() RETURNS SETOF public.upozorneni
    LANGUAGE sql STABLE
    AS $$
  select upozorneni.* from upozorneni
  where is_visible = true and sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by up_timestamp_add desc;
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
-- Name: tenant_administrator; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tenant_administrator (
    tenant_id bigint NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    is_visible boolean DEFAULT true NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL
);


--
-- Name: TABLE tenant_administrator; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant_administrator IS '@omit delete
@simpleCollections only';


--
-- Name: COLUMN tenant_administrator.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.tenant_administrator.active_range IS '@omit';


--
-- Name: tenant_administrator_active(public.tenant_administrator); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.tenant_administrator_active(c public.tenant_administrator) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select now() <@ c.active_range;
$$;


--
-- Name: FUNCTION tenant_administrator_active(c public.tenant_administrator); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.tenant_administrator_active(c public.tenant_administrator) IS '@filterable';


--
-- Name: tenant_membership; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tenant_membership (
    tenant_id bigint NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL
);


--
-- Name: TABLE tenant_membership; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant_membership IS '@omit delete
@simpleCollections only';


--
-- Name: COLUMN tenant_membership.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.tenant_membership.active_range IS '@omit';


--
-- Name: tenant_membership_active(public.tenant_membership); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.tenant_membership_active(c public.tenant_membership) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select now() <@ c.active_range;
$$;


--
-- Name: FUNCTION tenant_membership_active(c public.tenant_membership); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.tenant_membership_active(c public.tenant_membership) IS '@filterable';


--
-- Name: tenant_trainer; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tenant_trainer (
    tenant_id bigint NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    is_visible boolean DEFAULT true,
    description text DEFAULT ''::text NOT NULL,
    default_price public.price DEFAULT NULL::public.price_type,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL
);


--
-- Name: TABLE tenant_trainer; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant_trainer IS '@omit delete
@simpleCollections only';


--
-- Name: COLUMN tenant_trainer.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.tenant_trainer.active_range IS '@omit';


--
-- Name: tenant_trainer_active(public.tenant_trainer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.tenant_trainer_active(c public.tenant_trainer) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select now() <@ c.active_range;
$$;


--
-- Name: FUNCTION tenant_trainer_active(c public.tenant_trainer); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.tenant_trainer_active(c public.tenant_trainer) IS '@filterable';


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
-- Name: FUNCTION verify_function(f regproc, relid regclass); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.verify_function(f regproc, relid regclass) IS '@omit';


--
-- Name: array_accum(anycompatiblearray); Type: AGGREGATE; Schema: public; Owner: -
--

CREATE AGGREGATE public.array_accum(anycompatiblearray) (
    SFUNC = array_cat,
    STYPE = anycompatiblearray,
    INITCOND = '{}'
);


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
-- Name: parameters; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.parameters (
    pa_name character varying(40) NOT NULL,
    pa_value text NOT NULL
);


--
-- Name: TABLE parameters; Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON TABLE app_private.parameters IS '@omit create,update,delete';


--
-- Name: pary_navrh; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.pary_navrh (
    pn_id bigint NOT NULL,
    pn_navrhl bigint NOT NULL,
    pn_partner bigint NOT NULL,
    pn_partnerka bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pn_id) STORED
);


--
-- Name: TABLE pary_navrh; Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON TABLE app_private.pary_navrh IS '@omit create,update,delete';


--
-- Name: pary_navrh_pn_id_seq; Type: SEQUENCE; Schema: app_private; Owner: -
--

CREATE SEQUENCE app_private.pary_navrh_pn_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: pary_navrh_pn_id_seq; Type: SEQUENCE OWNED BY; Schema: app_private; Owner: -
--

ALTER SEQUENCE app_private.pary_navrh_pn_id_seq OWNED BY app_private.pary_navrh.pn_id;


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
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    title_photo_url text
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
-- Name: cohort_group; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.cohort_group (
    id bigint NOT NULL,
    name text NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    ordering integer DEFAULT 1 NOT NULL,
    is_public boolean DEFAULT true NOT NULL,
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
-- Name: cohort_membership_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.cohort_membership ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.cohort_membership_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: couple_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.couple ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.couple_id_seq
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
-- Name: TABLE dokumenty; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.dokumenty IS '@simpleCollections only';


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
-- Name: event_attendance_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.event_attendance ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.event_attendance_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: event_instance_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.event_instance ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.event_instance_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: event_instance_trainer; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_instance_trainer (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    instance_id bigint NOT NULL,
    person_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE event_instance_trainer; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_instance_trainer IS '@omit create,update,delete
@simpleCollections only';


--
-- Name: event_instance_trainer_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.event_instance_trainer ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.event_instance_trainer_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: event_lesson_demand_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.event_lesson_demand ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.event_lesson_demand_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: event_registration_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.event_registration ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.event_registration_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: event_target_cohort; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_target_cohort (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    cohort_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE event_target_cohort; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_target_cohort IS '@omit create,update,delete
@simpleCollections only';


--
-- Name: event_target_cohort_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.event_target_cohort ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.event_target_cohort_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: event_trainer_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.event_trainer ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.event_trainer_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


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
-- Name: TABLE form_responses; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.form_responses IS '@omit update,delete';


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
-- Name: TABLE galerie_dir; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.galerie_dir IS '@omit create,update,delete';


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
-- Name: TABLE galerie_foto; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.galerie_foto IS '@omit create,update,delete';


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
    description jsonb NOT NULL,
    address public.address_domain
);


--
-- Name: location_attachment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.location_attachment (
    location_id bigint NOT NULL,
    object_name text NOT NULL
);


--
-- Name: TABLE location_attachment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.location_attachment IS '@omit create,update,delete';


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
-- Name: otp_token; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.otp_token (
    id bigint NOT NULL,
    access_token uuid DEFAULT gen_random_uuid() NOT NULL,
    user_id bigint,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    expires_at timestamp with time zone DEFAULT (now() + '24:00:00'::interval) NOT NULL,
    used_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE otp_token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.otp_token IS '@omit';


--
-- Name: otp_token_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.otp_token ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.otp_token_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: person_email; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.person_email (
    person_id bigint NOT NULL,
    email text NOT NULL,
    is_primary boolean DEFAULT false,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE person_email; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.person_email IS '@omit';


--
-- Name: person_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.person ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.person_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: person_invitation; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.person_invitation (
    id bigint NOT NULL,
    access_token uuid DEFAULT gen_random_uuid() NOT NULL,
    person_id bigint,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    used_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE person_invitation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.person_invitation IS '@omit';


--
-- Name: person_invitation_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.person_invitation ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.person_invitation_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: person_phone; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.person_phone (
    person_id bigint NOT NULL,
    phone text NOT NULL,
    is_primary boolean DEFAULT false,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE person_phone; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.person_phone IS '@omit';


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
-- Name: TABLE platby_category; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.platby_category IS '@omit create,update,delete';


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
-- Name: TABLE platby_category_group; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.platby_category_group IS '@omit';


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
-- Name: TABLE platby_group; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.platby_group IS '@omit';


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
-- Name: TABLE platby_group_skupina; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.platby_group_skupina IS '@omit';


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
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    status public.payment_status DEFAULT 'paid'::public.payment_status NOT NULL
);


--
-- Name: TABLE platby_item; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.platby_item IS '@omit create,update,delete';


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
-- Name: TABLE platby_raw; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.platby_raw IS '@omit';


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
-- Name: TABLE room_attachment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.room_attachment IS '@omit create,update,delete';


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
-- Name: scoreboard; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.scoreboard AS
 WITH members AS (
         SELECT person.id
           FROM (public.person
             JOIN public.cohort_membership ON ((cohort_membership.person_id = person.id)))
          WHERE (now() <@ cohort_membership.active_range)
        ), attendances AS (
         SELECT event_attendance.person_id,
                CASE
                    WHEN (event_registration.target_cohort_id IS NULL) THEN 3
                    ELSE 0
                END AS lesson_score,
                CASE
                    WHEN (event_registration.target_cohort_id IS NULL) THEN 0
                    ELSE 2
                END AS group_score,
            event_instance.since
           FROM (((public.event_attendance
             JOIN public.event_registration ON ((event_registration.id = event_attendance.registration_id)))
             JOIN public.event ON ((event.id = event_registration.event_id)))
             JOIN public.event_instance ON ((event_attendance.instance_id = event_instance.id)))
          WHERE ((event_attendance.status = 'attended'::public.attendance_type) AND (event.type = 'lesson'::public.event_type) AND (event_instance.since > '2022-01-01 00:00:00+00'::timestamp with time zone) AND (event_attendance.person_id IN ( SELECT members.id
                   FROM members)))
        )
 SELECT attendances.person_id,
    sum(attendances.lesson_score) AS lesson_total_score,
    sum(attendances.group_score) AS group_total_score,
    sum((attendances.lesson_score + attendances.group_score)) AS total_score,
    rank() OVER (ORDER BY (sum((attendances.lesson_score + attendances.group_score))) DESC) AS ranking
   FROM attendances
  GROUP BY attendances.person_id
  ORDER BY (sum((attendances.lesson_score + attendances.group_score))) DESC;


--
-- Name: VIEW scoreboard; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.scoreboard IS '@foreignKey (person_id) references person (id)
@simpleCollections only';


--
-- Name: skupiny; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.skupiny (
    s_id bigint NOT NULL,
    s_name text NOT NULL,
    s_description text NOT NULL,
    s_color_rgb text NOT NULL,
    s_location text DEFAULT ''::text NOT NULL,
    s_visible boolean DEFAULT true NOT NULL,
    ordering integer DEFAULT 1 NOT NULL,
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
-- Name: tenant_administrator_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.tenant_administrator ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.tenant_administrator_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: tenant_attachment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tenant_attachment (
    tenant_id bigint NOT NULL,
    object_name text NOT NULL,
    type public.tenant_attachment_type
);


--
-- Name: TABLE tenant_attachment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant_attachment IS '@omit create,update,delete';


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
    location_id bigint NOT NULL,
    id bigint NOT NULL
);


--
-- Name: TABLE tenant_location; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant_location IS '@omit create,update,delete
@simpleCollections only';


--
-- Name: tenant_location_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.tenant_location ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.tenant_location_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: tenant_membership_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.tenant_membership ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.tenant_membership_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: tenant_trainer_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.tenant_trainer ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.tenant_trainer_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: upozorneni_skupiny; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.upozorneni_skupiny (
    ups_id bigint NOT NULL,
    ups_id_rodic bigint NOT NULL,
    ups_id_skupina bigint NOT NULL,
    ups_color text NOT NULL,
    id bigint GENERATED ALWAYS AS (ups_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: TABLE upozorneni_skupiny; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.upozorneni_skupiny IS '@omit create,update,delete';


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
-- Name: user_proxy; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_proxy (
    user_id bigint NOT NULL,
    person_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL
);


--
-- Name: TABLE user_proxy; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_proxy IS '@omit create,update,delete
@simpleCollections only';


--
-- Name: user_proxy_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.user_proxy ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.user_proxy_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


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
-- Name: pary_navrh pn_id; Type: DEFAULT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.pary_navrh ALTER COLUMN pn_id SET DEFAULT nextval('app_private.pary_navrh_pn_id_seq'::regclass);


--
-- Name: aktuality at_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality ALTER COLUMN at_id SET DEFAULT nextval('public.aktuality_at_id_seq'::regclass);


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
-- Name: parameters idx_23816_primary; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.parameters
    ADD CONSTRAINT idx_23816_primary PRIMARY KEY (pa_name);


--
-- Name: pary_navrh idx_23840_primary; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.pary_navrh
    ADD CONSTRAINT idx_23840_primary PRIMARY KEY (pn_id);


--
-- Name: attachment attachment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT attachment_pkey PRIMARY KEY (object_name);


--
-- Name: cohort_group cohort_group_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_group
    ADD CONSTRAINT cohort_group_pkey PRIMARY KEY (id);


--
-- Name: cohort_membership cohort_membership_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_pkey PRIMARY KEY (id);


--
-- Name: couple couple_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_pkey PRIMARY KEY (id);


--
-- Name: event_lesson_demand eld_unique_registration_trainer_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_lesson_demand
    ADD CONSTRAINT eld_unique_registration_trainer_key UNIQUE (registration_id, trainer_id);


--
-- Name: event_attendance event_attendance_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_attendance
    ADD CONSTRAINT event_attendance_pkey PRIMARY KEY (id);


--
-- Name: event_attendance event_attendance_unique_event_person_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_attendance
    ADD CONSTRAINT event_attendance_unique_event_person_key UNIQUE (registration_id, instance_id, person_id);


--
-- Name: event_instance event_instance_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_pkey PRIMARY KEY (id);


--
-- Name: event_instance_trainer event_instance_trainer_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_instance_trainer
    ADD CONSTRAINT event_instance_trainer_pkey PRIMARY KEY (id);


--
-- Name: event_lesson_demand event_lesson_demand_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_lesson_demand
    ADD CONSTRAINT event_lesson_demand_pkey PRIMARY KEY (id);


--
-- Name: event_registration event_registration_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_pkey PRIMARY KEY (id);


--
-- Name: event_registration event_registration_unique_event_person_couple_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_unique_event_person_couple_key UNIQUE NULLS NOT DISTINCT (event_id, person_id, couple_id);


--
-- Name: event_target_cohort event_target_cohort_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_pkey PRIMARY KEY (id);


--
-- Name: event_trainer event_trainer_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_pkey PRIMARY KEY (id);


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
-- Name: otp_token otp_token_access_token_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.otp_token
    ADD CONSTRAINT otp_token_access_token_key UNIQUE (access_token);


--
-- Name: otp_token otp_token_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.otp_token
    ADD CONSTRAINT otp_token_pkey PRIMARY KEY (id);


--
-- Name: person_email person_email_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person_email
    ADD CONSTRAINT person_email_pkey PRIMARY KEY (person_id, email);


--
-- Name: person_invitation person_invitation_access_token_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person_invitation
    ADD CONSTRAINT person_invitation_access_token_key UNIQUE (access_token);


--
-- Name: person_invitation person_invitation_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person_invitation
    ADD CONSTRAINT person_invitation_pkey PRIMARY KEY (id);


--
-- Name: person_phone person_phone_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person_phone
    ADD CONSTRAINT person_phone_pkey PRIMARY KEY (person_id, phone);


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
-- Name: tenant_administrator tenant_administrator_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_administrator
    ADD CONSTRAINT tenant_administrator_pkey PRIMARY KEY (id);


--
-- Name: tenant_attachment tenant_attachment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_attachment
    ADD CONSTRAINT tenant_attachment_pkey PRIMARY KEY (tenant_id, object_name);


--
-- Name: tenant_location tenant_location_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_location
    ADD CONSTRAINT tenant_location_pkey PRIMARY KEY (id);


--
-- Name: tenant_membership tenant_membership_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_membership
    ADD CONSTRAINT tenant_membership_pkey PRIMARY KEY (id);


--
-- Name: tenant tenant_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant
    ADD CONSTRAINT tenant_pkey PRIMARY KEY (id);


--
-- Name: tenant_trainer tenant_trainer_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_trainer
    ADD CONSTRAINT tenant_trainer_pkey PRIMARY KEY (id);


--
-- Name: user_proxy user_proxy_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_proxy
    ADD CONSTRAINT user_proxy_pkey PRIMARY KEY (id);


--
-- Name: idx_23840_pary_navrh_pn_navrhl_fkey; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX idx_23840_pary_navrh_pn_navrhl_fkey ON app_private.pary_navrh USING btree (pn_navrhl);


--
-- Name: idx_23840_pary_navrh_pn_partner_fkey; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX idx_23840_pary_navrh_pn_partner_fkey ON app_private.pary_navrh USING btree (pn_partner);


--
-- Name: idx_23840_pary_navrh_pn_partnerka_fkey; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX idx_23840_pary_navrh_pn_partnerka_fkey ON app_private.pary_navrh USING btree (pn_partnerka);


--
-- Name: cohort_membership_cohort_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_membership_cohort_id_idx ON public.cohort_membership USING btree (cohort_id);


--
-- Name: cohort_membership_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_membership_person_id_idx ON public.cohort_membership USING btree (person_id);


--
-- Name: cohort_membership_range_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_membership_range_idx ON public.cohort_membership USING gist (active_range, tenant_id, person_id);


--
-- Name: couple_man_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX couple_man_id_idx ON public.couple USING btree (man_id);


--
-- Name: couple_range_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX couple_range_idx ON public.couple USING gist (active_range, man_id, woman_id);


--
-- Name: couple_woman_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX couple_woman_id_idx ON public.couple USING btree (woman_id);


--
-- Name: d_kategorie; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX d_kategorie ON public.dokumenty USING btree (d_kategorie);


--
-- Name: d_timestamp; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX d_timestamp ON public.dokumenty USING btree (d_timestamp);


--
-- Name: event_attendance_instance_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_attendance_instance_id_idx ON public.event_attendance USING btree (instance_id);


--
-- Name: event_attendance_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_attendance_person_id_idx ON public.event_attendance USING btree (person_id);


--
-- Name: event_attendance_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_attendance_tenant_id_idx ON public.event_attendance USING btree (tenant_id);


--
-- Name: event_instance_event_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_instance_event_id_idx ON public.event_instance USING btree (event_id);


--
-- Name: event_instance_location_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_instance_location_id_idx ON public.event_instance USING btree (location_id);


--
-- Name: event_instance_range_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_instance_range_idx ON public.event_instance USING gist (range);


--
-- Name: event_instance_since_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_instance_since_idx ON public.event_instance USING btree (since);


--
-- Name: event_instance_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_instance_tenant_id_idx ON public.event_instance USING btree (tenant_id);


--
-- Name: event_instance_trainer_instance_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_instance_trainer_instance_id_idx ON public.event_instance_trainer USING btree (instance_id);


--
-- Name: event_instance_trainer_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_instance_trainer_person_id_idx ON public.event_instance_trainer USING btree (person_id);


--
-- Name: event_instance_trainer_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_instance_trainer_tenant_id_idx ON public.event_instance_trainer USING btree (tenant_id);


--
-- Name: event_instance_until_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_instance_until_idx ON public.event_instance USING btree (until);


--
-- Name: event_lesson_demand_registration_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_lesson_demand_registration_id_idx ON public.event_lesson_demand USING btree (registration_id);


--
-- Name: event_lesson_demand_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_lesson_demand_tenant_id_idx ON public.event_lesson_demand USING btree (tenant_id);


--
-- Name: event_lesson_demand_trainer_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_lesson_demand_trainer_id_idx ON public.event_lesson_demand USING btree (trainer_id);


--
-- Name: event_registration_couple_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_couple_id_idx ON public.event_registration USING btree (couple_id);


--
-- Name: event_registration_event_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_event_id_idx ON public.event_registration USING btree (event_id);


--
-- Name: event_registration_payment_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_payment_id_idx ON public.event_registration USING btree (payment_id);


--
-- Name: event_registration_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_person_id_idx ON public.event_registration USING btree (person_id);


--
-- Name: event_registration_target_cohort_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_target_cohort_id_idx ON public.event_registration USING btree (target_cohort_id);


--
-- Name: event_registration_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_tenant_id_idx ON public.event_registration USING btree (tenant_id);


--
-- Name: event_target_cohort_cohort_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_target_cohort_cohort_id_idx ON public.event_target_cohort USING btree (cohort_id);


--
-- Name: event_target_cohort_event_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_target_cohort_event_id_idx ON public.event_target_cohort USING btree (event_id);


--
-- Name: event_target_cohort_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_target_cohort_tenant_id_idx ON public.event_target_cohort USING btree (tenant_id);


--
-- Name: event_trainer_event_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_trainer_event_id_idx ON public.event_trainer USING btree (event_id);


--
-- Name: event_trainer_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_trainer_person_id_idx ON public.event_trainer USING btree (person_id);


--
-- Name: event_trainer_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_trainer_tenant_id_idx ON public.event_trainer USING btree (tenant_id);


--
-- Name: event_type_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_type_idx ON public.event USING btree (type);


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
-- Name: idx_cg_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_cg_tenant ON public.cohort_group USING btree (tenant_id);


--
-- Name: idx_cm_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_cm_tenant ON public.cohort_membership USING btree (tenant_id);


--
-- Name: idx_d_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_d_tenant ON public.dokumenty USING btree (tenant_id);


--
-- Name: idx_e_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_e_tenant ON public.event USING btree (tenant_id);


--
-- Name: idx_fr_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_fr_tenant ON public.form_responses USING btree (tenant_id);


--
-- Name: idx_gd_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_gd_tenant ON public.galerie_dir USING btree (tenant_id);


--
-- Name: idx_gf_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_gf_tenant ON public.galerie_foto USING btree (tenant_id);


--
-- Name: idx_ot_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ot_tenant ON public.otp_token USING btree (tenant_id);


--
-- Name: idx_pc_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_pc_tenant ON public.platby_category USING btree (tenant_id);


--
-- Name: idx_pcg_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_pcg_tenant ON public.platby_category_group USING btree (tenant_id);


--
-- Name: idx_pei_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_pei_tenant ON public.person_invitation USING btree (tenant_id);


--
-- Name: idx_pg_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_pg_tenant ON public.platby_group USING btree (tenant_id);


--
-- Name: idx_pgs_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_pgs_tenant ON public.platby_group_skupina USING btree (tenant_id);


--
-- Name: idx_pi_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_pi_tenant ON public.platby_item USING btree (tenant_id);


--
-- Name: idx_pr_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_pr_tenant ON public.platby_raw USING btree (tenant_id);


--
-- Name: idx_sk_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_sk_tenant ON public.skupiny USING btree (tenant_id);


--
-- Name: idx_up_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_up_tenant ON public.upozorneni USING btree (tenant_id);


--
-- Name: idx_ups_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ups_tenant ON public.upozorneni_skupiny USING btree (tenant_id);


--
-- Name: idx_us_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_us_tenant ON public.users USING btree (tenant_id);


--
-- Name: is_public; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX is_public ON public.cohort_group USING btree (is_public);


--
-- Name: is_visible; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX is_visible ON public.event USING btree (is_visible);


--
-- Name: object_name; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX object_name ON public.location_attachment USING btree (object_name);


--
-- Name: ordering; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ordering ON public.cohort_group USING btree (ordering);


--
-- Name: otp_token_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX otp_token_user_id_idx ON public.otp_token USING btree (user_id);


--
-- Name: person_address_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX person_address_person_id_idx ON public.person_address USING btree (person_id);


--
-- Name: person_email_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX person_email_person_id_idx ON public.person_email USING btree (person_id);


--
-- Name: person_invitation_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX person_invitation_person_id_idx ON public.person_invitation USING btree (person_id);


--
-- Name: person_phone_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX person_phone_person_id_idx ON public.person_phone USING btree (person_id);


--
-- Name: room_attachment_object_name_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX room_attachment_object_name_idx ON public.room_attachment USING btree (object_name);


--
-- Name: room_location_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX room_location_idx ON public.room USING btree (location);


--
-- Name: s_visible; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX s_visible ON public.skupiny USING btree (s_visible);


--
-- Name: since; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX since ON public.event USING btree (since);


--
-- Name: skupiny_cohort_group_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX skupiny_cohort_group_idx ON public.skupiny USING btree (cohort_group);


--
-- Name: skupiny_ordering_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX skupiny_ordering_idx ON public.skupiny USING btree (ordering);


--
-- Name: ss_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX ss_user ON public.session USING btree (ss_user);


--
-- Name: tenant_administrator_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_administrator_person_id_idx ON public.tenant_administrator USING btree (person_id);


--
-- Name: tenant_administrator_range_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_administrator_range_idx ON public.tenant_administrator USING gist (active_range, tenant_id, person_id);


--
-- Name: tenant_administrator_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_administrator_tenant_id_idx ON public.tenant_administrator USING btree (tenant_id);


--
-- Name: tenant_attachment_object_name_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_attachment_object_name_idx ON public.tenant_attachment USING btree (object_name);


--
-- Name: tenant_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_id ON public.aktuality USING btree (tenant_id);


--
-- Name: tenant_location_location_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_location_location_id_idx ON public.tenant_location USING btree (location_id);


--
-- Name: tenant_location_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_location_tenant_id_idx ON public.tenant_location USING btree (tenant_id);


--
-- Name: tenant_membership_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_membership_person_id_idx ON public.tenant_membership USING btree (person_id);


--
-- Name: tenant_membership_range_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_membership_range_idx ON public.tenant_membership USING gist (active_range, tenant_id, person_id);


--
-- Name: tenant_membership_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_membership_tenant_id_idx ON public.tenant_membership USING btree (tenant_id);


--
-- Name: tenant_trainer_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_trainer_person_id_idx ON public.tenant_trainer USING btree (person_id);


--
-- Name: tenant_trainer_range_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_trainer_range_idx ON public.tenant_trainer USING gist (active_range, tenant_id, person_id);


--
-- Name: tenant_trainer_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_trainer_tenant_id_idx ON public.tenant_trainer USING btree (tenant_id);


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
-- Name: user_proxy_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_proxy_person_id_idx ON public.user_proxy USING btree (person_id);


--
-- Name: user_proxy_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_proxy_user_id_idx ON public.user_proxy USING btree (user_id);


--
-- Name: crm_activity _100_timestamps; Type: TRIGGER; Schema: app_private; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON app_private.crm_activity FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: crm_prospect _100_timestamps; Type: TRIGGER; Schema: app_private; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON app_private.crm_prospect FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: cohort_membership _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.cohort_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: couple _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.couple FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: event_attendance _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_attendance FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: event_instance _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_instance FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: event_instance_trainer _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_instance_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: event_lesson_demand _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_lesson_demand FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: event_registration _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: event_target_cohort _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_target_cohort FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: event_trainer _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: form_responses _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.form_responses FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: person _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: person_address _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person_address FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: person_email _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person_email FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: person_phone _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person_phone FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: tenant_administrator _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.tenant_administrator FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: tenant_membership _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.tenant_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: tenant_trainer _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.tenant_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: user_proxy _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.user_proxy FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: users _200_encrypt_password; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_encrypt_password BEFORE INSERT OR UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg_users__encrypt_password();


--
-- Name: person_address _200_primary; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_primary BEFORE INSERT OR UPDATE ON public.person_address FOR EACH ROW EXECUTE FUNCTION app_private.tg__person_address_primary();


--
-- Name: person_email _200_primary; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_primary BEFORE INSERT OR UPDATE ON public.person_email FOR EACH ROW EXECUTE FUNCTION app_private.tg__person_email_primary();


--
-- Name: person_phone _200_primary; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_primary BEFORE INSERT OR UPDATE ON public.person_phone FOR EACH ROW EXECUTE FUNCTION app_private.tg__person_phone_primary();


--
-- Name: event_instance _500_create_attendance; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_create_attendance AFTER INSERT ON public.event_instance FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_instance__create_attendance();


--
-- Name: event_registration _500_create_attendance; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_create_attendance AFTER INSERT ON public.event_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_registration__create_attendance();


--
-- Name: users _500_notify_admin; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_notify_admin AFTER INSERT ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg_users__notify_admin();


--
-- Name: event_target_cohort _500_register_members; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_register_members AFTER INSERT ON public.event_target_cohort FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_target_cohort__register_members();


--
-- Name: person_invitation _500_send; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_send AFTER INSERT ON public.person_invitation FOR EACH ROW EXECUTE FUNCTION app_private.tg_person_invitation__send();


--
-- Name: event_target_cohort _500_unregister_members; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_unregister_members AFTER DELETE ON public.event_target_cohort FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_target_cohort__unregister_members();


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
-- Name: pary_navrh pary_navrh_pn_navrhl_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_navrhl_fkey FOREIGN KEY (pn_navrhl) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: pary_navrh pary_navrh_pn_partner_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_partner_fkey FOREIGN KEY (pn_partner) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: pary_navrh pary_navrh_pn_partnerka_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.pary_navrh
    ADD CONSTRAINT pary_navrh_pn_partnerka_fkey FOREIGN KEY (pn_partnerka) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


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
-- Name: aktuality aktuality_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: attachment attachment_uploaded_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT attachment_uploaded_by_fkey FOREIGN KEY (uploaded_by) REFERENCES public.users(u_id);


--
-- Name: cohort_group cohort_group_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_group
    ADD CONSTRAINT cohort_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: cohort_membership cohort_membership_cohort_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.skupiny(s_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: cohort_membership cohort_membership_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: cohort_membership cohort_membership_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: couple couple_man_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_man_id_fkey FOREIGN KEY (man_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: couple couple_woman_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_woman_id_fkey FOREIGN KEY (woman_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: dokumenty dokumenty_d_kdo_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dokumenty
    ADD CONSTRAINT dokumenty_d_kdo_fkey FOREIGN KEY (d_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: dokumenty dokumenty_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dokumenty
    ADD CONSTRAINT dokumenty_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: event_attendance event_attendance_instance_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_attendance
    ADD CONSTRAINT event_attendance_instance_id_fkey FOREIGN KEY (instance_id) REFERENCES public.event_instance(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_attendance event_attendance_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_attendance
    ADD CONSTRAINT event_attendance_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_attendance event_attendance_registration_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_attendance
    ADD CONSTRAINT event_attendance_registration_id_fkey FOREIGN KEY (registration_id) REFERENCES public.event_registration(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_attendance event_attendance_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_attendance
    ADD CONSTRAINT event_attendance_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_instance event_instance_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_instance event_instance_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.location(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: event_instance event_instance_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_instance_trainer event_instance_trainer_instance_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_instance_trainer
    ADD CONSTRAINT event_instance_trainer_instance_id_fkey FOREIGN KEY (instance_id) REFERENCES public.event_instance(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_instance_trainer event_instance_trainer_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_instance_trainer
    ADD CONSTRAINT event_instance_trainer_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_instance_trainer event_instance_trainer_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_instance_trainer
    ADD CONSTRAINT event_instance_trainer_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_lesson_demand event_lesson_demand_registration_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_lesson_demand
    ADD CONSTRAINT event_lesson_demand_registration_id_fkey FOREIGN KEY (registration_id) REFERENCES public.event_registration(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_lesson_demand event_lesson_demand_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_lesson_demand
    ADD CONSTRAINT event_lesson_demand_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_lesson_demand event_lesson_demand_trainer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_lesson_demand
    ADD CONSTRAINT event_lesson_demand_trainer_id_fkey FOREIGN KEY (trainer_id) REFERENCES public.event_trainer(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_registration event_registration_couple_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_couple_id_fkey FOREIGN KEY (couple_id) REFERENCES public.couple(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_registration event_registration_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_registration event_registration_payment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES public.platby_item(pi_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_registration event_registration_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_registration event_registration_target_cohort_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_target_cohort_id_fkey FOREIGN KEY (target_cohort_id) REFERENCES public.event_target_cohort(id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: event_registration event_registration_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_target_cohort event_target_cohort_cohort_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.skupiny(s_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_target_cohort event_target_cohort_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_target_cohort event_target_cohort_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event event_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: event_trainer event_trainer_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_trainer event_trainer_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_trainer event_trainer_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: form_responses form_responses_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.form_responses
    ADD CONSTRAINT form_responses_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: galerie_dir galerie_dir_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.galerie_dir
    ADD CONSTRAINT galerie_dir_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


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
-- Name: galerie_foto galerie_foto_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT galerie_foto_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


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
-- Name: otp_token otp_token_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.otp_token
    ADD CONSTRAINT otp_token_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: otp_token otp_token_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.otp_token
    ADD CONSTRAINT otp_token_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(u_id);


--
-- Name: person_address person_address_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person_address
    ADD CONSTRAINT person_address_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: person_email person_email_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person_email
    ADD CONSTRAINT person_email_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id);


--
-- Name: person_invitation person_invitation_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person_invitation
    ADD CONSTRAINT person_invitation_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id);


--
-- Name: person_invitation person_invitation_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person_invitation
    ADD CONSTRAINT person_invitation_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: person_phone person_phone_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person_phone
    ADD CONSTRAINT person_phone_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


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
-- Name: platby_category_group platby_category_group_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_category_group
    ADD CONSTRAINT platby_category_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: platby_category platby_category_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_category
    ADD CONSTRAINT platby_category_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


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
-- Name: platby_group_skupina platby_group_skupina_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: platby_group platby_group_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group
    ADD CONSTRAINT platby_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


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
-- Name: platby_item platby_item_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_item
    ADD CONSTRAINT platby_item_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: platby_raw platby_raw_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_raw
    ADD CONSTRAINT platby_raw_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


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
-- Name: skupiny skupiny_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.skupiny
    ADD CONSTRAINT skupiny_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: tenant_administrator tenant_administrator_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_administrator
    ADD CONSTRAINT tenant_administrator_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: tenant_administrator tenant_administrator_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_administrator
    ADD CONSTRAINT tenant_administrator_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


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
-- Name: tenant_membership tenant_membership_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_membership
    ADD CONSTRAINT tenant_membership_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: tenant_membership tenant_membership_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_membership
    ADD CONSTRAINT tenant_membership_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: tenant_trainer tenant_trainer_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_trainer
    ADD CONSTRAINT tenant_trainer_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: tenant_trainer tenant_trainer_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_trainer
    ADD CONSTRAINT tenant_trainer_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: upozorneni_skupiny upozorneni_skupiny_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT upozorneni_skupiny_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


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
-- Name: upozorneni upozorneni_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni
    ADD CONSTRAINT upozorneni_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: upozorneni upozorneni_up_kdo_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni
    ADD CONSTRAINT upozorneni_up_kdo_fkey FOREIGN KEY (up_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: user_proxy user_proxy_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_proxy
    ADD CONSTRAINT user_proxy_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: user_proxy user_proxy_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_proxy
    ADD CONSTRAINT user_proxy_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(u_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: users users_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: parameters admin_all; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY admin_all ON app_private.parameters TO administrator USING (true) WITH CHECK (true);


--
-- Name: pary_navrh admin_all; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY admin_all ON app_private.pary_navrh TO administrator USING (true) WITH CHECK (true);


--
-- Name: parameters all_view; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY all_view ON app_private.parameters FOR SELECT USING (true);


--
-- Name: pary_navrh manage_own; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY manage_own ON app_private.pary_navrh USING (((pn_navrhl = public.current_user_id()) OR (pn_partner = public.current_user_id()) OR (pn_partnerka = public.current_user_id()))) WITH CHECK (((pn_navrhl = public.current_user_id()) AND ((pn_partner = public.current_user_id()) OR (pn_partnerka = public.current_user_id()))));


--
-- Name: parameters; Type: ROW SECURITY; Schema: app_private; Owner: -
--

ALTER TABLE app_private.parameters ENABLE ROW LEVEL SECURITY;

--
-- Name: pary_navrh; Type: ROW SECURITY; Schema: app_private; Owner: -
--

ALTER TABLE app_private.pary_navrh ENABLE ROW LEVEL SECURITY;

--
-- Name: aktuality admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.aktuality TO administrator USING (true) WITH CHECK (true);


--
-- Name: attachment admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.attachment TO administrator USING (true) WITH CHECK (true);


--
-- Name: cohort_group admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.cohort_group TO administrator USING (true) WITH CHECK (true);


--
-- Name: cohort_membership admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.cohort_membership TO administrator USING (true);


--
-- Name: couple admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.couple TO administrator USING (true);


--
-- Name: dokumenty admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.dokumenty TO administrator USING (true) WITH CHECK (true);


--
-- Name: event admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event TO administrator USING (true);


--
-- Name: event_attendance admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_attendance TO administrator USING (true);


--
-- Name: event_instance admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_instance TO administrator USING (true);


--
-- Name: event_instance_trainer admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_instance_trainer TO administrator USING (true);


--
-- Name: event_lesson_demand admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_lesson_demand TO administrator USING (true);


--
-- Name: event_registration admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_registration TO administrator USING (true);


--
-- Name: event_target_cohort admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_target_cohort TO administrator USING (true);


--
-- Name: event_trainer admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_trainer TO administrator USING (true);


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
-- Name: person admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.person TO administrator USING (true);


--
-- Name: person_address admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.person_address TO administrator USING (true);


--
-- Name: person_email admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.person_email TO administrator USING (true);


--
-- Name: person_phone admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.person_phone TO administrator USING (true);


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

CREATE POLICY admin_all ON public.tenant TO administrator USING ((id = public.current_tenant_id()));


--
-- Name: tenant_administrator admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant_administrator TO administrator USING (true);


--
-- Name: tenant_attachment admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant_attachment TO administrator USING (true) WITH CHECK (true);


--
-- Name: tenant_location admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant_location TO administrator USING (true) WITH CHECK (true);


--
-- Name: tenant_membership admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant_membership TO administrator USING (true);


--
-- Name: tenant_trainer admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant_trainer TO administrator USING (true);


--
-- Name: upozorneni admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.upozorneni TO administrator USING (true) WITH CHECK (true);


--
-- Name: upozorneni_skupiny admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.upozorneni_skupiny TO administrator USING (true) WITH CHECK (true);


--
-- Name: user_proxy admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.user_proxy TO administrator USING (true);


--
-- Name: users admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.users TO administrator USING (true) WITH CHECK (true);


--
-- Name: person_invitation admin_create; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_create ON public.person_invitation USING ((EXISTS ( SELECT 1
   FROM public.tenant_administrator
  WHERE ((tenant_administrator.person_id = ANY (public.current_person_ids())) AND (tenant_administrator.tenant_id = public.current_tenant_id())))));


--
-- Name: person admin_myself; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_myself ON public.person FOR UPDATE USING ((id IN ( SELECT public.my_person_ids() AS my_person_ids)));


--
-- Name: person_address admin_personal; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_personal ON public.person_address USING ((person_id IN ( SELECT public.my_person_ids() AS my_person_ids)));


--
-- Name: person_email admin_personal; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_personal ON public.person_email USING ((person_id IN ( SELECT public.my_person_ids() AS my_person_ids)));


--
-- Name: person_phone admin_personal; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_personal ON public.person_phone USING ((person_id IN ( SELECT public.my_person_ids() AS my_person_ids)));


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
-- Name: cohort_group; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.cohort_group ENABLE ROW LEVEL SECURITY;

--
-- Name: cohort_membership; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.cohort_membership ENABLE ROW LEVEL SECURITY;

--
-- Name: couple; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.couple ENABLE ROW LEVEL SECURITY;

--
-- Name: dokumenty; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.dokumenty ENABLE ROW LEVEL SECURITY;

--
-- Name: event; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event ENABLE ROW LEVEL SECURITY;

--
-- Name: event_attendance; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event_attendance ENABLE ROW LEVEL SECURITY;

--
-- Name: event_instance; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event_instance ENABLE ROW LEVEL SECURITY;

--
-- Name: event_instance_trainer; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event_instance_trainer ENABLE ROW LEVEL SECURITY;

--
-- Name: event_lesson_demand; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event_lesson_demand ENABLE ROW LEVEL SECURITY;

--
-- Name: event_registration; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event_registration ENABLE ROW LEVEL SECURITY;

--
-- Name: event_target_cohort; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event_target_cohort ENABLE ROW LEVEL SECURITY;

--
-- Name: event_trainer; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event_trainer ENABLE ROW LEVEL SECURITY;

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
-- Name: session manage_own; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_own ON public.session USING ((ss_user = public.current_user_id())) WITH CHECK ((ss_user = public.current_user_id()));


--
-- Name: users manage_own; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_own ON public.users USING ((u_id = public.current_user_id())) WITH CHECK ((u_id = public.current_user_id()));


--
-- Name: platby_category member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.platby_category FOR SELECT TO member USING (true);


--
-- Name: platby_category_group member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.platby_category_group FOR SELECT TO member USING (true);


--
-- Name: platby_group member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.platby_group FOR SELECT TO member USING (true);


--
-- Name: platby_group_skupina member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.platby_group_skupina FOR SELECT TO member USING (true);


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
-- Name: upozorneni member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.upozorneni FOR SELECT TO member USING (true);


--
-- Name: upozorneni_skupiny member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.upozorneni_skupiny FOR SELECT TO member USING (true);


--
-- Name: aktuality my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.aktuality AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: cohort_group my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.cohort_group AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: dokumenty my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.dokumenty AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


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
-- Name: person_invitation my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.person_invitation AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));


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
-- Name: skupiny my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.skupiny AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: tenant_attachment my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.tenant_attachment AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


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
-- Name: otp_token; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.otp_token ENABLE ROW LEVEL SECURITY;

--
-- Name: person; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.person ENABLE ROW LEVEL SECURITY;

--
-- Name: person_address; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.person_address ENABLE ROW LEVEL SECURITY;

--
-- Name: person_email; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.person_email ENABLE ROW LEVEL SECURITY;

--
-- Name: person_invitation; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.person_invitation ENABLE ROW LEVEL SECURITY;

--
-- Name: person_phone; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.person_phone ENABLE ROW LEVEL SECURITY;

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

CREATE POLICY public_view ON public.attachment FOR SELECT TO anonymous USING (true);


--
-- Name: cohort_group public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.cohort_group FOR SELECT TO anonymous USING (true);


--
-- Name: dokumenty public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.dokumenty FOR SELECT TO member USING (true);


--
-- Name: location public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.location FOR SELECT TO anonymous USING (true);


--
-- Name: location_attachment public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.location_attachment FOR SELECT TO anonymous USING (true);


--
-- Name: room public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.room FOR SELECT TO anonymous USING (true);


--
-- Name: room_attachment public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.room_attachment FOR SELECT TO anonymous USING (true);


--
-- Name: tenant public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant FOR SELECT TO anonymous USING (true);


--
-- Name: tenant_administrator public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant_administrator FOR SELECT USING (true);


--
-- Name: tenant_attachment public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant_attachment FOR SELECT TO anonymous USING (true);


--
-- Name: tenant_location public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant_location FOR SELECT TO anonymous USING (true);


--
-- Name: tenant_trainer public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant_trainer FOR SELECT USING (true);


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
-- Name: tenant_administrator; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant_administrator ENABLE ROW LEVEL SECURITY;

--
-- Name: tenant_attachment; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant_attachment ENABLE ROW LEVEL SECURITY;

--
-- Name: tenant_location; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant_location ENABLE ROW LEVEL SECURITY;

--
-- Name: tenant_membership; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant_membership ENABLE ROW LEVEL SECURITY;

--
-- Name: tenant_trainer; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant_trainer ENABLE ROW LEVEL SECURITY;

--
-- Name: upozorneni; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.upozorneni ENABLE ROW LEVEL SECURITY;

--
-- Name: upozorneni_skupiny; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.upozorneni_skupiny ENABLE ROW LEVEL SECURITY;

--
-- Name: user_proxy; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.user_proxy ENABLE ROW LEVEL SECURITY;

--
-- Name: users; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.users ENABLE ROW LEVEL SECURITY;

--
-- Name: user_proxy view_personal; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_personal ON public.user_proxy FOR SELECT USING ((user_id = public.current_user_id()));


--
-- Name: event view_public; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_public ON public.event FOR SELECT TO anonymous USING ((is_public = true));


--
-- Name: event view_same_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_same_tenant ON public.event FOR SELECT USING ((tenant_id IN ( SELECT public.my_tenant_ids() AS my_tenant_ids)));


--
-- Name: person view_same_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_same_tenant ON public.person FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.tenant_membership
  WHERE ((now() <@ tenant_membership.active_range) AND (tenant_membership.person_id = tenant_membership.id) AND (tenant_membership.tenant_id IN ( SELECT public.my_tenant_ids() AS my_tenant_ids))))));


--
-- Name: person view_tenant_admin; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_tenant_admin ON public.person FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.tenant_administrator
  WHERE ((now() <@ tenant_administrator.active_range) AND (tenant_administrator.person_id = tenant_administrator.id)))));


--
-- Name: person view_tenant_trainer; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_tenant_trainer ON public.person FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.tenant_trainer
  WHERE ((now() <@ tenant_trainer.active_range) AND (tenant_trainer.person_id = tenant_trainer.id)))));


--
-- Name: event_attendance view_visible_event; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_event ON public.event_attendance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event_instance
  WHERE (event_attendance.instance_id = event_instance.id))));


--
-- Name: event_instance view_visible_event; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_event ON public.event_instance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_instance.event_id = event.id))));


--
-- Name: event_instance_trainer view_visible_event; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_event ON public.event_instance_trainer FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event_instance
  WHERE (event_instance_trainer.instance_id = event_instance.id))));


--
-- Name: event_lesson_demand view_visible_event; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_event ON public.event_lesson_demand FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event_registration
  WHERE (event_lesson_demand.registration_id = event_registration.id))));


--
-- Name: event_registration view_visible_event; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_event ON public.event_registration FOR SELECT USING (((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_registration.event_id = event.id))) AND ((EXISTS ( SELECT 1
   FROM public.person
  WHERE (event_registration.person_id = person.id))) OR (EXISTS ( SELECT 1
   FROM public.couple
  WHERE (event_registration.couple_id = couple.id))))));


--
-- Name: event_target_cohort view_visible_event; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_event ON public.event_target_cohort FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_target_cohort.event_id = event.id))));


--
-- Name: event_trainer view_visible_event; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_event ON public.event_trainer FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_trainer.event_id = event.id))));


--
-- Name: cohort_membership view_visible_person; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_person ON public.cohort_membership FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.person
  WHERE (cohort_membership.person_id = person.id))));


--
-- Name: couple view_visible_person; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_person ON public.couple FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.person
  WHERE ((couple.man_id = person.id) OR (couple.woman_id = person.id)))));


--
-- Name: person_address view_visible_person; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_person ON public.person_address FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.person
  WHERE (person_address.person_id = person.id))));


--
-- Name: person_email view_visible_person; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_person ON public.person_email FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.person
  WHERE (person_email.person_id = person.id))));


--
-- Name: person_phone view_visible_person; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_person ON public.person_phone FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.person
  WHERE (person_phone.person_id = person.id))));


--
-- Name: tenant_membership view_visible_person; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_person ON public.tenant_membership FOR SELECT USING (true);


--
-- Name: SCHEMA app_private; Type: ACL; Schema: -; Owner: -
--

GRANT ALL ON SCHEMA app_private TO postgres;


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: -
--

REVOKE USAGE ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO anonymous;


--
-- Name: FUNCTION current_tenant_id(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_tenant_id() TO anonymous;


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
-- Name: COLUMN users.u_email; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_email) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_poznamky; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_poznamky) ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.u_nationality; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(u_nationality) ON TABLE public.users TO anonymous;


--
-- Name: TABLE event_registration; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_registration TO anonymous;


--
-- Name: TABLE upozorneni; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.upozorneni TO anonymous;


--
-- Name: FUNCTION archived_announcements(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.archived_announcements() TO anonymous;


--
-- Name: FUNCTION attachment_directories(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.attachment_directories() TO anonymous;


--
-- Name: FUNCTION current_user_id(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_user_id() TO anonymous;


--
-- Name: TABLE attachment; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.attachment TO anonymous;


--
-- Name: FUNCTION attachment_directory(attachment public.attachment); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.attachment_directory(attachment public.attachment) TO anonymous;


--
-- Name: FUNCTION cancel_registration(registration_id bigint); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.cancel_registration(registration_id bigint) TO anonymous;


--
-- Name: FUNCTION change_password(old_pass character varying, new_pass character varying); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.change_password(old_pass character varying, new_pass character varying) TO member;


--
-- Name: TABLE cohort_membership; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.cohort_membership TO anonymous;


--
-- Name: FUNCTION cohort_membership_active(c public.cohort_membership); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.cohort_membership_active(c public.cohort_membership) TO anonymous;


--
-- Name: TABLE couple; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.couple TO anonymous;


--
-- Name: FUNCTION couple_active(c public.couple); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.couple_active(c public.couple) TO anonymous;


--
-- Name: TABLE event_attendance; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_attendance TO anonymous;


--
-- Name: FUNCTION couple_attendances(p public.couple); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.couple_attendances(p public.couple) TO anonymous;


--
-- Name: TABLE person; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.person TO anonymous;


--
-- Name: FUNCTION create_person(INOUT p public.person, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamp with time zone); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.create_person(INOUT p public.person, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamp with time zone) TO anonymous;


--
-- Name: FUNCTION current_couple_ids(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_couple_ids() TO anonymous;


--
-- Name: FUNCTION current_person_ids(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_person_ids() TO anonymous;


--
-- Name: FUNCTION edit_registration(registration_id bigint, note text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.edit_registration(registration_id bigint, note text) TO anonymous;


--
-- Name: TABLE event_instance; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_instance TO anonymous;


--
-- Name: FUNCTION event_instances_for_range(only_mine boolean, type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_instances_for_range(only_mine boolean, type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone) TO anonymous;


--
-- Name: TABLE event; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event TO anonymous;


--
-- Name: FUNCTION event_registrants(e public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_registrants(e public.event) TO anonymous;


--
-- Name: FUNCTION event_remaining_lessons(e public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_remaining_lessons(e public.event) TO anonymous;


--
-- Name: FUNCTION event_remaining_person_spots(e public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_remaining_person_spots(e public.event) TO anonymous;


--
-- Name: TABLE event_trainer; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_trainer TO anonymous;


--
-- Name: FUNCTION event_trainer_lessons_remaining(e public.event_trainer); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_trainer_lessons_remaining(e public.event_trainer) TO anonymous;


--
-- Name: FUNCTION filtered_people(in_tenants bigint[], in_cohort bigint, is_trainer boolean, is_admin boolean); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.filtered_people(in_tenants bigint[], in_cohort bigint, is_trainer boolean, is_admin boolean) TO anonymous;


--
-- Name: TABLE tenant; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant TO anonymous;


--
-- Name: FUNCTION get_current_tenant(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.get_current_tenant() TO anonymous;


--
-- Name: FUNCTION get_current_user(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.get_current_user() TO anonymous;


--
-- Name: FUNCTION invitation_info(token uuid); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.invitation_info(token uuid) TO anonymous;


--
-- Name: TABLE session; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.session TO anonymous;


--
-- Name: FUNCTION login(login character varying, passwd character varying, OUT sess public.session, OUT usr public.users, OUT jwt public.jwt_token); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.login(login character varying, passwd character varying, OUT sess public.session, OUT usr public.users, OUT jwt public.jwt_token) TO anonymous;


--
-- Name: FUNCTION my_announcements(archive boolean); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.my_announcements(archive boolean) TO anonymous;


--
-- Name: FUNCTION my_cohort_ids(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.my_cohort_ids() TO anonymous;


--
-- Name: FUNCTION my_couple_ids(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.my_couple_ids() TO anonymous;


--
-- Name: FUNCTION my_person_ids(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.my_person_ids() TO anonymous;


--
-- Name: FUNCTION my_tenant_ids(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.my_tenant_ids() TO anonymous;


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
-- Name: FUNCTION person_active_couples(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_active_couples(p public.person) TO anonymous;


--
-- Name: FUNCTION person_all_couples(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_all_couples(p public.person) TO anonymous;


--
-- Name: FUNCTION person_cohort_ids(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_cohort_ids(p public.person) TO anonymous;


--
-- Name: FUNCTION person_couple_ids(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_couple_ids(p public.person) TO anonymous;


--
-- Name: FUNCTION person_has_user(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_has_user(p public.person) TO anonymous;


--
-- Name: FUNCTION person_is_admin(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_is_admin(p public.person) TO anonymous;


--
-- Name: FUNCTION person_is_trainer(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_is_trainer(p public.person) TO anonymous;


--
-- Name: FUNCTION person_name(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_name(p public.person) TO anonymous;


--
-- Name: TABLE person_address; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.person_address TO anonymous;


--
-- Name: FUNCTION person_tenant_ids(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_tenant_ids(p public.person) TO anonymous;


--
-- Name: TABLE event_lesson_demand; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_lesson_demand TO anonymous;


--
-- Name: FUNCTION register_to_event(INOUT registration public.event_registration, lessons public.event_lesson_demand[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.register_to_event(INOUT registration public.event_registration, lessons public.event_lesson_demand[]) TO anonymous;


--
-- Name: FUNCTION register_using_invitation(email text, login text, passwd text, token uuid, OUT usr public.users, OUT sess public.session, OUT jwt public.jwt_token); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.register_using_invitation(email text, login text, passwd text, token uuid, OUT usr public.users, OUT sess public.session, OUT jwt public.jwt_token) TO anonymous;


--
-- Name: FUNCTION reset_password(login character varying, email character varying); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.reset_password(login character varying, email character varying) TO anonymous;


--
-- Name: FUNCTION set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer) TO anonymous;


--
-- Name: FUNCTION sticky_announcements(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.sticky_announcements() TO anonymous;


--
-- Name: FUNCTION submit_form(type text, data jsonb, url text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.submit_form(type text, data jsonb, url text) TO anonymous;


--
-- Name: TABLE tenant_administrator; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant_administrator TO anonymous;


--
-- Name: FUNCTION tenant_administrator_active(c public.tenant_administrator); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.tenant_administrator_active(c public.tenant_administrator) TO anonymous;


--
-- Name: TABLE tenant_membership; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant_membership TO anonymous;


--
-- Name: FUNCTION tenant_membership_active(c public.tenant_membership); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.tenant_membership_active(c public.tenant_membership) TO anonymous;


--
-- Name: TABLE tenant_trainer; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant_trainer TO anonymous;


--
-- Name: FUNCTION tenant_trainer_active(c public.tenant_trainer); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.tenant_trainer_active(c public.tenant_trainer) TO anonymous;


--
-- Name: FUNCTION users_date_of_newest_payment(a public.users); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.users_date_of_newest_payment(a public.users) TO anonymous;


--
-- Name: FUNCTION users_date_of_oldest_payment(a public.users); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.users_date_of_oldest_payment(a public.users) TO anonymous;


--
-- Name: FUNCTION users_has_valid_payment(a public.users); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.users_has_valid_payment(a public.users) TO anonymous;


--
-- Name: FUNCTION users_in_public_cohort(a public.users); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.users_in_public_cohort(a public.users) TO anonymous;


--
-- Name: TABLE parameters; Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON TABLE app_private.parameters TO anonymous;


--
-- Name: TABLE pary_navrh; Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON TABLE app_private.pary_navrh TO anonymous;


--
-- Name: SEQUENCE pary_navrh_pn_id_seq; Type: ACL; Schema: app_private; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE app_private.pary_navrh_pn_id_seq TO anonymous;


--
-- Name: SEQUENCE akce_a_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.akce_a_id_seq TO anonymous;


--
-- Name: TABLE aktuality; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.aktuality TO anonymous;


--
-- Name: SEQUENCE aktuality_at_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.aktuality_at_id_seq TO anonymous;


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
-- Name: TABLE event_instance_trainer; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_instance_trainer TO anonymous;


--
-- Name: TABLE event_target_cohort; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_target_cohort TO anonymous;


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
-- Name: TABLE person_email; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.person_email TO anonymous;


--
-- Name: TABLE person_invitation; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.person_invitation TO anonymous;


--
-- Name: TABLE person_phone; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.person_phone TO anonymous;


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
-- Name: TABLE scoreboard; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.scoreboard TO anonymous;


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
-- Name: TABLE user_proxy; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.user_proxy TO anonymous;


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

