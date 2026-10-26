--
-- PostgreSQL database dump
--

\restrict Uj6s2rBFWEaDgfIWk3EuLVmOQ3zhEGFovqN5eMQolptGGIZkDNulGcVXW1mgdis

-- Dumped from database version 18.4
-- Dumped by pg_dump version 18.4

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
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
-- Name: crawler; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA crawler;


--
-- Name: federated; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA federated;


--
-- Name: public; Type: SCHEMA; Schema: -; Owner: -
--

-- *not* creating schema, since initdb creates it


--
-- Name: wdsf; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA wdsf;


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
-- Name: hypopg; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS hypopg WITH SCHEMA public;


--
-- Name: EXTENSION hypopg; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION hypopg IS 'Hypothetical indexes for PostgreSQL';


--
-- Name: pg_trgm; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pg_trgm WITH SCHEMA public;


--
-- Name: EXTENSION pg_trgm; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pg_trgm IS 'text similarity measurement and index searching based on trigrams';


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
-- Name: unaccent; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS unaccent WITH SCHEMA public;


--
-- Name: EXTENSION unaccent; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION unaccent IS 'text search dictionary that removes accents';


--
-- Name: fetch_status; Type: TYPE; Schema: crawler; Owner: -
--

CREATE TYPE crawler.fetch_status AS ENUM (
    'pending',
    'ok',
    'gone',
    'error',
    'transient'
);


--
-- Name: process_status; Type: TYPE; Schema: crawler; Owner: -
--

CREATE TYPE crawler.process_status AS ENUM (
    'pending',
    'ok',
    'error'
);


--
-- Name: competition_type; Type: TYPE; Schema: federated; Owner: -
--

CREATE TYPE federated.competition_type AS ENUM (
    'cup',
    'ranking',
    'league',
    'championship',
    'top_level',
    'super_league',
    'g_cup',
    'unknown'
);


--
-- Name: TYPE competition_type; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TYPE federated.competition_type IS '@name competition_type';


--
-- Name: competitor_role; Type: TYPE; Schema: federated; Owner: -
--

CREATE TYPE federated.competitor_role AS ENUM (
    'lead',
    'follow',
    'member',
    'substitute'
);


--
-- Name: TYPE competitor_role; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TYPE federated.competitor_role IS '@name competitor_role';


--
-- Name: competitor_type; Type: TYPE; Schema: federated; Owner: -
--

CREATE TYPE federated.competitor_type AS ENUM (
    'couple',
    'solo',
    'duo',
    'trio',
    'formation',
    'group',
    'team'
);


--
-- Name: TYPE competitor_type; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TYPE federated.competitor_type IS '@name competitor_type';


--
-- Name: event_status; Type: TYPE; Schema: federated; Owner: -
--

CREATE TYPE federated.event_status AS ENUM (
    'unknown',
    'planned',
    'in_progress',
    'completed',
    'postponed',
    'cancelled'
);


--
-- Name: TYPE event_status; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TYPE federated.event_status IS '@name federation_event_status';


--
-- Name: federation_event_year; Type: TYPE; Schema: federated; Owner: -
--

CREATE TYPE federated.federation_event_year AS (
	year integer,
	event_count integer,
	mapped_event_count integer
);


--
-- Name: TYPE federation_event_year; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TYPE federated.federation_event_year IS '@name federation_event_year';


--
-- Name: COLUMN federation_event_year.year; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.federation_event_year.year IS '@notNull';


--
-- Name: COLUMN federation_event_year.event_count; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.federation_event_year.event_count IS '@notNull';


--
-- Name: COLUMN federation_event_year.mapped_event_count; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.federation_event_year.mapped_event_count IS '@notNull';


--
-- Name: federation_event_facets; Type: TYPE; Schema: federated; Owner: -
--

CREATE TYPE federated.federation_event_facets AS (
	years federated.federation_event_year[]
);


--
-- Name: TYPE federation_event_facets; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TYPE federated.federation_event_facets IS '@name federation_event_facets';


--
-- Name: COLUMN federation_event_facets.years; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.federation_event_facets.years IS '@notNull';


--
-- Name: gender; Type: TYPE; Schema: federated; Owner: -
--

CREATE TYPE federated.gender AS ENUM (
    'male',
    'female',
    'other',
    'unknown'
);


--
-- Name: TYPE gender; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TYPE federated.gender IS '@name gender';


--
-- Name: official_role; Type: TYPE; Schema: federated; Owner: -
--

CREATE TYPE federated.official_role AS ENUM (
    'adjudicator',
    'chairperson',
    'invigilator',
    'scrutineer',
    'lead_scrutineer'
);


--
-- Name: TYPE official_role; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TYPE federated.official_role IS '@name official_role';


--
-- Name: person_license_discipline; Type: TYPE; Schema: federated; Owner: -
--

CREATE TYPE federated.person_license_discipline AS ENUM (
    'general',
    'standard',
    'latin',
    'breaking',
    'hiphop',
    'caribbean',
    'stage',
    'smooth',
    'disco',
    'solo_syncro_choreo',
    'ten_dance',
    'show_dance_standard',
    'show_dance_latin',
    'formation_standard',
    'formation_latin',
    'pd_standard',
    'pd_latin',
    'pd_ten_dance',
    'pd_show_dance_standard',
    'pd_show_dance_latin',
    'unknown'
);


--
-- Name: TYPE person_license_discipline; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TYPE federated.person_license_discipline IS '@name person_license_discipline';


--
-- Name: person_license_kind; Type: TYPE; Schema: federated; Owner: -
--

CREATE TYPE federated.person_license_kind AS ENUM (
    'athlete',
    'trainer',
    'adjudicator',
    'chairperson',
    'invigilator',
    'scrutineer',
    'lead_scrutineer',
    'examiner',
    'dj',
    'head_judge',
    'official'
);


--
-- Name: TYPE person_license_kind; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TYPE federated.person_license_kind IS '@name person_license_kind';


--
-- Name: person_license_status; Type: TYPE; Schema: federated; Owner: -
--

CREATE TYPE federated.person_license_status AS ENUM (
    'active',
    'expired',
    'revoked',
    'resting',
    'retired',
    'aspiring',
    'suspended',
    'unknown'
);


--
-- Name: TYPE person_license_status; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TYPE federated.person_license_status IS '@name person_license_status';


--
-- Name: score_component; Type: TYPE; Schema: federated; Owner: -
--

CREATE TYPE federated.score_component AS ENUM (
    'mark',
    'places',
    'ajs_tq',
    'ajs_mm',
    'ajs_ps',
    'ajs_cp',
    'ajs_reduction'
);


--
-- Name: TYPE score_component; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TYPE federated.score_component IS '@name score_component';


--
-- Name: scoring_method; Type: TYPE; Schema: federated; Owner: -
--

CREATE TYPE federated.scoring_method AS ENUM (
    'skating_marks',
    'skating_places',
    'ajs-3.0'
);


--
-- Name: TYPE scoring_method; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TYPE federated.scoring_method IS '@name scoring_method';


--
-- Name: access_credential_kind; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.access_credential_kind AS ENUM (
    'MIFARE'
);


--
-- Name: activity_timeline_kind; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.activity_timeline_kind AS ENUM (
    'EVENT_ATTENDANCE',
    'COMPETITION_BRIEF',
    'COMPETITION_RESULT',
    'JUDGING',
    'BIRTHDAY'
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
-- Name: announcement_audience_role; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.announcement_audience_role AS ENUM (
    'member',
    'trainer',
    'administrator'
);


--
-- Name: announcement_audience_type_input; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.announcement_audience_type_input AS (
	id bigint,
	cohort_id bigint,
	audience_role public.announcement_audience_role
);


--
-- Name: announcement_status; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.announcement_status AS ENUM (
    'draft',
    'scheduled',
    'published',
    'archived'
);


--
-- Name: announcement_type_input; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.announcement_type_input AS (
	id bigint,
	title text,
	body text,
	is_sticky boolean,
	scheduled_since timestamp with time zone,
	scheduled_until timestamp with time zone,
	status public.announcement_status
);


--
-- Name: application_form_status; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.application_form_status AS ENUM (
    'new',
    'sent',
    'approved',
    'rejected'
);


--
-- Name: article_type_input; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.article_type_input AS (
	id bigint,
	title text,
	preview text,
	body text,
	title_photo_url text,
	is_visible boolean,
	published_at timestamp with time zone
);


--
-- Name: attendance_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.attendance_type AS ENUM (
    'unknown',
    'attended',
    'not-excused'
);


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: category; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.category (
    id bigint NOT NULL,
    name text NOT NULL,
    series text NOT NULL,
    discipline text NOT NULL,
    age_group text NOT NULL,
    gender_group text DEFAULT 'mixed'::text NOT NULL,
    class text NOT NULL,
    competitor_type federated.competitor_type DEFAULT 'couple'::federated.competitor_type NOT NULL,
    base_dance_program_id bigint,
    age_group_code text
);


--
-- Name: TABLE category; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.category IS '@name category
@behavior -insert -update -delete +list -connection';


--
-- Name: COLUMN category.age_group; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.category.age_group IS '@behavior -*';


--
-- Name: COLUMN category.age_group_code; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.category.age_group_code IS '@name ageGroup
@behavior -insert -update';


--
-- Name: competition_participation_record; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.competition_participation_record AS (
	person_id bigint,
	person_name text,
	federation text,
	federated_person_id text,
	competitor_id text,
	competitor_name text,
	competitor_type federated.competitor_type,
	event_id bigint,
	event_name text,
	event_location text,
	competition_id bigint,
	competition_date date,
	check_in_end time without time zone,
	category federated.category,
	dances text[],
	participants integer,
	ranking integer,
	ranking_to integer,
	point_gain numeric(10,3),
	is_final boolean,
	has_result boolean,
	competition_type federated.competition_type,
	event_external_id text,
	competition_external_id text,
	final_gain numeric(10,3)
);


--
-- Name: event_capacity_unit; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_capacity_unit AS ENUM (
    'people',
    'registrations'
);


--
-- Name: event_conflict; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_conflict AS (
	person_id bigint,
	person_name text,
	first_event_id bigint,
	first_event_name text,
	first_since timestamp with time zone,
	first_until timestamp with time zone,
	second_event_id bigint,
	second_event_name text,
	second_since timestamp with time zone,
	second_until timestamp with time zone,
	overlap_range tstzrange
);


--
-- Name: TYPE event_conflict; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TYPE public.event_conflict IS '
@foreignKey (person_id) references person (id)
@foreignKey (first_event_id) references event (id)|@fieldName firstEvent
@foreignKey (second_event_id) references event (id)|@fieldName secondEvent
';


--
-- Name: COLUMN event_conflict.person_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.event_conflict.person_id IS '@notNull';


--
-- Name: COLUMN event_conflict.first_event_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.event_conflict.first_event_id IS '@notNull';


--
-- Name: COLUMN event_conflict.second_event_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.event_conflict.second_event_id IS '@notNull';


--
-- Name: COLUMN event_conflict.overlap_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.event_conflict.overlap_range IS '@notNull';


--
-- Name: event_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_type AS ENUM (
    'camp',
    'lesson',
    'reservation',
    'holiday',
    'group'
);


--
-- Name: event_details_input; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_details_input AS (
	parent_id bigint,
	name text,
	type public.event_type,
	location_id bigint,
	location_text text,
	capacity integer,
	capacity_unit public.event_capacity_unit,
	is_visible boolean,
	is_public boolean,
	has_public_details boolean,
	is_locked boolean,
	enable_notes boolean
);


--
-- Name: event_registration_input; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_registration_input AS (
	person_id bigint,
	couple_id bigint,
	target_cohort_id bigint,
	is_cancelled boolean
);


--
-- Name: event_input; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_input AS (
	id bigint,
	since timestamp with time zone,
	until timestamp with time zone,
	is_cancelled boolean,
	registrations public.event_registration_input[]
);


--
-- Name: event_instance_range_scope; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_instance_range_scope AS ENUM (
    'all',
    'top_level',
    'mine',
    'relevant'
);


--
-- Name: event_registration_info; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_registration_info AS (
	registrations integer,
	people integer,
	remaining_capacity integer,
	my boolean
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
-- Name: event_registration_person_input; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_registration_person_input AS (
	first_name text,
	last_name text,
	gender public.gender_type,
	nationality text,
	email text,
	phone text,
	birth_date date,
	csts_id integer,
	wdsf_id integer
);


--
-- Name: event_registration_source; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_registration_source AS ENUM (
    'self',
    'manager',
    'cohort'
);


--
-- Name: event_registration_status; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_registration_status AS ENUM (
    'active',
    'cancelled'
);


--
-- Name: event_series_input; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_series_input AS (
	id bigint,
	name text
);


--
-- Name: event_trainer_input; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_trainer_input AS (
	person_id bigint,
	lessons_offered integer
);


--
-- Name: jwt_token; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.jwt_token AS (
	exp integer,
	user_id bigint,
	tenant_id bigint,
	email text,
	my_person_ids bigint[],
	my_tenant_ids bigint[],
	my_cohort_ids bigint[],
	my_couple_ids bigint[],
	is_system_admin boolean,
	guest_tenant_ids bigint[],
	member_tenant_ids bigint[],
	trainer_tenant_ids bigint[],
	admin_tenant_ids bigint[]
);


--
-- Name: TYPE jwt_token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TYPE public.jwt_token IS '@jwt';


--
-- Name: location_details_input; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.location_details_input AS (
	id bigint,
	name text,
	long_name text,
	description text,
	address public.address_domain,
	show_in_lists boolean,
	ordering integer,
	latitude double precision,
	longitude double precision
);


--
-- Name: current_tenant_id(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_tenant_id() RETURNS bigint
    LANGUAGE sql STABLE
    AS $$
  select COALESCE(nullif(current_setting('jwt.claims.tenant_id', true), '')::bigint, 1);
$$;


--
-- Name: FUNCTION current_tenant_id(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.current_tenant_id() IS '@behavior -*';


--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users (
    id bigint CONSTRAINT users_u_id_not_null NOT NULL,
    u_login public.citext,
    u_pass character(40) NOT NULL,
    u_jmeno text,
    u_prijmeni text,
    u_email public.citext NOT NULL,
    updated_at timestamp with time zone DEFAULT now() CONSTRAINT users_u_timestamp_not_null NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP CONSTRAINT users_u_created_at_not_null NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    last_login timestamp with time zone,
    last_active_at timestamp with time zone,
    last_version text
);


--
-- Name: TABLE users; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.users IS '@behavior -insert -update -delete';


--
-- Name: COLUMN users.u_pass; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.users.u_pass IS '@behavior -*';


--
-- Name: login_result; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.login_result AS (
	usr public.users,
	jwt public.jwt_token
);


--
-- Name: TYPE login_result; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TYPE public.login_result IS '@name result';


--
-- Name: payment_status; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.payment_status AS ENUM (
    'tentative',
    'unpaid',
    'paid'
);


--
-- Name: person_federation_candidates_record; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.person_federation_candidates_record AS (
	id integer,
	name text,
	age_group text,
	similarity real,
	birth_year integer,
	csts_id integer,
	wdsf_id integer,
	is_strong_match boolean
);


--
-- Name: COLUMN person_federation_candidates_record.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.person_federation_candidates_record.id IS '@notNull';


--
-- Name: COLUMN person_federation_candidates_record.similarity; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.person_federation_candidates_record.similarity IS '@notNull';


--
-- Name: COLUMN person_federation_candidates_record.is_strong_match; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.person_federation_candidates_record.is_strong_match IS '@notNull';


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
	CONSTRAINT price_check CHECK (((VALUE IS NULL) OR (((VALUE).currency IS NOT NULL) AND ((VALUE).amount IS NOT NULL) AND (length((VALUE).currency) = 3) AND ((VALUE).currency = upper((VALUE).currency)))));


--
-- Name: relationship_status; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.relationship_status AS ENUM (
    'pending',
    'active',
    'expired'
);


--
-- Name: scoreboard_record; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.scoreboard_record AS (
	person_id bigint,
	cohort_id bigint,
	lesson_total_score bigint,
	group_total_score bigint,
	event_total_score bigint,
	manual_total_score bigint,
	total_score bigint,
	ranking bigint
);


--
-- Name: TYPE scoreboard_record; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TYPE public.scoreboard_record IS '@foreignKey (person_id) references person (id)
@foreignKey (cohort_id) references cohort (id)';


--
-- Name: security_event_kind; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.security_event_kind AS ENUM (
    'login_succeeded',
    'login_failed',
    'registration',
    'impersonation',
    'password_reset_requested',
    'password_changed',
    'invitation_accepted',
    'person_linked',
    'person_unlinked',
    'membership_granted',
    'membership_revoked',
    'trainer_granted',
    'trainer_revoked',
    'administrator_granted',
    'administrator_revoked',
    'access_credential_issued',
    'access_credential_ended'
);


--
-- Name: security_event_method; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.security_event_method AS ENUM (
    'password',
    'otp',
    'manual',
    'scheduled'
);


--
-- Name: series_info; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.series_info AS (
	id bigint,
	name text,
	"position" integer,
	length integer,
	since timestamp with time zone,
	until timestamp with time zone
);


--
-- Name: trainer_group_attendance_completion; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.trainer_group_attendance_completion AS (
	person_id bigint,
	total_events integer,
	filled_events integer,
	partially_filled_events integer,
	unfilled_events integer,
	filled_ratio double precision,
	total_attendances integer,
	pending_attendances integer
);


--
-- Name: TYPE trainer_group_attendance_completion; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TYPE public.trainer_group_attendance_completion IS '@foreignKey (person_id) references person (id)';


--
-- Name: transaction_source; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.transaction_source AS ENUM (
    'auto-bank',
    'auto-credit',
    'manual-bank',
    'manual-credit',
    'manual-cash'
);


--
-- Name: accessible_event_ids(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.accessible_event_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  with recursive attended(id) as (
    select event_id from public.event_registration
    where tenant_id = (select public.current_tenant_id())
      and person_id = any((select public.current_person_ids())::bigint[])
      and registration_status = 'active'
    union
    select child.id from public.event child
    join attended parent on child.parent_id = parent.id
    where child.parent_id is not null
      and child.tenant_id = (select public.current_tenant_id())
  )
  -- Scan events once; their primary key already makes the result unique.
  select id from public.event
  where tenant_id = (select public.current_tenant_id())
    and (
      is_public
      or (is_visible and (select pg_has_role(current_setting('role'), 'member', 'member')))
      or (manager_person_ids && (select current_person_ids()))
      or id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[])
      or id in (select id from attended)
    );
$$;


--
-- Name: announcement_status_next(timestamp with time zone, timestamp with time zone, timestamp with time zone, public.announcement_status); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.announcement_status_next(ts timestamp with time zone, scheduled_since timestamp with time zone, scheduled_until timestamp with time zone, current_status public.announcement_status) RETURNS public.announcement_status
    LANGUAGE sql IMMUTABLE
    AS $$
  select case
    when current_status in ('draft', 'archived') then current_status
    when scheduled_until is not null and ts >= scheduled_until then 'archived'
    when scheduled_since is not null and ts < scheduled_since then 'scheduled'
    else 'published'
  end::announcement_status;
$$;


--
-- Name: can_trainer_edit_event(bigint); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.can_trainer_edit_event(p_event_id bigint) RETURNS boolean
    LANGUAGE sql STABLE SECURITY DEFINER LEAKPROOF PARALLEL SAFE
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  with recursive chain as (
    select i.id, i.parent_id from event i where i.id = p_event_id
    union all
    select p.id, p.parent_id from event p join chain c on p.id = c.parent_id
  ),
  trainers as (
    select person_id from event_trainer where event_id in (select id from chain)
  )
  select exists (
    select 1 from trainers where person_id = any ((select current_person_ids())::bigint[])
  ) or not exists (
    select 1 from trainers
  );
$$;


--
-- Name: cohort_has_accessible_event(bigint); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.cohort_has_accessible_event(p_cohort_id bigint) RETURNS boolean
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  -- Most cohorts have a public or member-visible event. Check their indexed
  -- links before enumerating the caller's additional event access.
  select exists (
    select 1
    from event_target_cohort target
    join event e on e.tenant_id = target.tenant_id and e.id = target.event_id
    where target.tenant_id = (select current_tenant_id())
      and target.cohort_id = p_cohort_id
      and (e.is_public or (e.is_visible
        and (select pg_has_role(current_setting('role'), 'member', 'member'))))
  ) or exists (
    with targets as materialized (
      select event_id from event_target_cohort
      where tenant_id = (select current_tenant_id()) and cohort_id = p_cohort_id
    )
    select 1 from targets
    where event_id = any (array(select app_private.accessible_event_ids()))
  );
$$;


--
-- Name: competitor_category_candidates(text[], text[], text[]); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.competitor_category_candidates(age_groups text[] DEFAULT NULL::text[], classes text[] DEFAULT NULL::text[], disciplines text[] DEFAULT NULL::text[]) RETURNS TABLE(competitor_id text, age_group_code text, class text, discipline text, age_matches boolean, class_matches boolean, discipline_matches boolean)
    LANGUAGE sql STABLE
    AS $$
  select p.competitor_id, cat.age_group_code, cat.class, cat.discipline,
    coalesce(cardinality(age_groups), 0) = 0 or cat.age_group_code = any(
      array(select federated.resolve_age_group(value) from unnest(age_groups) value)
    ),
    coalesce(cardinality(classes), 0) = 0 or cat.class = any(classes),
    coalesce(cardinality(disciplines), 0) = 0 or cat.discipline = any(disciplines)
  from federated.competitor_category_progress p
  join federated.category cat on cat.id = p.category_id;
$$;


--
-- Name: create_jwt_token(public.users); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.create_jwt_token(u public.users) RETURNS public.jwt_token
    LANGUAGE sql STABLE
    AS $$
with
  person_ids as (
    select distinct up.person_id from user_proxy up where up.user_id = u.id
  ),
  tenant_memberships as (
    select distinct tm.tenant_id from tenant_membership tm join person_ids p on p.person_id = tm.person_id where tm.status = 'active'
  ),
  tenant_trainers as (
    select distinct tt.tenant_id from tenant_trainer tt join person_ids p on p.person_id = tt.person_id where tt.status = 'active'
  ),
  tenant_admins as (
    select distinct ta.tenant_id from tenant_administrator ta join person_ids p on p.person_id = ta.person_id where ta.status = 'active'
    union
    select id from tenant where app_private.is_system_admin(u.id)
  ),
  tenant_ids as (
    select tenant_id from tenant_memberships
    union
    select tenant_id from tenant_trainers
    union
    select tenant_id from tenant_admins
  ),
  cohort_ids as (
    select distinct cm.cohort_id from cohort_membership cm join person_ids p on p.person_id = cm.person_id where cm.status = 'active'
  ),
  couple_ids as (
    select distinct c.id from couple c join person_ids p on p.person_id = c.man_id where c.status = 'active'
    union all
    select distinct c.id from couple c join person_ids p on p.person_id = c.woman_id where c.status = 'active'
  )
  select
    extract(epoch from now() + interval '7 days')::integer as exp,
    u.id as user_id,
    (select current_tenant_id()) as tenant_id,
    u.u_email as email,

    coalesce((select array_agg(p.person_id) from person_ids p), '{}'::bigint[]) as my_person_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_ids p), '{}'::bigint[]) as my_tenant_ids,
    coalesce((select array_agg(p.cohort_id) from cohort_ids p), '{}'::bigint[]) as my_cohort_ids,
    coalesce((select array_agg(p.id) from couple_ids p), '{}'::bigint[]) as my_couple_ids,

    app_private.is_system_admin(u.id) as is_system_admin,

    '{}'::bigint[] as guest_tenant_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_memberships p), '{}'::bigint[]) as member_tenant_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_trainers p), '{}'::bigint[]) as trainer_tenant_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_admins p), '{}'::bigint[]) as admin_tenant_ids;
$$;


--
-- Name: FUNCTION create_jwt_token(u public.users); Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON FUNCTION app_private.create_jwt_token(u public.users) IS 'Generates the JWT payload including global system administrator flag.';


--
-- Name: payment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.payment (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    accounting_period_id bigint NOT NULL,
    cohort_subscription_id bigint,
    event_id bigint,
    status public.payment_status NOT NULL,
    variable_symbol text,
    specific_symbol text,
    is_auto_credit_allowed boolean DEFAULT true NOT NULL,
    due_at timestamp with time zone,
    paid_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE payment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.payment IS '@behavior +list -connection -insert -query:resource:list -query:resource:connection';


--
-- Name: create_latest_lesson_payments(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.create_latest_lesson_payments() RETURNS SETOF public.payment
    LANGUAGE plpgsql
    AS $$
begin
  perform set_config('jwt.claims.tenant_id', '2', true);
  perform set_config('jwt.claims.my_tenant_ids', '[2]', true);
  if not (select row_security_active('event')) then
    set local role to administrator;
  end if;

  return query WITH created AS (
    SELECT p.*
    FROM event ei
      JOIN LATERAL create_event_payment(ei) p ON true
    WHERE ei.type = 'lesson'
      AND NOT ei.is_cancelled
      AND ei.since < now()
      AND NOT EXISTS (
        SELECT 1
        FROM payment p
        WHERE p.event_id = ei.id AND p.status = 'paid'
      )
      AND p.status in ('unpaid', 'tentative')
  ),
  unpaid AS (
    UPDATE payment p
      SET status = 'unpaid'
      FROM created
      WHERE p.id = created.id
      RETURNING p.*
  )
  SELECT p.*
  FROM unpaid
    CROSS JOIN LATERAL resolve_payment_with_credit(unpaid.*) p
  WHERE p IS NOT NULL;
end;
$$;


--
-- Name: cron_update_memberships(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.cron_update_memberships() RETURNS void
    LANGUAGE sql
    AS $$
  UPDATE user_proxy SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE couple SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE cohort_membership SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE tenant_membership SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE tenant_trainer SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE tenant_administrator SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE access_credential SET status = app_private.relationship_status_next(now(), valid_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), valid_range, status);

  UPDATE announcement
  SET status = app_private.announcement_status_next(now(), scheduled_since, scheduled_until, status)
  WHERE status IN ('scheduled', 'published')
    AND status IS DISTINCT FROM app_private.announcement_status_next(now(), scheduled_since, scheduled_until, status);
$$;


--
-- Name: drop_policies(text); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.drop_policies(tbl text) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  target regclass := tbl::regclass;
  policy_name name;
begin
  for policy_name in
    select polname
    from pg_catalog.pg_policy
    where polrelid = target
  loop
    execute format('drop policy %I on %s', policy_name, target);
  end loop;
end;
$$;


--
-- Name: ensure_couple_person_registrations(bigint); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.ensure_couple_person_registrations(p_registration_id bigint) RETURNS void
    LANGUAGE plpgsql
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  parent event_registration;
  child event_registration;
  partner_id bigint;
begin
  perform e.id from event e
  join event_registration r on r.event_id = e.id
  where r.id = p_registration_id for update of e;

  select * into parent from event_registration
  where id = p_registration_id and couple_id is not null and registration_status = 'active';
  if not found then return; end if;

  for partner_id in
    select unnest(array[man_id, woman_id]) from couple where id = parent.couple_id
  loop
    select * into child from event_registration
    where event_id = parent.event_id and person_id = partner_id
    order by (registration_status = 'active') desc,
      (parent_registration_id = parent.id) desc nulls last,
      (parent_registration_id is null) desc, id desc
    limit 1 for update;

    if found then
      if child.registration_status = 'active' and child.parent_registration_id is not null
        and child.parent_registration_id <> parent.id then
        raise exception 'PERSON_ALREADY_REGISTERED' using errcode = '22023';
      end if;

      -- Keep attendance, notes and personal lesson requests on the same row.
      update event_registration
      set parent_registration_id = parent.id, registration_status = 'active',
          source = parent.source, target_cohort_id = null
      where id = child.id and (parent_registration_id, registration_status, source, target_cohort_id)
        is distinct from (parent.id, 'active'::event_registration_status, parent.source, null);
    else
      insert into event_registration (event_id, tenant_id, parent_registration_id, person_id, status, source)
      values (parent.event_id, parent.tenant_id, parent.id, partner_id, 'unknown', parent.source);
    end if;
  end loop;
end;
$$;


--
-- Name: event_manager_person_ids(bigint); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.event_manager_person_ids(p_event_id bigint) RETURNS bigint[]
    LANGUAGE sql STABLE PARALLEL SAFE
    AS $$
  with recursive chain as (
    select id, parent_id
    from event where id = p_event_id
    union all
    select parent.id, parent.parent_id
    from event parent
    join chain child on child.parent_id = parent.id
  ), managers as (
    select distinct person_id from event_trainer
    where event_id in (select id from chain)
  )
  select coalesce(array_agg(person_id order by person_id), '{}'::bigint[])
  from managers;
$$;


--
-- Name: event_share_claims(bigint, text); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.event_share_claims(p_tenant_id bigint, p_share text) RETURNS TABLE(instance_ids bigint[])
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $_$
  with recursive root as (
    select id
    from event
    where tenant_id = p_tenant_id
      and (
        share_token = p_share
        or (
          is_public
          and has_public_details
          and id = case
            when p_share ~ '^\d{1,18}$' then p_share::bigint
          end
        )
      )
  ),
  events as (
    select id from root
    union
    select child.id
    from event child
    join events parent on child.parent_id = parent.id
    where child.tenant_id = p_tenant_id
  )
  select array(select id from events)
  where exists (select 1 from root);
$_$;


--
-- Name: event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    since timestamp with time zone NOT NULL,
    until timestamp with time zone NOT NULL,
    is_cancelled boolean DEFAULT false NOT NULL,
    range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
    name text,
    type public.event_type,
    location_text text,
    location_id bigint,
    is_visible boolean,
    is_public boolean,
    manager_person_ids bigint[] DEFAULT '{}'::bigint[] NOT NULL,
    stats jsonb DEFAULT '{}'::jsonb NOT NULL,
    parent_id bigint,
    capacity integer,
    capacity_unit public.event_capacity_unit DEFAULT 'people'::public.event_capacity_unit NOT NULL,
    description text,
    summary text,
    is_locked boolean,
    enable_notes boolean,
    files_legacy text,
    series_id bigint,
    has_public_details boolean DEFAULT false NOT NULL,
    share_token text,
    cover_image_id bigint,
    series_info public.series_info,
    CONSTRAINT event_public_details_require_public CHECK (((NOT has_public_details) OR (is_public IS TRUE))),
    CONSTRAINT event_until_gt_since CHECK ((until > since))
);


--
-- Name: TABLE event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event IS '@behavior +list -connection -insert';


--
-- Name: COLUMN event.share_token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.event.share_token IS '@behavior -*';


--
-- Name: COLUMN event.series_info; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.event.series_info IS '@behavior -attribute:insert -attribute:update -attribute:filterBy -attribute:orderBy';


--
-- Name: tenant_trainer; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tenant_trainer (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    is_visible boolean DEFAULT true NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
    member_price_45min public.price DEFAULT NULL::public.price_type,
    member_payout_45min public.price DEFAULT NULL::public.price_type,
    guest_price_45min public.price DEFAULT NULL::public.price_type,
    guest_payout_45min public.price DEFAULT NULL::public.price_type,
    create_payout_payments boolean DEFAULT true NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    member_price_45min_amount numeric(19,4) GENERATED ALWAYS AS ((member_price_45min).amount) STORED,
    member_payout_45min_amount numeric(19,4) GENERATED ALWAYS AS ((member_payout_45min).amount) STORED,
    guest_price_45min_amount numeric(19,4) GENERATED ALWAYS AS ((guest_price_45min).amount) STORED,
    guest_payout_45min_amount numeric(19,4) GENERATED ALWAYS AS ((guest_payout_45min).amount) STORED,
    currency text GENERATED ALWAYS AS ((member_price_45min).currency) STORED,
    is_external boolean DEFAULT false NOT NULL,
    CONSTRAINT tenant_trainer_until_gt_since CHECK ((until > since))
);


--
-- Name: TABLE tenant_trainer; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant_trainer IS '@behavior +list +connection -query:resource:list -query:resource:connection';


--
-- Name: COLUMN tenant_trainer.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.tenant_trainer.active_range IS '@behavior -*';


--
-- Name: event_trainers_at(public.event, timestamp with time zone); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.event_trainers_at(v_instance public.event, v_at timestamp with time zone) RETURNS SETOF public.tenant_trainer
    LANGUAGE sql STABLE
    AS $$
select distinct on (tt.tenant_id, tt.person_id) tt.*
from event_trainer k
join tenant_trainer tt on tt.tenant_id = k.tenant_id and tt.person_id = k.person_id
where k.event_id = v_instance.id
  and tt.active_range @> v_at
order by tt.tenant_id, tt.person_id, lower(tt.active_range) desc;
$$;


--
-- Name: is_system_admin(bigint); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.is_system_admin(bigint) RETURNS boolean
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'app_private', 'public', 'pg_temp'
    AS $_$
  select coalesce(exists(
    select 1 from app_private.system_admin_user sau where user_id = $1
  ), false);
$_$;


--
-- Name: FUNCTION is_system_admin(bigint); Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON FUNCTION app_private.is_system_admin(bigint) IS 'Returns true when the given user id has global system administrator privileges.';


--
-- Name: normalize_name(text); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.normalize_name(text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    AS $_$
  select lower(public.unaccent('public.unaccent', $1));
$_$;


--
-- Name: person_membership_candidates(text); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.person_membership_candidates(membership_state text) RETURNS TABLE(person_id bigint, in_state boolean, cohort_ids bigint[], is_trainer boolean, is_admin boolean)
    LANGUAGE sql STABLE
    AS $$
  with current_people as (
    select person_id from current_tenant_membership
    union select person_id from current_tenant_trainer
    union select person_id from current_tenant_administrator
  ), former_people as (
    select person_id from tenant_membership where tenant_id = current_tenant_id() and status = 'expired'
    union select person_id from tenant_trainer where tenant_id = current_tenant_id() and status = 'expired'
    union select person_id from tenant_administrator where tenant_id = current_tenant_id() and status = 'expired'
  ), trainers as (
    select person_id from current_tenant_trainer where membership_state = 'current'
    union all
    select person_id from tenant_trainer
    where membership_state = 'former' and tenant_id = current_tenant_id() and status = 'expired'
  ), admins as (
    select person_id from current_tenant_administrator where membership_state = 'current'
    union all
    select person_id from tenant_administrator
    where membership_state = 'former' and tenant_id = current_tenant_id() and status = 'expired'
  ), cohorts as (
    select person_id, cohort_id from current_cohort_membership where membership_state = 'current'
    union all
    select person_id, cohort_id from cohort_membership
    where membership_state = 'former' and tenant_id = current_tenant_id() and status = 'expired'
  )
  select p.id,
    case membership_state
      when 'current' then p.id in (select person_id from current_people)
      when 'former' then p.id in (select person_id from former_people)
        and p.id not in (select person_id from current_people)
      else false
    end,
    array(select distinct cohort_id from cohorts where person_id = p.id),
    p.id in (select person_id from trainers),
    p.id in (select person_id from admins)
  from person p;
$$;


--
-- Name: queue_announcement_notifications(bigint); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.queue_announcement_notifications(in_announcement_id bigint) RETURNS void
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  v_user_ids bigint[];
begin
  if not exists (
    select from announcement
    where id = in_announcement_id and status = 'published'
  ) then
    return;
  end if;

  with role_flags as (
    select
      coalesce(bool_or(audience_role = 'member'), false) as has_member,
      coalesce(bool_or(audience_role = 'trainer'), false) as has_trainer,
      coalesce(bool_or(audience_role = 'administrator'), false) as has_administrator
    from announcement_audience
    where announcement_id = in_announcement_id
      and audience_role is not null
  ),
  role_people as (
    select distinct x.person_id
    from role_flags rf
    join lateral (
      select tm.person_id from current_tenant_membership tm where rf.has_member
      union all
      select tt.person_id from current_tenant_trainer tt where rf.has_trainer
      union all
      select ta.person_id from current_tenant_administrator ta where rf.has_administrator
    ) x on true
  ),
  role_users as (
    select distinct u.id as user_id
    from role_people rp
    join user_proxy up on up.person_id = rp.person_id and up.status = 'active'
    join users u on u.id = up.user_id
  ),
  cohort_users as (
    select distinct u.id as user_id
    from announcement_audience aa
    join current_cohort_membership cm on cm.cohort_id = aa.cohort_id
    join user_proxy up on up.person_id = cm.person_id and up.status = 'active'
    join users u on u.id = up.user_id
    where aa.announcement_id = in_announcement_id
      and aa.cohort_id is not null
  )
  select array_agg(distinct user_id order by user_id)
  into v_user_ids
  from (
    select user_id from role_users
    union
    select user_id from cohort_users
  ) recipients;

  if v_user_ids is null or array_length(v_user_ids, 1) = 0 then
    return;
  end if;

  perform graphile_worker.add_job(
    'notify_announcement',
    json_build_object(
      'announcement_id', in_announcement_id,
      'user_ids', v_user_ids
    )
  );
end;
$$;


--
-- Name: FUNCTION queue_announcement_notifications(in_announcement_id bigint); Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON FUNCTION app_private.queue_announcement_notifications(in_announcement_id bigint) IS '@omit';


--
-- Name: reconcile_event_cohort_registrations(bigint[], bigint[]); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.reconcile_event_cohort_registrations(p_event_ids bigint[], p_person_ids bigint[] DEFAULT NULL::bigint[]) RETURNS void
    LANGUAGE sql
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select instance.id from event instance
  where instance.id = any(p_event_ids) order by instance.id for update;

  -- Direct registrations (including cancellations) take precedence. Couples
  -- already supply their person rows; cohorts supply only the remaining people.
  with desired as materialized (
    select instance.id as event_id, instance.tenant_id, membership.person_id,
      min(target.cohort_id) as target_cohort_id
    from event instance
    join event_target_cohort target on target.event_id = instance.id
    join cohort_membership membership on membership.cohort_id = target.cohort_id
    where instance.id = any(p_event_ids)
      and instance.since >= now()
      and membership.status = 'active' and membership.active_range @> now()
      and (p_person_ids is null or membership.person_id = any(p_person_ids))
      and not exists (
        select 1 from event_registration registration
        where registration.event_id = instance.id
          and registration.person_id = membership.person_id
          and (
            (registration.parent_registration_id is null and registration.source is distinct from 'cohort')
            or (registration.parent_registration_id is not null and registration.registration_status = 'active')
          )
      )
    group by instance.id, instance.tenant_id, membership.person_id
  ), cancelled as (
    update event_registration registration
    set registration_status = 'cancelled'
    from event instance
    where registration.event_id = any(p_event_ids)
      and instance.id = registration.event_id and instance.since >= now()
      and registration.source = 'cohort'
      and registration.registration_status = 'active'
      and (p_person_ids is null or registration.person_id = any(p_person_ids))
      and not exists (
        select 1 from desired
        where desired.event_id = registration.event_id
          and desired.person_id = registration.person_id
      )
  )
  insert into event_registration as registration (
    tenant_id, event_id, person_id, target_cohort_id, source, status
  )
  select tenant_id, event_id, person_id, target_cohort_id, 'cohort', 'unknown'
  from desired
  on conflict (event_id, couple_id, person_id) where parent_registration_id is null
  do update set registration_status = 'active', target_cohort_id = excluded.target_cohort_id
  where registration.source = 'cohort'
    and (registration.registration_status, registration.target_cohort_id)
      is distinct from ('active'::event_registration_status, excluded.target_cohort_id);
$$;


--
-- Name: refresh_competitor_activity(text[]); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.refresh_competitor_activity(competitor_ids text[]) RETURNS void
    LANGUAGE plpgsql
    AS $$
begin
  if coalesce(cardinality(competitor_ids), 0) = 0 then
    return;
  end if;

  perform c.id from federated.competitor c where c.id = any(competitor_ids) order by c.id for no key update;

  with activity as (
    select c.id,
      (select max(comp.start_date)
       from federated.competition_result r
       join federated.competition comp on comp.id = r.competition_id
       where r.competitor_id = c.id) as latest_result_at,
      (select max(comp.start_date)
       from federated.competition_entry e
       join federated.competition comp on comp.id = e.competition_id
       where e.competitor_id = c.id and not e.cancelled
         and not exists (
           select 1 from federated.competition_result r
           where (r.competition_id, r.competitor_id) = (e.competition_id, e.competitor_id)
         )) as latest_entry_at
    from federated.competitor c
    where c.id = any(competitor_ids)
  )
  update federated.competitor c
  set latest_result_at = a.latest_result_at, latest_entry_at = a.latest_entry_at
  from activity a
  where c.id = a.id
    and (c.latest_result_at, c.latest_entry_at) is distinct from
      (a.latest_result_at, a.latest_entry_at);
end;
$$;


--
-- Name: refresh_event_manager_person_ids(bigint); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.refresh_event_manager_person_ids(p_event_id bigint) RETURNS void
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  with recursive affected as (
    select id from event where id = p_event_id
    union all
    select child.id
    from event child
    join affected parent on child.parent_id = parent.id
  ), desired as (
    select id, app_private.event_manager_person_ids(id) as person_ids
    from affected
  )
  update event instance
  set manager_person_ids = desired.person_ids
  from desired
  where instance.id = desired.id
    and instance.manager_person_ids is distinct from desired.person_ids
$$;


--
-- Name: refresh_event_series_info(bigint[]); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.refresh_event_series_info(p_series_ids bigint[]) RETURNS void
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  if coalesce(cardinality(p_series_ids), 0) = 0 then
    return;
  end if;

  perform id from event_series
  where id = any(p_series_ids) order by id for no key update;

  with actual as (
    select member.id,
      row(series.id, series.name,
        row_number() over ordered::integer,
        count(*) over whole_series::integer,
        min(member.since) over whole_series,
        max(member.until) over whole_series)::series_info as info
    from event member
    join event_series series
      on series.tenant_id = member.tenant_id and series.id = member.series_id
    where series.id = any(p_series_ids)
    window whole_series as (partition by member.tenant_id, member.series_id),
      ordered as (whole_series order by member.since, member.id)
  )
  update event member set series_info = actual.info
  from actual
  where member.id = actual.id and member.series_info is distinct from actual.info;
end;
$$;


--
-- Name: refresh_event_stats(bigint); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.refresh_event_stats(p_event_id bigint) RETURNS void
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select 1 from event where id = p_event_id for no key update;

  update event instance
  set stats = actual.stats
  from (
    select jsonb_build_object(
      'TOTAL', count(*)::int,
      'UNKNOWN', count(*) filter (where status = 'unknown')::int,
      'ATTENDED', count(*) filter (where status = 'attended')::int,
      'NOT_EXCUSED', count(*) filter (where status = 'not-excused')::int
    ) as stats
    from event_registration
    where event_id = p_event_id
      and person_id is not null
      and registration_status = 'active'
  ) actual
  where instance.id = p_event_id and instance.stats is distinct from actual.stats;
$$;


--
-- Name: relationship_status_next(timestamp with time zone, tstzrange, public.relationship_status); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.relationship_status_next(ts timestamp with time zone, range tstzrange, current public.relationship_status) RETURNS public.relationship_status
    LANGUAGE sql IMMUTABLE
    AS $$
  select case
    when ts < lower(range) then 'pending'
    when not upper_inf(range) and ts >= upper(range) then 'expired'
    when range @> ts then 'active'
    else current
  end
$$;


--
-- Name: current_user_id(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_user_id() RETURNS bigint
    LANGUAGE sql STABLE
    AS $$
  SELECT nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$;


--
-- Name: FUNCTION current_user_id(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.current_user_id() IS '@behavior -*';


--
-- Name: event_lesson_request; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_lesson_request (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    trainer_id bigint NOT NULL,
    registration_id bigint NOT NULL,
    lesson_count integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_lesson_request_lesson_count_check CHECK ((lesson_count > 0))
);


--
-- Name: TABLE event_lesson_request; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_lesson_request IS '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';


--
-- Name: COLUMN event_lesson_request.registration_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.event_lesson_request.registration_id IS '@hasDefault';


--
-- Name: event_registration; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_registration (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    parent_registration_id bigint,
    couple_id bigint,
    person_id bigint,
    target_cohort_id bigint,
    status public.attendance_type,
    note text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    attendance_created_at timestamp with time zone,
    attendance_updated_at timestamp with time zone,
    attendance_note text,
    registration_status public.event_registration_status DEFAULT 'active'::public.event_registration_status NOT NULL,
    source public.event_registration_source,
    created_by bigint DEFAULT public.current_user_id(),
    CONSTRAINT event_registration_attendance_state CHECK (((person_id IS NULL) OR ((status IS NOT NULL) AND (attendance_created_at IS NOT NULL) AND (attendance_updated_at IS NOT NULL)))),
    CONSTRAINT event_registration_shape CHECK (
CASE
    WHEN (parent_registration_id IS NULL) THEN (((couple_id IS NOT NULL) AND (person_id IS NULL)) OR ((couple_id IS NULL) AND (person_id IS NOT NULL)))
    ELSE ((person_id IS NOT NULL) AND (couple_id IS NULL))
END)
);


--
-- Name: TABLE event_registration; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_registration IS '@behavior +list +connection -insert -update -delete';


--
-- Name: COLUMN event_registration.created_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.event_registration.created_by IS '@behavior -*';


--
-- Name: set_lesson_request(public.event_registration, bigint, integer); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.set_lesson_request(registration public.event_registration, trainer_id bigint, lesson_count integer) RETURNS public.event_lesson_request
    LANGUAGE plpgsql STRICT
    AS $_$
declare
  trainer event_trainer;
  lesson_request event_lesson_request;
  other_lessons bigint;
begin
  select * into trainer
  from event_trainer
  where id = $2 and event_id = registration.event_id and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'TRAINER_NOT_FOUND' using errcode = '28000';
  end if;

  if $3 = 0 then
    delete from event_lesson_request eld
    where eld.registration_id = registration.id and eld.trainer_id = $2;

    return null;
  end if;

  if trainer.lessons_offered = 0 then
    raise exception 'LESSONS_NOT_OFFERED' using errcode = '28000';
  end if;

  if trainer.lessons_offered is not null then
    select coalesce(sum(eld.lesson_count), 0) into other_lessons
    from event_lesson_request eld
    join event_registration other_registration
      on other_registration.id = eld.registration_id
     and other_registration.registration_status = 'active'
    where eld.trainer_id = $2 and eld.registration_id <> registration.id;

    if $3 > trainer.lessons_offered - other_lessons then
      raise exception 'LESSON_LIMIT_EXCEEDED' using errcode = '22023';
    end if;
  end if;

  insert into event_lesson_request (registration_id, trainer_id, lesson_count)
  values (registration.id, $2, $3)
  on conflict on constraint event_lesson_request_registration_id_trainer_id_key do update set lesson_count = $3
  returning * into lesson_request;

  return lesson_request;
end;
$_$;


--
-- Name: tg__timestamps(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg__timestamps() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare
  ignored text[];
begin
  if tg_op = 'UPDATE' then
    -- Don't bump updated_at for no-op updates
    if new is not distinct from old then
      return new;
    end if;

    select coalesce(array_agg(attribute.attname::text order by attribute.attnum), '{}') || tg_argv
      into ignored
    from pg_catalog.pg_attribute as attribute
    where attribute.attrelid = tg_relid
      and attribute.attnum > 0
      and not attribute.attisdropped
      and attribute.attgenerated <> '';

    -- Ignore generated columns and any independent state named by the trigger
    if ignored <> '{}' and to_jsonb(new) - ignored is not distinct from to_jsonb(old) - ignored then
      return new;
    end if;
  end if;

  new.created_at = case when tg_op = 'INSERT' then now() else old.created_at end;
  new.updated_at = now();
  return new;
end;
$$;


--
-- Name: FUNCTION tg__timestamps(); Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON FUNCTION app_private.tg__timestamps() IS 'This trigger should be called on all tables with created_at, updated_at - it ensures that they cannot be manipulated and that updated_at will always be larger than the previous updated_at.';


--
-- Name: tg_account_balances__update(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_account_balances__update() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
BEGIN
  PERFORM graphile_worker.add_job(
    'refresh_account_balances',
    job_key := 'refresh_account_balances'
  );
  return null;
END
$$;


--
-- Name: tg_aktuality__author(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_aktuality__author() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  NEW.at_kdo = current_user_id();
  RETURN NEW;
END;
$$;


--
-- Name: tg_aktuality__sync_attachments(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_aktuality__sync_attachments() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare
  file_ids bigint[];
begin
  select coalesce(array_agg(distinct file.id), '{}'::bigint[])
  into file_ids
  from regexp_matches(
    concat_ws(' ', new.at_preview, new.at_text, new.title_photo_url),
    '/f/([0-9]+)/',
    'g'
  ) match(parts)
  join file on file.id::text = match.parts[1]
    and file.tenant_id = new.tenant_id
    and file.uploaded_at is not null;

  delete from article_attachment
  where aktuality_id = new.id
    and inline
    and file_id <> all(file_ids);

  insert into article_attachment (tenant_id, aktuality_id, file_id, inline)
  select new.tenant_id, new.id, input.file_id, true
  from unnest(file_ids) input(file_id)
  on conflict (tenant_id, aktuality_id, file_id) do nothing;

  return new;
end;
$$;


--
-- Name: FUNCTION tg_aktuality__sync_attachments(); Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON FUNCTION app_private.tg_aktuality__sync_attachments() IS 'Synchronizes inline file references from article content.';


--
-- Name: tg_announcement__after_write(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_announcement__after_write() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
-- @plpgsql_check_options: oldtable = oldtable, newtable = newtable
declare
  rec record;
begin
  for rec in
    select * from newtable
  loop
    if rec.status = 'published' then
      if TG_OP = 'INSERT' then
        perform app_private.queue_announcement_notifications(rec.id);
      elsif not exists (select from oldtable where id = rec.id and status = 'published') then
        perform app_private.queue_announcement_notifications(rec.id);
      end if;
    end if;
  end loop;

  return null;
end;
$$;


--
-- Name: tg_announcement__author(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_announcement__author() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if new.author_id is null then
    new.author_id = current_user_id();
  end if;
  return new;
end;
$$;


--
-- Name: tg_announcement__status(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_announcement__status() RETURNS trigger
    LANGUAGE plpgsql
    SET search_path TO 'pg_catalog', 'public', 'app_private'
    AS $$
begin
  new.status = app_private.announcement_status_next(
    now(), new.scheduled_since, new.scheduled_until, new.status
  );
  return new;
end;
$$;


--
-- Name: tg_announcement__sync_attachments(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_announcement__sync_attachments() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare
  file_ids bigint[];
begin
  select coalesce(array_agg(distinct file.id), '{}'::bigint[])
  into file_ids
  from regexp_matches(coalesce(new.body, ''), '/f/([0-9]+)/', 'g') match(parts)
  join file on file.id::text = match.parts[1]
    and file.tenant_id = new.tenant_id
    and file.uploaded_at is not null;

  delete from announcement_attachment
  where announcement_id = new.id
    and inline
    and file_id <> all(file_ids);

  insert into announcement_attachment (tenant_id, announcement_id, file_id, inline)
  select new.tenant_id, new.id, input.file_id, true
  from unnest(file_ids) input(file_id)
  on conflict (tenant_id, announcement_id, file_id) do nothing;

  return new;
end;
$$;


--
-- Name: FUNCTION tg_announcement__sync_attachments(); Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON FUNCTION app_private.tg_announcement__sync_attachments() IS 'Synchronizes inline file references from announcement content.';


--
-- Name: tg_announcement_audience__after_write(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_announcement_audience__after_write() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
-- @plpgsql_check_options: oldtable = oldtable, newtable = newtable
declare
  rec record;
begin
  if TG_OP = 'DELETE' then
    for rec in (select distinct announcement_id from oldtable) loop
      perform app_private.queue_announcement_notifications(rec.announcement_id);
    end loop;
  else
    for rec in (select distinct announcement_id from newtable) loop
      perform app_private.queue_announcement_notifications(rec.announcement_id);
    end loop;
  end if;
  return null;
end;
$$;


--
-- Name: tg_cohort_membership__on_status(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_cohort_membership__on_status() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'app_private', 'pg_temp'
    AS $$
declare
  event_ids bigint[];
  person_ids bigint[];
begin
  select array_agg(distinct target.event_id)
  into event_ids
  from event_target_cohort target
  join event instance on instance.id = target.event_id
  where target.cohort_id = any (
    case when tg_op = 'INSERT' then array[new.cohort_id]
      when tg_op = 'DELETE' then array[old.cohort_id]
      else array[old.cohort_id, new.cohort_id]
    end
  ) and instance.since >= now();

  if event_ids is not null then
    person_ids := case when tg_op = 'INSERT' then array[new.person_id]
      when tg_op = 'DELETE' then array[old.person_id]
      else array[old.person_id, new.person_id]
    end;
    perform app_private.reconcile_event_cohort_registrations(event_ids, person_ids);
  end if;

  return case when tg_op = 'DELETE' then old else new end;
end;
$$;


--
-- Name: tg_competition_dates__refresh_competitor_activity(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_competition_dates__refresh_competitor_activity() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare
  competitor_ids text[];
begin
  with changed as (
    select n.id from newtable n join oldtable o using (id)
    where n.start_date is distinct from o.start_date
  ), affected as (
    select r.competitor_id from federated.competition_result r join changed c on c.id = r.competition_id
    union
    select e.competitor_id from federated.competition_entry e join changed c on c.id = e.competition_id
  )
  select array_agg(competitor_id) into competitor_ids from affected;
  perform app_private.refresh_competitor_activity(competitor_ids);
  return null;
end;
$$;


--
-- Name: tg_competitor_activity__refresh(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_competitor_activity__refresh() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
-- @plpgsql_check_options: oldtable = oldtable, newtable = newtable
declare
  competitor_ids text[];
begin
  if TG_OP = 'INSERT' then
    select array_agg(distinct competitor_id) into competitor_ids from newtable;
  elsif TG_OP = 'DELETE' then
    select array_agg(distinct competitor_id) into competitor_ids from oldtable;
  else
    select array_agg(competitor_id) into competitor_ids
    from (select competitor_id from oldtable union select competitor_id from newtable) affected;
  end if;
  perform app_private.refresh_competitor_activity(competitor_ids);
  return null;
end;
$$;


--
-- Name: tg_event__delete_payment_on_cancellation(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event__delete_payment_on_cancellation() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  payment_id bigint;
begin
  delete from payment where event_id = OLD.id;

  if not new.is_cancelled then
    select (create_event_payment(i)).id into payment_id
    from event i
    where i.type='lesson'
      and i.id = NEW.id
      and i.since < now()
      and not i.is_cancelled
      and not exists (
        select * from payment where event_id = i.id
      );

    update payment set status ='unpaid' where id = payment_id;
    perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
  end if;

  return OLD;
end;
$$;


--
-- Name: tg_event__refresh_manager_person_ids(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event__refresh_manager_person_ids() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  perform app_private.refresh_event_manager_person_ids(new.id);
  return null;
end;
$$;


--
-- Name: tg_event__refresh_series_info(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event__refresh_series_info() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
declare
  series_ids bigint[];
begin
  if tg_op = 'INSERT' then
    select array_agg(distinct series_id) into series_ids
    from changed_rows where series_id is not null;
  elsif tg_op = 'DELETE' then
    select array_agg(distinct series_id) into series_ids
    from deleted_rows where series_id is not null;
  else
    select array_agg(distinct affected.series_id) into series_ids
    from deleted_rows old_row full join changed_rows new_row using (id)
    cross join lateral (values (old_row.series_id), (new_row.series_id)) affected(series_id)
    where row(old_row.id, old_row.tenant_id, old_row.series_id, old_row.since, old_row.until)
      is distinct from
      row(new_row.id, new_row.tenant_id, new_row.series_id, new_row.since, new_row.until)
      and affected.series_id is not null;
  end if;

  if tg_op <> 'DELETE' then
    if exists (select 1 from changed_rows
      where series_id is null and series_info is distinct from null) then
      update event member set series_info = null
      from changed_rows changed
      where member.id = changed.id and changed.series_id is null
        and member.series_info is distinct from null;
    end if;
  end if;

  perform app_private.refresh_event_series_info(series_ids);
  return null;
end;
$$;


--
-- Name: tg_event__validate_parent(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event__validate_parent() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  if new.parent_id is null then return new; end if;

  perform 1 from event parent where parent.id = new.parent_id for no key update;

  if not found or exists (
    with recursive ancestors as (
      select id, parent_id
      from event where id = new.parent_id
      union
      select parent.id, parent.parent_id
      from event parent
      join ancestors child on parent.id = child.parent_id
    )
    select 1 from ancestors where id = new.id
  ) then
    raise exception 'Event instance parent must not create a cycle'
      using errcode = '23514';
  end if;

  return new;
end;
$$;


--
-- Name: tg_event_people__refresh(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event_people__refresh() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  perform graphile_worker.add_job(
    'refresh_event_people',
    queue_name := 'refresh_event_people',
    job_key := 'refresh_event_people'
  );
  return null;
end;
$$;


--
-- Name: tg_event_registration__attendance_timestamps(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event_registration__attendance_timestamps() RETURNS trigger
    LANGUAGE plpgsql
    SET search_path TO 'pg_catalog', 'pg_temp'
    AS $$
begin
  if new.person_id is null then
    new.attendance_created_at := null;
    new.attendance_updated_at := null;
  elsif tg_op = 'INSERT' or new.person_id is distinct from old.person_id then
    new.attendance_created_at := new.created_at;
    new.attendance_updated_at := new.updated_at;
  else
    new.attendance_created_at := old.attendance_created_at;
    new.attendance_updated_at := case
      when new.status is not distinct from old.status
        and new.attendance_note is not distinct from old.attendance_note
        then old.attendance_updated_at
      when old.attendance_updated_at >= new.updated_at
        then old.attendance_updated_at + interval '1 millisecond'
      else new.updated_at
    end;
  end if;
  return new;
end;
$$;


--
-- Name: tg_event_registration__refresh_stats_stmt(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event_registration__refresh_stats_stmt() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'pg_temp'
    AS $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
begin
  if tg_op = 'INSERT' then
    perform app_private.refresh_event_stats(affected.event_id)
    from (
      select distinct event_id from changed_rows
      where person_id is not null order by event_id
    ) affected;
  elsif tg_op = 'DELETE' then
    perform app_private.refresh_event_stats(affected.event_id)
    from (
      select distinct event_id from deleted_rows
      where person_id is not null order by event_id
    ) affected;
  else
    perform app_private.refresh_event_stats(affected.event_id)
    from (
      select distinct affected.event_id
      from deleted_rows old_row
      join changed_rows new_row using (id)
      cross join lateral (values
        (old_row.event_id, old_row.person_id),
        (new_row.event_id, new_row.person_id)
      ) affected(event_id, person_id)
      where row(old_row.event_id, old_row.person_id, old_row.status, old_row.registration_status)
        is distinct from
        row(new_row.event_id, new_row.person_id, new_row.status, new_row.registration_status)
        and affected.person_id is not null
      order by affected.event_id
    ) affected;
  end if;
  return null;
end;
$$;


--
-- Name: tg_event_registration__sync_lesson_payment(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event_registration__sync_lesson_payment() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  affected_event_id bigint;
  instance event;
  payment_id bigint;
begin
  for affected_event_id in select distinct unnest(array[old.event_id, new.event_id]) loop
    select * into instance from event where id = affected_event_id for no key update;
    if not found then continue; end if;

    if (
      not exists (select 1 from payment where event_id = instance.id)
      or array(select d.person_id from payment_debtor d join payment p on p.id = d.payment_id
          where p.event_id = instance.id order by d.person_id)
        = array(select distinct r.person_id from event_registration r
          where r.event_id = instance.id and r.registration_status = 'active'
            and r.person_id is not null and not instance.is_cancelled order by r.person_id)
    ) then
      continue;
    end if;

    delete from payment where event_id = instance.id;
    if not instance.is_cancelled and instance.type = 'lesson' and instance.since < now() then
      payment_id := (create_event_payment(instance)).id;
      update payment set status = 'unpaid' where id = payment_id;
      perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
    end if;
  end loop;
  return null;
end;
$$;


--
-- Name: tg_event_series__refresh_info(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event_series__refresh_info() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
declare
  series_ids bigint[];
begin
  select array_agg(changed.id) into series_ids
  from changed_rows changed join deleted_rows previous using (id)
  where changed.name is distinct from previous.name;
  perform app_private.refresh_event_series_info(series_ids);
  return null;
end;
$$;


--
-- Name: tg_event_trainer__refresh_manager_person_ids(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event_trainer__refresh_manager_person_ids() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if tg_op = 'DELETE' then
    perform app_private.refresh_event_manager_person_ids(old.event_id);
  elsif tg_op = 'INSERT' then
    perform app_private.refresh_event_manager_person_ids(new.event_id);
  elsif old.event_id is distinct from new.event_id
    or old.person_id is distinct from new.person_id then
    perform app_private.refresh_event_manager_person_ids(old.event_id);
    perform app_private.refresh_event_manager_person_ids(new.event_id);
  end if;
  return null;
end;
$$;


--
-- Name: tg_file__delete(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_file__delete() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  perform graphile_worker.add_job(
    'delete_file',
    json_build_object('object_key', old.object_key)
  );
  return old;
end;
$$;


--
-- Name: tg_payment__fill_accounting_period(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_payment__fill_accounting_period() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  v_now timestamptz := current_timestamp;
  since timestamptz;
begin
  if NEW.tenant_id is null then
    raise exception 'payment.tenant_id must be set before tg_payment__fill_accounting_period';
  end if;

  if NEW.accounting_period_id is null then
    select id into NEW.accounting_period_id
    from accounting_period
    where tenant_id = NEW.tenant_id and range @> v_now
    order by lower(range) desc
    limit 1;

    if not found then
      since := case
        when extract(month from v_now) >= 9 then date_trunc('year', v_now) + interval '8 months'
        else date_trunc('year', v_now) + interval '8 months' - interval '1 year'
      end;

      begin
        insert into accounting_period (tenant_id, name, since, until)
        values (
          NEW.tenant_id,
          'Školní rok ' || extract(year from since),
          since,
          since + interval '12 months'
        )
        returning id into NEW.accounting_period_id;
      exception
        when exclusion_violation or unique_violation then
          select id into NEW.accounting_period_id
          from accounting_period
          where tenant_id = NEW.tenant_id and range @> v_now
          order by lower(range) desc
          limit 1;

          if not found then
            raise exception 'Failed to create/find accounting_period for tenant % at %', NEW.tenant_id, v_now;
          end if;
      end;
    end if;
  end if;

  return NEW;
end
$$;


--
-- Name: tg_person_invitation__send(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_person_invitation__send() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  perform graphile_worker.add_job('send_invitation', json_build_object('id', NEW.id));
  return NEW;
end;
$$;


--
-- Name: tg_relationship__status(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_relationship__status() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  new.status := app_private.relationship_status_next(
    now(), tstzrange(new.since, new.until, '[)'), new.status
  );
  return new;
end;
$$;


--
-- Name: tg_resolve_age_group(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_resolve_age_group() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  new.age_group_code := federated.resolve_age_group(new.age_group);
  return new;
end;
$$;


--
-- Name: FUNCTION tg_resolve_age_group(); Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON FUNCTION app_private.tg_resolve_age_group() IS '@behavior -*';


--
-- Name: tg_security_event__credential(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_security_event__credential() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (person_id, kind, method)
      values (old.person_id, 'access_credential_ended', 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (person_id, kind, method, effective_at)
    values (new.person_id, 'access_credential_issued', 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (person_id, kind, method, effective_at)
    values (
      new.person_id,
      (case when new.status = 'active' then 'access_credential_issued' else 'access_credential_ended' end)::security_event_kind,
      (case when current_user_id() is null then 'scheduled' else 'manual' end)::security_event_method,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;


--
-- Name: tg_security_event__password(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_security_event__password() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  insert into security_event (user_id, kind, method)
  values (new.id, 'password_changed', 'manual');
  return new;
end;
$$;


--
-- Name: tg_security_event__range(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_security_event__range() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (person_id, kind, method)
      values (old.person_id, tg_argv[1]::security_event_kind, 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (person_id, kind, method, effective_at)
    values (new.person_id, tg_argv[0]::security_event_kind, 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (person_id, kind, method, effective_at)
    values (
      new.person_id,
      (case when new.status = 'active' then tg_argv[0] else tg_argv[1] end)::security_event_kind,
      (case when current_user_id() is null then 'scheduled' else 'manual' end)::security_event_method,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;


--
-- Name: tg_security_event__user_proxy(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_security_event__user_proxy() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (user_id, person_id, kind, method)
      values (old.user_id, old.person_id, 'person_unlinked', 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (user_id, person_id, kind, method, effective_at)
    values (new.user_id, new.person_id, 'person_linked', 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (user_id, person_id, kind, method, effective_at)
    values (
      new.user_id,
      new.person_id,
      (case when new.status = 'active' then 'person_linked' else 'person_unlinked' end)::security_event_kind,
      (case when current_user_id() is null then 'scheduled' else 'manual' end)::security_event_method,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;


--
-- Name: tg_tenant_membership__on_status(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_tenant_membership__on_status() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if new.status = 'expired' and not exists (
    select from tenant_membership
    where id <> new.id
      and tenant_id = new.tenant_id
      and person_id = new.person_id
      and active_range @> new.until
  ) then
    update cohort_membership
    set status = 'expired', until = new.until
    where tenant_id = new.tenant_id
      and person_id = new.person_id
      and since < new.until
      and (until is null or until > new.until);
  end if;
  return new;
end;
$$;


--
-- Name: tg_transaction__effective_date(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_transaction__effective_date() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  if NEW.effective_date is null then
    NEW.effective_date = (select coalesce(
      (select since from payment join event on event_id = event.id where NEW.payment_id = payment.id),
      (select due_at from payment where NEW.payment_id = payment.id),
      NEW.created_at
    ));
  end if;
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
  if length(new.u_pass) <> 40 then
    v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');
    new.u_pass := encode(digest(v_salt || new.u_pass || v_salt, 'sha1'), 'hex');
  end if;
  return new;
end;
$$;


--
-- Name: tg_users__trim_login(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_users__trim_login() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  new.u_login := trim(new.u_login);
  return new;
end;
$$;


--
-- Name: tstzrange_overlaps_internal(tstzrange, anyrange); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tstzrange_overlaps_internal(tstzrange, anyrange) RETURNS boolean
    LANGUAGE internal IMMUTABLE STRICT LEAKPROOF PARALLEL SAFE
    AS $$range_overlaps$$;


--
-- Name: &&; Type: OPERATOR; Schema: app_private; Owner: -
--

CREATE OPERATOR app_private.&& (
    FUNCTION = app_private.tstzrange_overlaps_internal,
    LEFTARG = tstzrange,
    RIGHTARG = anyrange,
    RESTRICT = matchingsel,
    JOIN = matchingjoinsel
);


--
-- Name: tstzrange_overlaps(tstzrange, tstzrange); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tstzrange_overlaps(tstzrange, tstzrange) RETURNS boolean
    LANGUAGE sql IMMUTABLE STRICT LEAKPROOF PARALLEL SAFE
    RETURN ($1 OPERATOR(app_private.&&) $2);


--
-- Name: visible_announcement_ids(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.visible_announcement_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  with eligible as (
    select announcement.id
    from announcement
    where pg_catalog.pg_has_role(pg_catalog.current_setting('role'), 'member', 'member')
      and announcement.tenant_id = (select current_tenant_id())
      and announcement.status in ('published', 'archived')
  )
  select eligible.id
  from eligible
  where not exists (
    select from announcement_audience where announcement_id = eligible.id
  )
  union all
  select announcement_id
  from announcement_audience join eligible on eligible.id = announcement_id
  where (
    cohort_id in (
      select cohort_id from current_cohort_membership where person_id = any ((select current_person_ids())::bigint[])
    )
    or audience_role = 'member' and exists (
      select from current_tenant_membership where person_id = any ((select current_person_ids())::bigint[])
    )
    or audience_role = 'trainer' and exists (
      select from current_tenant_trainer where person_id = any ((select current_person_ids())::bigint[])
    )
    or audience_role = 'administrator' and exists (
      select from current_tenant_administrator where person_id = any ((select current_person_ids())::bigint[])
    )
  );
$$;


--
-- Name: visible_file_ids(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.visible_file_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  -- Start with cover candidates rather than probing every accessible event.
  with event_covers as materialized (
    select id, cover_image_id from event
    where tenant_id = (select current_tenant_id())
      and cover_image_id is not null
  )
  select id
  from file
  where tenant_id = (select current_tenant_id())
    and is_public

  union all

  select f.file_id
  from announcement_attachment f
  join app_private.visible_announcement_ids() a(id) on a.id = f.announcement_id
  where f.tenant_id = (select current_tenant_id())

  union all

  select f.file_id
  from article_attachment f
  join aktuality a on id = f.aktuality_id and a.tenant_id = f.tenant_id
  where a.tenant_id = (select current_tenant_id())
    and a.is_visible

  union all

  select image.file_id
  from location_image image
  join location
    on location.tenant_id = image.tenant_id
    and location.id = image.location_id
  where image.tenant_id = (select current_tenant_id())

  union all

  select cover_image_id
  from location
  where tenant_id = (select current_tenant_id())
    and cover_image_id is not null

  union all

  select cover_image_id from event_covers
  where id = any (array(select app_private.accessible_event_ids()));
$$;


--
-- Name: visible_payment_ids(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.visible_payment_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select payment_id from public.payment_debtor
  where tenant_id = (select current_tenant_id())
    and person_id = any ((select current_person_ids())::bigint[])
  union
  select payment_id from public.payment_recipient
  where tenant_id = (select current_tenant_id())
    and account_id = any (array(select current_account_ids()));
$$;


--
-- Name: visible_person_ids(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.visible_person_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE SECURITY DEFINER LEAKPROOF PARALLEL SAFE
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select unnest(public.current_person_ids()) union all
  -- always visible: trainers + admins in current tenant
  select person_id from public.current_tenant_trainer
  union all
  select person_id from public.current_tenant_administrator
  union all
  -- members visible only if viewer is in tenant (any role)
  select person_id from public.current_tenant_membership
  where (select public.current_tenant_id() = any(public.my_tenants_array()))
  union all
  select visible_person_id from app_private.event_people
  where tenant_id = (select public.current_tenant_id())
    and viewer_person_id = any((select public.current_person_ids())::bigint[]);
$$;


--
-- Name: visible_user_proxy_ids(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.visible_user_proxy_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select id from user_proxy
  where status = 'active'
    and person_id in (
      select person_id from user_proxy
      where user_id = (select current_user_id()) and status = 'active'
    );
$$;


--
-- Name: reserve_request(text); Type: FUNCTION; Schema: crawler; Owner: -
--

CREATE FUNCTION crawler.reserve_request(in_host text, OUT granted boolean, OUT allowed_at timestamp with time zone) RETURNS record
    LANGUAGE plpgsql
    AS $$
DECLARE
  r crawler.rate_limit_rule;
BEGIN
  SELECT *
  INTO r
  FROM crawler.rate_limit_rule
  WHERE host = in_host
  FOR UPDATE;

  IF NOT FOUND THEN
    granted := true;
    allowed_at := now();
    RETURN;
  END IF;

  IF r.next_available_at <= now() THEN
    UPDATE crawler.rate_limit_rule
       SET next_available_at = now() + r.spacing
     WHERE host = in_host;

    granted := true;
    allowed_at := now();
    RETURN;
  ELSE
    granted := false;
    allowed_at := (
      SELECT GREATEST(r.next_available_at, COALESCE(MAX(run_at), now()) + r.spacing)
      FROM graphile_worker.jobs
      WHERE key LIKE 'fetch:' || r.host || ':%'
    );
    RETURN;
  END IF;
END;
$$;


--
-- Name: FUNCTION reserve_request(in_host text, OUT granted boolean, OUT allowed_at timestamp with time zone); Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON FUNCTION crawler.reserve_request(in_host text, OUT granted boolean, OUT allowed_at timestamp with time zone) IS '@behavior -*';


--
-- Name: store_response(bigint, text, integer, text, jsonb, crawler.fetch_status); Type: FUNCTION; Schema: crawler; Owner: -
--

CREATE FUNCTION crawler.store_response(p_frontier_id bigint, p_url text, p_http_status integer, p_error text, p_content jsonb, p_fetch_status crawler.fetch_status) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
  previous crawler.json_response;
  observed_at timestamptz;
  payload_hash text;
  response_id bigint;
begin
  perform 1 from crawler.frontier where id = p_frontier_id for update;
  if not found then
    raise exception 'Crawler frontier % does not exist', p_frontier_id;
  end if;

  select r.* into previous
  from crawler.frontier f
  join crawler.json_response r on r.id = f.last_response_id
  where f.id = p_frontier_id;

  observed_at := clock_timestamp();
  payload_hash := encode(digest(p_content::text, 'sha256'), 'hex');

  if previous.id is not null and
    row(previous.url, previous.http_status, previous.error, previous.content_hash)
      is not distinct from row(p_url, p_http_status, p_error, payload_hash)
  then
    update crawler.json_response
    set sys_period = coalesce(sys_period, tstzrange(fetched_at, null, '[)')),
        last_observed_at = observed_at,
        observation_count = observation_count + 1
    where id = previous.id;
    response_id := previous.id;
  else
    if previous.id is not null then
      update crawler.json_response
      set sys_period = tstzrange(fetched_at, observed_at, '[)'),
          last_observed_at = coalesce(last_observed_at, fetched_at)
      where id = previous.id;
    end if;

    if p_content is not null then
      insert into crawler.json_response_cache (content) values (p_content)
      on conflict (content_hash) do nothing;
    end if;

    insert into crawler.json_response (
      frontier_id, url, http_status, error, content_hash,
      fetched_at, sys_period, last_observed_at
    ) values (
      p_frontier_id, p_url, p_http_status, p_error, payload_hash,
      observed_at, tstzrange(observed_at, null, '[)'), observed_at
    ) returning id into response_id;
  end if;

  update crawler.frontier
  set last_response_id = response_id,
      last_successful_response_id = case when p_fetch_status = 'ok'
        then response_id else last_successful_response_id end
  where id = p_frontier_id;

  return response_id;
end;
$$;


--
-- Name: FUNCTION store_response(p_frontier_id bigint, p_url text, p_http_status integer, p_error text, p_content jsonb, p_fetch_status crawler.fetch_status); Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON FUNCTION crawler.store_response(p_frontier_id bigint, p_url text, p_http_status integer, p_error text, p_content jsonb, p_fetch_status crawler.fetch_status) IS '@behavior -*';


--
-- Name: class_rank(text); Type: FUNCTION; Schema: federated; Owner: -
--

CREATE FUNCTION federated.class_rank(class text) RETURNS integer
    LANGUAGE sql IMMUTABLE
    AS $$
  select case class
    when 'M' then 16
    when 'S' then 16
    when 'A' then 15
    when 'B' then 14
    when 'C' then 13
    when 'D' then 12
    when 'E' then 11
    when 'Gold' then 4
    when 'Silver' then 3
    when 'Bronze' then 2
    when 'Novice' then 1
    else 0
  end;
$$;


--
-- Name: FUNCTION class_rank(class text); Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON FUNCTION federated.class_rank(class text) IS '@behavior -*';


--
-- Name: federation; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.federation (
    code text NOT NULL,
    name text NOT NULL
);


--
-- Name: TABLE federation; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.federation IS '@name federation
@behavior -insert -update -delete +list -connection';


--
-- Name: federation_competitor_facets(federated.federation, text, federated.competitor_type[], text[], text[], text[], date); Type: FUNCTION; Schema: federated; Owner: -
--

CREATE FUNCTION federated.federation_competitor_facets(federation federated.federation, search text DEFAULT NULL::text, competitor_types federated.competitor_type[] DEFAULT NULL::federated.competitor_type[], age_groups text[] DEFAULT NULL::text[], classes text[] DEFAULT NULL::text[], disciplines text[] DEFAULT NULL::text[], competed_since date DEFAULT NULL::date) RETURNS TABLE(facet text, value text, count integer)
    LANGUAGE sql STABLE
    AS $$
  with candidates as materialized (
    select c.id, c.competitor_type::text as competitor_type,
      p.age_group_code, p.class, p.discipline,
      coalesce(cardinality(competitor_types), 0) = 0 or c.competitor_type = any(competitor_types) as type_matches,
      -- Missing progress matches only category filters that are absent.
      coalesce(p.age_matches, coalesce(cardinality(age_groups), 0) = 0) as age_matches,
      coalesce(p.class_matches, coalesce(cardinality(classes), 0) = 0) as class_matches,
      coalesce(p.discipline_matches, coalesce(cardinality(disciplines), 0) = 0) as discipline_matches
    from federated.competitor c
    left join app_private.competitor_category_candidates(age_groups, classes, disciplines) p
      on p.competitor_id = c.id
    where c.federation = federation.code
      and (nullif(trim(search), '') is null or c.external_id::text = trim(search) or c.id = trim(search)
        or c.search_name like '%' || replace(replace(replace(
          app_private.normalize_name(trim(search)), '!', '!!'), '%', '!%'), '_', '!_') || '%' escape '!')
      and (competed_since is null or (c.latest_result_at >= competed_since
        and (c.latest_result_at <= current_date or exists (
          select 1 from federated.competition_result r
          join federated.competition comp on comp.id = r.competition_id
          where r.competitor_id = c.id and comp.start_date >= competed_since and comp.start_date <= current_date
        ))))
  )
  select option.facet, option.value, count(distinct c.id)::integer
  from candidates c
  cross join lateral (values
    ('type', c.competitor_type, c.age_matches and c.class_matches and c.discipline_matches),
    ('ageGroup', c.age_group_code, c.type_matches and c.class_matches and c.discipline_matches),
    ('class', c.class, c.type_matches and c.age_matches and c.discipline_matches),
    ('discipline', c.discipline, c.type_matches and c.age_matches and c.class_matches)
  ) option(facet, value, matches)
  where option.matches and option.value is not null
  group by option.facet, option.value
  order by option.facet, option.value;
$$;


--
-- Name: FUNCTION federation_competitor_facets(federation federated.federation, search text, competitor_types federated.competitor_type[], age_groups text[], classes text[], disciplines text[], competed_since date); Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON FUNCTION federated.federation_competitor_facets(federation federated.federation, search text, competitor_types federated.competitor_type[], age_groups text[], classes text[], disciplines text[], competed_since date) IS '@name federation_competitor_facets
@behavior +list -connection';


--
-- Name: federation_event_facets(federated.federation, boolean); Type: FUNCTION; Schema: federated; Owner: -
--

CREATE FUNCTION federated.federation_event_facets(federation federated.federation, include_cancelled boolean DEFAULT true) RETURNS federated.federation_event_facets
    LANGUAGE sql STABLE
    AS $$
  select row(array(
    select row(
      extract(year from e.start_date)::integer,
      count(*) filter (where include_cancelled is distinct from false or e.status <> 'cancelled')::integer,
      count(*) filter (
        where (include_cancelled is distinct from false or e.status <> 'cancelled')
          and e.venue_lat is not null and e.venue_lng is not null
      )::integer
    )::federated.federation_event_year
    from federated.event e
    where e.federation = federation.code
    group by extract(year from e.start_date)
    order by extract(year from e.start_date) desc
  ))::federated.federation_event_facets;
$$;


--
-- Name: FUNCTION federation_event_facets(federation federated.federation, include_cancelled boolean); Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON FUNCTION federated.federation_event_facets(federation federated.federation, include_cancelled boolean) IS '@name federation_event_facets';


--
-- Name: federation_people_facets(federated.federation, text, text[], text[], text[]); Type: FUNCTION; Schema: federated; Owner: -
--

CREATE FUNCTION federated.federation_people_facets(federation federated.federation, search text DEFAULT NULL::text, age_groups text[] DEFAULT NULL::text[], genders text[] DEFAULT NULL::text[], nationalities text[] DEFAULT NULL::text[]) RETURNS TABLE(facet text, value text, count integer)
    LANGUAGE sql STABLE
    AS $$
  with candidates as (
    select
      coalesce(p.age_group_code, nullif(p.age_group, ''), 'unknown') as age_group,
      coalesce(p.gender::text, 'unknown') as gender,
      coalesce(nullif(p.nationality, ''), 'unknown') as nationality
    from federated.person p
    where p.federation = federation.code
      and (nullif(trim(search), '') is null or p.external_id::text = trim(search) or p.id = trim(search)
        or p.search_name like '%' || replace(replace(replace(
          app_private.normalize_name(trim(search)), '!', '!!'), '%', '!%'), '_', '!_') || '%' escape '!')
  ), matches as (
    select *,
      coalesce(cardinality(age_groups), 0) = 0 or age_group = any(age_groups) as age_matches,
      coalesce(cardinality(genders), 0) = 0 or gender = any(genders) as gender_matches,
      coalesce(cardinality(nationalities), 0) = 0 or nationality = any(nationalities) as nationality_matches
    from candidates
  )
  select option.facet, option.value, count(*)::integer
  from matches m
  cross join lateral (values
    ('ageGroup', m.age_group, m.gender_matches and m.nationality_matches),
    ('gender', m.gender, m.age_matches and m.nationality_matches),
    ('nationality', m.nationality, m.age_matches and m.gender_matches)
  ) option(facet, value, matches)
  where option.matches
  group by option.facet, option.value
  order by option.facet, option.value;
$$;


--
-- Name: FUNCTION federation_people_facets(federation federated.federation, search text, age_groups text[], genders text[], nationalities text[]); Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON FUNCTION federated.federation_people_facets(federation federated.federation, search text, age_groups text[], genders text[], nationalities text[]) IS '@name federation_people_facets
@behavior +list -connection';


--
-- Name: resolve_age_group(text); Type: FUNCTION; Schema: federated; Owner: -
--

CREATE FUNCTION federated.resolve_age_group(source_name text) RETURNS text
    LANGUAGE sql STABLE STRICT
    AS $$
  select code from federated.age_group
  where regexp_replace(lower(source_name), '[^a-z0-9]', '', 'g')
    = any(array_prepend(regexp_replace(lower(code), '[^a-z0-9]', '', 'g'), aliases));
$$;


--
-- Name: FUNCTION resolve_age_group(source_name text); Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON FUNCTION federated.resolve_age_group(source_name text) IS '@behavior -*';


--
-- Name: access_credential; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.access_credential (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    person_id bigint NOT NULL,
    kind public.access_credential_kind DEFAULT 'MIFARE'::public.access_credential_kind NOT NULL,
    label text NOT NULL,
    code text NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    valid_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by bigint DEFAULT public.current_user_id(),
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    CONSTRAINT access_credential_code_check CHECK (((code <> ''::text) AND (code = btrim(code)))),
    CONSTRAINT access_credential_label_check CHECK (((label <> ''::text) AND (label = btrim(label)))),
    CONSTRAINT access_credential_until_gt_since CHECK ((until > since))
);


--
-- Name: TABLE access_credential; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.access_credential IS '@behavior +list -connection -delete';


--
-- Name: COLUMN access_credential.valid_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.access_credential.valid_range IS '@behavior -*';


--
-- Name: access_credential_is_allowed(public.access_credential); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.access_credential_is_allowed(c public.access_credential) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select c.valid_range @> now() and (
    exists (select 1 from current_tenant_membership r where r.person_id = c.person_id)
    or exists (select 1 from current_tenant_trainer r where r.person_id = c.person_id)
    or exists (select 1 from current_tenant_administrator r where r.person_id = c.person_id)
  );
$$;


--
-- Name: access_credential_last_used(public.access_credential); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.access_credential_last_used(c public.access_credential) RETURNS timestamp with time zone
    LANGUAGE sql STABLE
    AS $$
  select max(e.occurred_at)
  from access_event e
  where e.tenant_id = c.tenant_id
    and e.kind = c.kind
    and e.code = c.code
    and e.allowed
    and c.valid_range @> e.occurred_at;
$$;


--
-- Name: account; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.account (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    person_id bigint,
    opening_balance numeric(19,4) DEFAULT 0.0 NOT NULL,
    currency public.citext DEFAULT 'CZK'::public.citext NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE account; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.account IS '@behavior +list -connection -insert -update -delete -query:resource:single';


--
-- Name: posting; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.posting (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    transaction_id bigint NOT NULL,
    account_id bigint NOT NULL,
    original_account_id bigint,
    amount numeric(19,4) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE posting; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.posting IS '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';


--
-- Name: transaction; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.transaction (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    accounting_period_id bigint NOT NULL,
    payment_id bigint,
    source public.transaction_source NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    description text,
    effective_date timestamp with time zone NOT NULL
);


--
-- Name: TABLE transaction; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.transaction IS '@behavior -insert -update';


--
-- Name: account_assets(public.account, timestamp with time zone, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.account_assets(a public.account, since timestamp with time zone, until timestamp with time zone) RETURNS numeric
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT COALESCE(sum(s.s), 0.0) AS "coalesce"
    FROM ( SELECT 0 AS s
         UNION
          SELECT COALESCE(sum(posting.amount), 0.0) AS s
            FROM (public.posting
              JOIN public.transaction ON ((posting.transaction_id = transaction.id)))
           WHERE (((account_assets.a).id = posting.account_id) AND (posting.amount > 0.0) AND (transaction.effective_date >= account_assets.since) AND (transaction.effective_date <= account_assets.until))
           GROUP BY posting.account_id) s;
END;


--
-- Name: account_balance(public.account); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.account_balance(a public.account) RETURNS numeric
    LANGUAGE sql STABLE
    AS $$
  select balance from account_balances where id=a.id;
$$;


--
-- Name: account_liabilities(public.account, timestamp with time zone, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.account_liabilities(a public.account, since timestamp with time zone, until timestamp with time zone) RETURNS numeric
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT COALESCE(sum(s.s), 0.0) AS "coalesce"
    FROM ( SELECT 0 AS s
         UNION
          SELECT COALESCE(sum(posting.amount), 0.0) AS s
            FROM (public.posting
              JOIN public.transaction ON ((posting.transaction_id = transaction.id)))
           WHERE (((account_liabilities.a).id = posting.account_id) AND (posting.amount < 0.0) AND (transaction.effective_date >= account_liabilities.since) AND (transaction.effective_date <= account_liabilities.until))
           GROUP BY posting.account_id) s;
END;


--
-- Name: activity_timeline_item; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.activity_timeline_item AS
 SELECT NULL::text AS id,
    NULL::public.activity_timeline_kind AS kind,
    NULL::timestamp with time zone AS sort_at,
    NULL::date AS activity_date,
    NULL::bigint AS person_id,
    NULL::text AS person_name,
    NULL::bigint AS event_attendance_id,
    NULL::bigint AS event_id,
    NULL::text AS federation,
    NULL::text AS federated_person_id,
    NULL::text AS competitor_id,
    NULL::text AS competitor_name,
    NULL::federated.competitor_type AS competitor_type,
    NULL::bigint AS competition_event_id,
    NULL::text AS competition_event_name,
    NULL::text AS competition_event_location,
    NULL::bigint AS competition_id,
    NULL::date AS competition_date,
    NULL::time without time zone AS check_in_end,
    NULL::federated.category AS category,
    NULL::text[] AS dances,
    NULL::integer AS participants,
    NULL::integer AS ranking,
    NULL::integer AS ranking_to,
    NULL::numeric(10,3) AS point_gain,
    NULL::boolean AS is_final,
    NULL::numeric(10,3) AS final_gain,
    NULL::federated.competition_type AS competition_type,
    NULL::text AS competition_event_external_id,
    NULL::text AS competition_external_id
  WHERE false;


--
-- Name: VIEW activity_timeline_item; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.activity_timeline_item IS '
@primaryKey id
@interface mode:single type:kind
@type EVENT_ATTENDANCE name:ActivityEventAttendance attributes:event_attendance_id,event_id
@type COMPETITION_BRIEF name:ActivityCompetitionBrief attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,check_in_end,category,dances,participants,competition_type,competition_event_external_id,competition_external_id
@type COMPETITION_RESULT name:ActivityCompetitionResult attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,dances,participants,ranking,ranking_to,point_gain,is_final,final_gain,competition_type,competition_event_external_id,competition_external_id
@type JUDGING name:ActivityJudging attributes:federation,federated_person_id,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,competition_type,competition_event_external_id,competition_external_id
@type BIRTHDAY name:ActivityBirthday
@foreignKey (person_id) references person (id)|@fieldName person|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_attendance_id) references event_registration (id)|@fieldName eventAttendance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_id) references event (id)|@fieldName event|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@behavior -query:resource:list -query:resource:connection -query:resource:single
';


--
-- Name: activity_timeline(timestamp with time zone, timestamp with time zone, bigint[], bigint, public.activity_timeline_kind[], public.event_type[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.activity_timeline(p_since timestamp with time zone, p_until timestamp with time zone, p_person_ids bigint[] DEFAULT NULL::bigint[], p_cohort_id bigint DEFAULT NULL::bigint, p_kinds public.activity_timeline_kind[] DEFAULT NULL::public.activity_timeline_kind[], p_event_types public.event_type[] DEFAULT NULL::public.event_type[]) RETURNS SETOF public.activity_timeline_item
    LANGUAGE plpgsql STABLE
    AS $$
declare
  include_event_attendance boolean;
  include_competition_brief boolean;
  include_competition_result boolean;
  include_judging boolean;
  include_birthday boolean;
begin
  if p_since is null or p_until is null or p_until <= p_since then
    return;
  end if;

  if cardinality(p_kinds) = 0 then
    return;
  end if;

  include_event_attendance =
    (p_kinds is null or 'EVENT_ATTENDANCE'::activity_timeline_kind = any(p_kinds))
    and (p_event_types is null or cardinality(p_event_types) > 0);
  include_competition_brief =
    p_kinds is null or 'COMPETITION_BRIEF'::activity_timeline_kind = any(p_kinds);
  include_competition_result =
    p_kinds is null or 'COMPETITION_RESULT'::activity_timeline_kind = any(p_kinds);
  include_judging =
    p_kinds is null or 'JUDGING'::activity_timeline_kind = any(p_kinds);
  include_birthday =
    p_kinds is null or 'BIRTHDAY'::activity_timeline_kind = any(p_kinds);

  if include_event_attendance then
    return query
      with scoped_people as (
        select distinct p.id, p.name
        from person p
        where (exists (select 1 from current_tenant_membership where person_id = p.id)
           or exists (select 1 from current_tenant_trainer where person_id = p.id)
           or exists (select 1 from current_tenant_administrator where person_id = p.id))
          and (p_person_ids is null or p.id = any(p_person_ids))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
      )
      select
        ('event_attendance:' || ea.id)::text as id,
        'EVENT_ATTENDANCE'::activity_timeline_kind as kind,
        ei.since as sort_at,
        ei.since::date as activity_date,
        ea.person_id,
        sp.name as person_name,
        ea.id as event_attendance_id,
        ei.id as event_id,
        null::text as federation,
        null::text as federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        null::bigint as competition_event_id,
        null::text as competition_event_name,
        null::text as competition_event_location,
        null::bigint as competition_id,
        null::date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::numeric(10, 3) as final_gain,
        null::federated.competition_type as competition_type,
        null::text as competition_event_external_id,
        null::text as competition_external_id
      from event_registration ea
      join event ei on ei.id = ea.event_id
      join scoped_people sp on sp.id = ea.person_id
      where ea.registration_status = 'active'
        and ei.since >= p_since
        and ei.since < p_until
        and (p_event_types is null or ei.type = any(p_event_types));
  end if;

  if include_birthday then
    return query
      with scoped_people as (
        select distinct p.id, p.name, p.birth_date
        from person p
        where (exists (select 1 from current_tenant_membership where person_id = p.id)
           or exists (select 1 from current_tenant_trainer where person_id = p.id)
           or exists (select 1 from current_tenant_administrator where person_id = p.id))
          and (p_person_ids is null or p.id = any(p_person_ids))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
          and p.birth_date is not null
      ),
      birthdays as (
        select
          sp.id as person_id,
          sp.name as person_name,
          make_date(
            years.year,
            extract(month from sp.birth_date)::int,
            least(
              extract(day from sp.birth_date)::int,
              extract(day from (
                make_date(years.year, extract(month from sp.birth_date)::int, 1)
                + interval '1 month - 1 day'
              ))::int
            )
          ) as birthday_date
        from scoped_people sp
        cross join generate_series(extract(year from p_since)::int, extract(year from p_until)::int) as years(year)
      )
      select
        ('birthday:' || b.person_id || ':' || b.birthday_date)::text as id,
        'BIRTHDAY'::activity_timeline_kind as kind,
        ((b.birthday_date + time '12:00')::timestamp)::timestamptz as sort_at,
        b.birthday_date as activity_date,
        b.person_id,
        b.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_id,
        null::text as federation,
        null::text as federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        null::bigint as competition_event_id,
        null::text as competition_event_name,
        null::text as competition_event_location,
        null::bigint as competition_id,
        null::date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::numeric(10, 3) as final_gain,
        null::federated.competition_type as competition_type,
        null::text as competition_event_external_id,
        null::text as competition_external_id
      from birthdays b
      join person p on p.id = b.person_id
      where b.birthday_date >= p.birth_date
        and ((b.birthday_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((b.birthday_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;

  if include_competition_result then
    return query
      select
        (
          'competition_result:' ||
          coalesce(cr.competition_id::text, '') || ':' ||
          coalesce(cr.competitor_id, '') || ':' ||
          coalesce(cr.person_id::text, '') || ':' ||
          coalesce((cr.category).id::text, '')
        )::text as id,
        'COMPETITION_RESULT'::activity_timeline_kind as kind,
        ((cr.competition_date + time '12:00')::timestamp)::timestamptz as sort_at,
        cr.competition_date as activity_date,
        cr.person_id,
        cr.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_id,
        cr.federation,
        cr.federated_person_id,
        cr.competitor_id,
        cr.competitor_name,
        cr.competitor_type,
        cr.event_id as competition_event_id,
        cr.event_name as competition_event_name,
        cr.event_location as competition_event_location,
        cr.competition_id,
        cr.competition_date,
        null::time as check_in_end,
        cr.category,
        cr.dances,
        cr.participants,
        cr.ranking,
        cr.ranking_to,
        cr.point_gain,
        cr.is_final,
        cr.final_gain,
        cr.competition_type,
        cr.event_external_id as competition_event_external_id,
        cr.competition_external_id
      from (
        select * from competition_report(p_since::date, p_until::date, p_cohort_id, p_person_ids)
      ) as cr
      where cr.competition_date is not null
        and ((cr.competition_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((cr.competition_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;

  if include_competition_brief then
    return query
      with reports as (
        select
          cr.person_id,
          cr.competition_id,
          cr.competitor_id,
          (cr.category).id as category_id
        from (
          select * from competition_report(p_since::date, p_until::date, p_cohort_id, p_person_ids)
        ) as cr
        where include_competition_result
      )
      select
        (
          'competition_brief:' ||
          coalesce(cb.competition_id::text, '') || ':' ||
          coalesce(cb.competitor_id, '') || ':' ||
          coalesce(cb.person_id::text, '') || ':' ||
          coalesce((cb.category).id::text, '')
        )::text as id,
        'COMPETITION_BRIEF'::activity_timeline_kind as kind,
        ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz as sort_at,
        cb.competition_date as activity_date,
        cb.person_id,
        cb.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_id,
        cb.federation,
        cb.federated_person_id,
        cb.competitor_id,
        cb.competitor_name,
        cb.competitor_type,
        cb.event_id as competition_event_id,
        cb.event_name as competition_event_name,
        cb.event_location as competition_event_location,
        cb.competition_id,
        cb.competition_date,
        cb.check_in_end,
        cb.category,
        cb.dances,
        cb.participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::numeric(10, 3) as final_gain,
        cb.competition_type,
        cb.event_external_id as competition_event_external_id,
        cb.competition_external_id
      from (
        select * from competition_brief(p_since::date, p_until::date, p_cohort_id, p_person_ids)
      ) as cb
      where cb.competition_date is not null
        and ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz >= p_since
        and ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz < p_until
        and not exists (
          select 1
          from reports r
          where r.person_id is not distinct from cb.person_id
            and r.competition_id is not distinct from cb.competition_id
            and r.competitor_id is not distinct from cb.competitor_id
            and r.category_id is not distinct from (cb.category).id
        );
  end if;

  if include_judging then
    return query
      with scoped_people as (
        select p.id, p.name
        from person p
        where (p.id = any(p_person_ids)
           or (p_person_ids is null and (
             exists (select 1 from current_tenant_membership where person_id = p.id)
             or exists (select 1 from current_tenant_trainer where person_id = p.id)
             or exists (select 1 from current_tenant_administrator where person_id = p.id)
           )))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
      ),
      federated_people as (
        select
          sp.id as person_id,
          sp.name as person_name,
          fp.id as federated_person_id,
          fp.federation
        from scoped_people sp
        join person_federation_link link on link.person_id = sp.id
        join federated.person fp
          on (fp.federation, fp.external_id) = (link.federation, link.external_id)
      )
      select distinct
        ('judging:event:' || e.id || ':' || fp.person_id)::text as id,
        'JUDGING'::activity_timeline_kind as kind,
        ((e.start_date + time '12:00')::timestamp)::timestamptz as sort_at,
        e.start_date as activity_date,
        fp.person_id,
        fp.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_id,
        e.federation,
        fp.federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        e.id as competition_event_id,
        e.name as competition_event_name,
        coalesce(e.location, e.city) as competition_event_location,
        null::bigint as competition_id,
        e.start_date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::numeric(10, 3) as final_gain,
        null::federated.competition_type as competition_type,
        e.external_id as competition_event_external_id,
        null::text as competition_external_id
      from federated.event_official eo
      join federated.event e on e.id = eo.event_id
      join federated_people fp on fp.federated_person_id = eo.person_id
      where eo.role = 'adjudicator'
        and e.start_date >= current_date
        and ((e.start_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((e.start_date + time '12:00')::timestamp)::timestamptz < p_until
      union all
      select
        ('judging:competition:' || c.id || ':' || fp.person_id)::text as id,
        'JUDGING'::activity_timeline_kind as kind,
        ((c.start_date + time '12:00')::timestamp)::timestamptz as sort_at,
        c.start_date as activity_date,
        fp.person_id,
        fp.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_id,
        c.federation,
        fp.federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        e.id as competition_event_id,
        e.name as competition_event_name,
        coalesce(e.location, e.city) as competition_event_location,
        c.id as competition_id,
        c.start_date as competition_date,
        null::time as check_in_end,
        cat as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::numeric(10, 3) as final_gain,
        c.competition_type,
        e.external_id as competition_event_external_id,
        c.external_id as competition_external_id
      from federated.competition_official co
      join federated.competition c on c.id = co.competition_id
      join federated.event e on e.id = c.event_id
      join federated.category cat on cat.id = c.category_id
      join federated_people fp on fp.federated_person_id = co.person_id
      where co.role = 'adjudicator'
        and c.start_date < current_date
        and ((c.start_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((c.start_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;
end;
$$;


--
-- Name: FUNCTION activity_timeline(p_since timestamp with time zone, p_until timestamp with time zone, p_person_ids bigint[], p_cohort_id bigint, p_kinds public.activity_timeline_kind[], p_event_types public.event_type[]); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.activity_timeline(p_since timestamp with time zone, p_until timestamp with time zone, p_person_ids bigint[], p_cohort_id bigint, p_kinds public.activity_timeline_kind[], p_event_types public.event_type[]) IS '@behavior +queryField:resource:list -queryField:resource:connection';


--
-- Name: announcement; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.announcement (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    author_id bigint,
    title text NOT NULL,
    body text NOT NULL,
    is_sticky boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone,
    scheduled_since timestamp with time zone,
    scheduled_until timestamp with time zone,
    status public.announcement_status DEFAULT 'draft'::public.announcement_status NOT NULL,
    CONSTRAINT announcement_schedule_check CHECK (((scheduled_since IS NULL) OR (scheduled_until IS NULL) OR (scheduled_since < scheduled_until)))
);


--
-- Name: TABLE announcement; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.announcement IS '@behavior -insert';


--
-- Name: announcement_author_name(public.announcement); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.announcement_author_name(a public.announcement) RETURNS text
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select concat_ws(' ', nullif(u.u_jmeno, ''), nullif(u.u_prijmeni, ''))
  from announcement stored
  join users u on u.id = stored.author_id
  where stored.id = a.id;
$$;


--
-- Name: cohort; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.cohort (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    cohort_group_id bigint,
    name text NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    color_rgb text NOT NULL,
    location text DEFAULT ''::text NOT NULL,
    is_visible boolean DEFAULT true NOT NULL,
    ordering integer DEFAULT 1 NOT NULL,
    external_ids text[],
    is_archived boolean DEFAULT false NOT NULL,
    image_id bigint
);


--
-- Name: TABLE cohort; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.cohort IS '@behavior +list -connection';


--
-- Name: archive_cohort(bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.archive_cohort(cohort_id bigint) RETURNS public.cohort
    LANGUAGE sql
    AS $_$
  update cohort_membership set until=now() where cohort_id = $1;
  update cohort
  set is_visible = false, cohort_group_id = null
  where id = $1
  returning *;
$_$;


--
-- Name: change_password(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.change_password(new_pass text) RETURNS void
    LANGUAGE plpgsql STRICT
    AS $$
begin
  update users set u_pass = new_pass
  where id = current_user_id();
end;
$$;


--
-- Name: competition_brief(date, date, bigint, bigint[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.competition_brief(p_since date DEFAULT ((date_trunc('week'::text, now()))::date + 4), p_until date DEFAULT ((date_trunc('week'::text, now()))::date + 7), p_cohort_id bigint DEFAULT NULL::bigint, p_person_ids bigint[] DEFAULT NULL::bigint[]) RETURNS SETOF public.competition_participation_record
    LANGUAGE sql STABLE
    AS $$
  with scoped_people as materialized (
    select p.id, p.name
    from person p
    where (p.id = any(p_person_ids)
       or (p_person_ids is null and p.id in (
         select person_id from current_tenant_membership
         union all
         select person_id from current_tenant_trainer
         union all
         select person_id from current_tenant_administrator
       )))
      and (p_cohort_id is null
       or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    join person_federation_link link on link.person_id = sp.id
    join federated.person fp
      on (fp.federation, fp.external_id) = (link.federation, link.external_id)
  )
  select
    fp.person_id,
    fp.person_name,
    fp.federation,
    fp.federated_person_id,
    c.id as competitor_id,
    c.name as competitor_name,
    c.competitor_type,
    e.id as event_id,
    e.name as event_name,
    coalesce(e.location, e.city) as event_location,
    comp.id as competition_id,
    comp.start_date as competition_date,
    comp.check_in_end,
    cat as category,
    dances.dances,
    comp.participants_total as participants,
    null::integer as ranking,
    null::integer as ranking_to,
    null::numeric(10,3) as point_gain,
    null::boolean as is_final,
    false as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id,
    null::numeric(10,3) as final_gain
  from federated_people fp
  join federated.competitor_component cc on cc.person_id = fp.federated_person_id
  join federated.competitor c on c.id = cc.competitor_id
  join federated.competition_entry ce on ce.competitor_id = c.id and not ce.cancelled
  join federated.competition comp on comp.id = ce.competition_id
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  left join lateral (
    select coalesce(array_agg(d.name order by dpd.dance_order), '{}'::text[]) as dances
    from federated.dance_program_dance dpd
    join federated.dance d on d.code = dpd.dance_code
    where dpd.program_id = cat.base_dance_program_id
  ) dances on true
  where comp.start_date >= p_since
    and comp.start_date < p_until
  order by
    comp.start_date,
    comp.check_in_end nulls last,
    fp.person_name,
    cat.discipline,
    cat.class,
    c.name;
$$;


--
-- Name: FUNCTION competition_brief(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.competition_brief(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) IS '@behavior -*';


--
-- Name: competition_report(date, date, bigint, bigint[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.competition_report(p_since date DEFAULT ((date_trunc('week'::text, now()))::date - 2), p_until date DEFAULT (date_trunc('week'::text, now()))::date, p_cohort_id bigint DEFAULT NULL::bigint, p_person_ids bigint[] DEFAULT NULL::bigint[]) RETURNS SETOF public.competition_participation_record
    LANGUAGE sql STABLE
    AS $$
  with scoped_people as materialized (
    select p.id, p.name
    from person p
    where (p.id = any(p_person_ids)
       or (p_person_ids is null and p.id in (
         select person_id from current_tenant_membership
         union all select person_id from current_tenant_trainer
         union all select person_id from current_tenant_administrator
       )))
      and (p_cohort_id is null
       or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    join person_federation_link link on link.person_id = sp.id
    join federated.person fp
      on (fp.federation, fp.external_id) = (link.federation, link.external_id)
  )
  select
    fp.person_id,
    fp.person_name,
    fp.federation,
    fp.federated_person_id,
    c.id as competitor_id,
    c.name as competitor_name,
    c.competitor_type,
    e.id as event_id,
    e.name as event_name,
    coalesce(e.location, e.city) as event_location,
    comp.id as competition_id,
    comp.start_date as competition_date,
    comp.check_in_end,
    cat as category,
    coalesce(dances.dances, '{}'::text[]) as dances,
    comp.participants_total as participants,
    cr.ranking,
    cr.ranking_to,
    cr.point_gain,
    cr.is_final,
    true as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id,
    cr.final_gain
  from federated_people fp
  join federated.competitor_component cc on cc.person_id = fp.federated_person_id
  join federated.competitor c on c.id = cc.competitor_id
  join federated.competition_result cr on cr.competitor_id = c.id
  join federated.competition comp on comp.id = cr.competition_id
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  left join lateral (
    select array_agg(d.name order by rd.dance_order) as dances
    from (
      select r.id
      from federated.competition_round r
      where r.competition_id = comp.id
      order by r.round_index desc, (r.round_key = 'F') desc, r.id desc
      limit 1
    ) r
    join federated.round_dance rd on rd.round_id = r.id
    join federated.dance d on d.code = rd.dance_code
  ) dances on true
  where comp.start_date >= p_since
    and comp.start_date < p_until
  order by
    comp.start_date,
    fp.person_name,
    cat.discipline,
    cat.class,
    cr.ranking,
    c.name;
$$;


--
-- Name: FUNCTION competition_report(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.competition_report(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) IS '@behavior -*';


--
-- Name: immutable_concat_ws(text, text[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.immutable_concat_ws(text, VARIADIC text[]) RETURNS text
    LANGUAGE internal IMMUTABLE PARALLEL SAFE
    AS $$text_concat_ws$$;


--
-- Name: competition; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.competition (
    id bigint NOT NULL,
    federation text NOT NULL,
    external_id text NOT NULL,
    event_id bigint NOT NULL,
    category_id bigint NOT NULL,
    start_date date NOT NULL,
    end_date date,
    check_in_end time without time zone,
    registration_fee numeric(10,3),
    participants_total integer,
    excused_total integer,
    completed_at timestamp with time zone,
    competition_type federated.competition_type,
    status federated.event_status DEFAULT 'unknown'::federated.event_status NOT NULL,
    CONSTRAINT competition_check CHECK (((end_date IS NULL) OR (end_date >= start_date)))
);


--
-- Name: TABLE competition; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.competition IS '@name competition
@behavior -insert -update -delete -list';


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
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    image_id bigint,
    CONSTRAINT couple_until_gt_since CHECK ((until > since))
);


--
-- Name: TABLE couple; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.couple IS '@behavior +list -connection -query:resource:list -query:resource:connection
@ref federationCompetitors via:public.couple_federation_link;federated.competitor
@ref federationRanklistEntries via:public.couple_federation_link;federated.competitor;federated.ranklist_entry';


--
-- Name: COLUMN couple.legacy_pary_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.couple.legacy_pary_id IS '@behavior -*';


--
-- Name: COLUMN couple.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.couple.active_range IS '@behavior -*';


--
-- Name: person; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.person (
    id bigint NOT NULL,
    first_name text NOT NULL,
    last_name text NOT NULL,
    gender public.gender_type NOT NULL,
    birth_date date,
    nationality text NOT NULL,
    tax_identification_number text,
    national_id_number text,
    csts_id integer,
    wdsf_id integer,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    legacy_user_id bigint,
    prefix_title text DEFAULT ''::text NOT NULL,
    suffix_title text DEFAULT ''::text NOT NULL,
    bio text DEFAULT ''::text NOT NULL,
    email public.citext,
    phone text,
    name text GENERATED ALWAYS AS (public.immutable_concat_ws(' '::text, VARIADIC ARRAY[NULLIF(btrim(prefix_title), ''::text), NULLIF(btrim(first_name), ''::text), NULLIF(btrim(last_name), ''::text),
CASE
    WHEN (btrim(suffix_title) = ''::text) THEN NULL::text
    ELSE (', '::text || btrim(suffix_title))
END])) STORED NOT NULL,
    address public.address_domain,
    external_ids text[],
    note text DEFAULT ''::text NOT NULL,
    search_name text GENERATED ALWAYS AS (app_private.normalize_name(public.immutable_concat_ws(' '::text, VARIADIC ARRAY[first_name, last_name]))) STORED,
    instagram_username text,
    tiktok_username text,
    facebook_url text,
    website_url text,
    image_id bigint
);


--
-- Name: TABLE person; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.person IS '@behavior -insert
@ref federationPeople via:public.person_federation_link;federated.person
@ref federationCategoryProgresses via:public.person_federation_link;federated.person;federated.competitor_component;federated.competitor;federated.competitor_category_progress
@ref federationRanklistEntries via:public.person_federation_link;federated.person;federated.competitor_component;federated.competitor;federated.ranklist_entry';


--
-- Name: COLUMN person.legacy_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.person.legacy_user_id IS '@behavior -*';


--
-- Name: COLUMN person.search_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.person.search_name IS '@behavior -*';


--
-- Name: competition_suggestions(bigint[], date, date, bigint[], bigint, bigint, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.competition_suggestions(event_ids bigint[], since date DEFAULT CURRENT_DATE, until date DEFAULT NULL::date, person_ids bigint[] DEFAULT NULL::bigint[], couple_id bigint DEFAULT NULL::bigint, cohort_id bigint DEFAULT NULL::bigint, registered_only boolean DEFAULT false) RETURNS TABLE(competition federated.competition, person public.person, couple public.couple, status text)
    LANGUAGE plpgsql STABLE
    AS $$
begin
  if event_ids is null then
    raise exception 'event_ids is required' using errcode = '22004';
  end if;
  return query
  select c, p, partnership, m.status
  from (
    select competition_id, local_person_id, local_couple_id, case
      when bool_or(matches.status = 'registered') then 'registered'
      when bool_or(matches.status = 'cancelled') then 'cancelled'
      else 'suggested'
    end as status
    from suggested_competition_matches(since, until, person_ids, couple_id, cohort_id, event_ids, registered_only)
      matches
    group by competition_id, local_person_id, local_couple_id
  ) m
  join federated.competition c on c.id = m.competition_id
  left join public.person p on p.id = m.local_person_id
  left join public.couple partnership on partnership.id = m.local_couple_id;
end;
$$;


--
-- Name: FUNCTION competition_suggestions(event_ids bigint[], since date, until date, person_ids bigint[], couple_id bigint, cohort_id bigint, registered_only boolean); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.competition_suggestions(event_ids bigint[], since date, until date, person_ids bigint[], couple_id bigint, cohort_id bigint, registered_only boolean) IS '@behavior +list -connection';


--
-- Name: confirm_membership_application(bigint, boolean, boolean, boolean, timestamp with time zone, bigint[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.confirm_membership_application(application_id bigint, is_member boolean DEFAULT true, is_trainer boolean DEFAULT false, is_admin boolean DEFAULT false, join_date timestamp with time zone DEFAULT now(), cohort_ids bigint[] DEFAULT ARRAY[]::bigint[]) RETURNS public.person
    LANGUAGE sql
    AS $$
  with application as materialized (
    select *
    from membership_application
    where id = application_id and status = 'sent'
    for update
  ), t_person as (
    insert into person (
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone,
      note
    )
    select
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id,
      coalesce(prefix_title, ''), coalesce(suffix_title, ''), coalesce(bio, ''),
      email, phone, coalesce(note, '')
    from application
    returning *
  ), appl as (
    update membership_application
    set status = 'approved'
    where id = (select id from application)
  ), member as (
    insert into tenant_membership (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_member
  ), trainer as (
    insert into tenant_trainer (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_trainer
  ), administrator as (
    insert into tenant_administrator (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_admin
  ), cohorts as (
    insert into cohort_membership (cohort_id, person_id, since)
    select cohort_id, t_person.id, join_date
    from t_person
    cross join unnest(coalesce(cohort_ids, array[]::bigint[])) selected(cohort_id)
  ), proxy as (
    insert into user_proxy (person_id, user_id)
    select t_person.id, application.created_by
    from t_person cross join application
  )
  select * from t_person;
$$;


--
-- Name: couple_age_group(public.couple); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.couple_age_group(c public.couple) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select r.age_group_code
  from person m, person w, federated.couple_age_rule r
  where m.id = c.man_id and w.id = c.woman_id
    and m.birth_date is not null and w.birth_date is not null
    and r.division = 'General'
    and r.age_group_code <> 'Under_21'
    and r.older_age @> (extract(year from current_date)::int - extract(year from least(m.birth_date, w.birth_date))::int)
    and r.younger_age @> (extract(year from current_date)::int - extract(year from greatest(m.birth_date, w.birth_date))::int)
  order by lower(r.older_age) desc nulls last, upper(r.older_age)
  limit 1;
$$;


--
-- Name: FUNCTION couple_age_group(c public.couple); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.couple_age_group(c public.couple) IS '@behavior -queryField';


--
-- Name: create_credit_transaction_for_person(bigint, text, numeric, text, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_credit_transaction_for_person(v_person_id bigint, v_description text, v_amount numeric, v_currency text, v_date timestamp with time zone DEFAULT now()) RETURNS public.transaction
    LANGUAGE sql
    AS $$
  with txn as (
    insert into transaction (source, description, effective_date)
    values ('manual-credit', v_description, v_date) returning *
  ), posting as (
    insert into posting (transaction_id, account_id, amount)
    values ((select id from txn), (select id from person_account(V_person_id, v_currency)), v_amount)
  )
  select * from txn;
$$;


--
-- Name: create_event_payment(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_event_payment(target_event public.event) RETURNS public.payment
    LANGUAGE plpgsql
    AS $$
declare
  created_payment payment;
  duration numeric(19, 4);
begin
  if current_tenant_id() <> 2 then
    return null;
  end if;

  -- Serialize initial charging with registration changes and their deferred sync.
  select * into target_event from event
  where id = target_event.id and tenant_id = current_tenant_id() for no key update;
  if not found or target_event.is_cancelled then return null; end if;

  select p.* into created_payment from payment p where p.event_id = target_event.id;

  if found then
    return created_payment;
  end if;

  if target_event.type <> 'lesson' or not exists (
    select 1
    from event_registration registration
    where registration.event_id = target_event.id
      and registration.person_id is not null
      and registration.registration_status = 'active'
  ) then
    return null;
  end if;

  duration := extract(epoch from (target_event.until - target_event.since)) / 60;

  insert into payment (accounting_period_id, status, event_id, due_at)
  values (
    (select id from accounting_period where tenant_id = current_tenant_id() and range @> now()),
    'tentative', target_event.id, now() + interval '2 weeks'
  )
  on conflict (event_id) where event_id is not null do nothing
  returning * into created_payment;

  if not found then
    return (select p from payment p where p.event_id = target_event.id);
  end if;

  insert into payment_recipient (payment_id, account_id, amount)
  select distinct on (account.id)
    created_payment.id,
    account.id,
    trainer.member_price_45min_amount / 45 * duration
  from event_trainers(target_event) trainer
  cross join lateral person_account(trainer.person_id, trainer.currency) account
  where trainer.tenant_id = current_tenant_id()
    and trainer.member_price_45min_amount is not null;

  insert into payment_debtor (payment_id, person_id)
  select distinct created_payment.id, registration.person_id
  from event_registration registration
  where registration.event_id = target_event.id
    and registration.person_id is not null
    and registration.registration_status = 'active';

  return created_payment;
end
$$;


--
-- Name: FUNCTION create_event_payment(target_event public.event); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.create_event_payment(target_event public.event) IS '@behavior -*';


--
-- Name: cohort_subscription; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.cohort_subscription (
    id bigint NOT NULL,
    cohort_id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    account_id bigint NOT NULL,
    price public.price NOT NULL,
    active boolean DEFAULT true NOT NULL,
    renews_on timestamp with time zone,
    "interval" interval DEFAULT '1 mon'::interval NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    amount numeric(19,4) GENERATED ALWAYS AS ((price).amount) STORED,
    currency text GENERATED ALWAYS AS ((price).currency) STORED
);


--
-- Name: TABLE cohort_subscription; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.cohort_subscription IS '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';


--
-- Name: create_missing_cohort_subscription_payments(public.cohort_subscription); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_missing_cohort_subscription_payments(c public.cohort_subscription) RETURNS SETOF public.payment
    LANGUAGE plpgsql
    AS $$
declare
  member person;
  v_specific text;
  v_payment payment;
  created_ids bigint[] := array[]::bigint[];
  v_renews_on timestamptz;
begin
  if not c.active then
    return;
  end if;
  select renews_on - interval into v_renews_on from cohort_subscription where id = c.id;
  v_specific := '' || (extract(year from v_renews_on) - 2000) || extract(month from v_renews_on) || extract(day from v_renews_on) || c.id;

  for member in select person.*
    from cohort_membership
    join person on person.id=person_id
    where cohort_id=c.cohort_id
      and v_renews_on <@ cohort_membership.active_range
      and not exists (
          select 1 from payment join payment_debtor on payment_id=payment.id
          where cohort_subscription_id = c.id and due_at = v_renews_on and person_id=person.id
      ) loop
    insert into payment (
      status,
      specific_symbol,
      variable_symbol,
      cohort_subscription_id,
      accounting_period_id,
      is_auto_credit_allowed,
      due_at
    ) values (
      'unpaid',
      v_specific,
      regexp_replace(coalesce(nullif(member.tax_identification_number, ''), member.id::text), '[^0-9]', '', 'g'),
      c.id,
      (select id from accounting_period where range @> now()),
      current_tenant_id() <> 1,
      v_renews_on
    ) returning * into v_payment;
    created_ids := created_ids || v_payment.id;

    insert into payment_recipient (payment_id, account_id, amount)
    values (v_payment.id, c.account_id, (c.price).amount);

    insert into payment_debtor (payment_id, person_id)
    values (v_payment.id, member.id);
  end loop;

  return query select * from payment where id = any (created_ids);
end
$$;


--
-- Name: FUNCTION create_missing_cohort_subscription_payments(c public.cohort_subscription); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.create_missing_cohort_subscription_payments(c public.cohort_subscription) IS '@behavior -*';


--
-- Name: create_next_cohort_subscription_payment(public.cohort_subscription); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_next_cohort_subscription_payment(c public.cohort_subscription) RETURNS SETOF public.payment
    LANGUAGE plpgsql
    AS $$
declare
  member person;
  v_specific text;
  v_payment payment;
  created_ids bigint[] := array[]::bigint[];
begin
  if not c.active then
    return;
  end if;
  update cohort_subscription set renews_on = renews_on + interval where id = c.id;
  v_specific := '' || (extract(year from c.renews_on) - 2000) || extract(month from c.renews_on) || extract(day from c.renews_on) || c.id;

  for member in select person.* from cohort_membership join person on person.id=person_id where cohort_id=c.cohort_id and c.renews_on <@ cohort_membership.active_range loop
    insert into payment (
      status,
      specific_symbol,
      variable_symbol,
      cohort_subscription_id,
      accounting_period_id,
      is_auto_credit_allowed,
      due_at
    ) values (
      'unpaid',
      v_specific,
      regexp_replace(coalesce(nullif(member.tax_identification_number, ''), member.id::text), '[^0-9]', '', 'g'),
      c.id,
      (select id from accounting_period where range @> now()),
      current_tenant_id() <> 1,
      c.renews_on
    ) returning * into v_payment;
    created_ids := created_ids || v_payment.id;

    insert into payment_recipient (payment_id, account_id, amount)
    values (v_payment.id, c.account_id, (c.price).amount);

    insert into payment_debtor (payment_id, person_id)
    values (v_payment.id, member.id);
  end loop;

  return query select * from payment where id = any (created_ids);
end
$$;


--
-- Name: FUNCTION create_next_cohort_subscription_payment(c public.cohort_subscription); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.create_next_cohort_subscription_payment(c public.cohort_subscription) IS '@behavior -*';


--
-- Name: create_person(bigint, public.person, boolean, boolean, boolean, boolean, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_person(person_id bigint, INOUT p public.person, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamp with time zone) RETURNS public.person
    LANGUAGE plpgsql
    AS $$
begin
  if person_id is null then
    insert into person (
      first_name,
      last_name,
      gender,
      birth_date,
      nationality,
      tax_identification_number,
      national_id_number,
      csts_id,
      wdsf_id,
      prefix_title,
      suffix_title,
      bio,
      email,
      phone,
      external_ids,
      instagram_username,
      tiktok_username,
      facebook_url,
      website_url
    ) values (
      p.first_name,
      p.last_name,
      p.gender,
      p.birth_date,
      p.nationality,
      p.tax_identification_number,
      p.national_id_number,
      p.csts_id,
      p.wdsf_id,
      p.prefix_title,
      p.suffix_title,
      p.bio,
      p.email,
      p.phone,
      p.external_ids,
      p.instagram_username,
      p.tiktok_username,
      p.facebook_url,
      p.website_url
    ) returning * into p;
  else
    select * into p from person where person.id=person_id;
  end if;
  if is_member = true then
    insert into tenant_membership (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_trainer = true then
    insert into tenant_trainer (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_admin = true then
    insert into tenant_administrator (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if send_invitation = true and p.email is not null and p.email <> '' then
    insert into person_invitation (person_id, tenant_id, email) values (p.id, current_tenant_id(), p.email);
  end if;
end
$$;


--
-- Name: current_account_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_account_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  select id from account
  where person_id = any ((select current_person_ids())::bigint[]);
$$;


--
-- Name: FUNCTION current_account_ids(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.current_account_ids() IS '@behavior -*';


--
-- Name: current_claims(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_claims() RETURNS jsonb
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select to_jsonb(app_private.create_jwt_token(users)) - 'exp'
  from users
  where id = nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$;


--
-- Name: current_couple_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_couple_ids() RETURNS bigint[]
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_couple_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;


--
-- Name: FUNCTION current_couple_ids(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.current_couple_ids() IS '@behavior -*';


--
-- Name: current_person_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_person_ids() RETURNS bigint[]
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_person_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;


--
-- Name: FUNCTION current_person_ids(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.current_person_ids() IS '@behavior -*';


--
-- Name: event_approx_price(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_approx_price(v_instance public.event) RETURNS TABLE(amount numeric, currency text)
    LANGUAGE sql STABLE
    AS $$
  with stats as materialized (
    select
      count(*) as num_participants,
      extract(epoch from (v_instance.until - v_instance.since)) / 60.0 as duration
    from event_registration registration
    where
      v_instance.type = 'lesson'
      and registration.event_id = v_instance.id
      and registration.person_id is not null
      and registration.registration_status = 'active'
  )
  select
    sum(tt.member_price_45min_amount * s.duration / 45 / s.num_participants) as amount,
    tt.currency as currency
  from stats s
  join lateral event_trainers(v_instance) tt on true
  where
    s.num_participants > 0
    and s.duration > 0
    and tt.member_price_45min_amount is not null
    and tt.currency is not null
  group by tt.currency;
$$;


--
-- Name: FUNCTION event_approx_price(v_instance public.event); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.event_approx_price(v_instance public.event) IS '@behavior +list -connection';


--
-- Name: event_facets(timestamp with time zone, timestamp with time zone, public.event_type[], bigint[], bigint[], bigint, public.event_instance_range_scope, bigint[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_facets(start_range timestamp with time zone, end_range timestamp with time zone, only_types public.event_type[] DEFAULT NULL::public.event_type[], trainer_ids bigint[] DEFAULT NULL::bigint[], participant_ids bigint[] DEFAULT NULL::bigint[], parent_id bigint DEFAULT NULL::bigint, scope public.event_instance_range_scope DEFAULT 'all'::public.event_instance_range_scope, location_ids bigint[] DEFAULT NULL::bigint[]) RETURNS TABLE(facet text, value text, count integer)
    LANGUAGE sql STABLE
    AS $_$
  with candidates as (
    select i.id, i.type, i.location_id,
      array(select distinct person_id from event_trainer where event_id = i.id) as trainers,
      array(select distinct person_id from event_registration
        where event_id = i.id and registration_status = 'active') as participants
    from event_instances_for_range(null, start_range, end_range,
      parent_id => $6, scope => $7) i
  ), matches as (
    select *,
      only_types is null or type = any(only_types) as type_matches,
      trainer_ids is null or trainers && trainer_ids as trainer_matches,
      participant_ids is null or participants && participant_ids as participant_matches,
      location_ids is null or location_id = any(location_ids) as location_matches
    from candidates
  )
  select option.facet, option.value, count(distinct m.id)::integer
  from matches m
  cross join lateral (
    select 'type' as facet, upper(m.type::text) as value
    where m.trainer_matches and m.participant_matches and m.location_matches
    union all
    select 'trainer', unnest(m.trainers)::text
    where m.type_matches and m.participant_matches and m.location_matches
    union all
    select 'participant', unnest(m.participants)::text
    where m.type_matches and m.trainer_matches and m.location_matches
    union all
    select 'location', m.location_id::text
    where m.type_matches and m.trainer_matches and m.participant_matches and m.location_id is not null
  ) option
  group by option.facet, option.value
  order by option.facet, option.value;
$_$;


--
-- Name: FUNCTION event_facets(start_range timestamp with time zone, end_range timestamp with time zone, only_types public.event_type[], trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[]); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.event_facets(start_range timestamp with time zone, end_range timestamp with time zone, only_types public.event_type[], trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[]) IS '@behavior +list -connection';


--
-- Name: event_instances_for_range(public.event_type, timestamp with time zone, timestamp with time zone, bigint[], bigint[], bigint, public.event_instance_range_scope, bigint[], public.event_type[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, trainer_ids bigint[] DEFAULT NULL::bigint[], participant_ids bigint[] DEFAULT NULL::bigint[], parent_id bigint DEFAULT NULL::bigint, scope public.event_instance_range_scope DEFAULT 'all'::public.event_instance_range_scope, location_ids bigint[] DEFAULT NULL::bigint[], only_types public.event_type[] DEFAULT NULL::public.event_type[]) RETURNS SETOF public.event
    LANGUAGE sql STABLE
    AS $_$
  with mine as (
    select event_id from event_registration
    where person_id = any ((select current_person_ids())::bigint[]) and registration_status = 'active'
    union all
    select event_id from event_trainer
    where person_id = any ((select current_person_ids())::bigint[])
  )
  select i.*
  from event i
  where i.tenant_id = current_tenant_id()
    and (only_type is null or i.type = only_type)
    and (only_types is null or i.type = any(only_types))
    and case
      when $6 is not null then i.parent_id = $6
        and (scope <> 'mine' or i.id in (select event_id from mine))
      when scope = 'all' then true
      when scope = 'top_level' then i.parent_id is null
      when scope = 'mine' then i.id in (select event_id from mine)
      when scope = 'relevant' then i.parent_id is null
        or i.id in (select event_id from mine)
        or i.parent_id in (select event_id from mine)
    end
    and start_range is not null
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and app_private.tstzrange_overlaps(i.range, tstzrange(
      start_range, greatest(start_range, coalesce(end_range, 'infinity'::timestamptz)), '[]'))
    and (trainer_ids is null
      or exists (select 1 from event_trainer where event_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_registration where event_id = i.id and person_id = any (participant_ids) and registration_status = 'active'))
    and (location_ids is null or i.location_id = any (location_ids))
  order by i.since
  ;
$_$;


--
-- Name: FUNCTION event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[], only_types public.event_type[]); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[], only_types public.event_type[]) IS '@behavior +list -connection';


--
-- Name: event_overlaps_attendee_report(timestamp with time zone, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_overlaps_attendee_report(p_since timestamp with time zone, p_until timestamp with time zone) RETURNS SETOF public.event_conflict
    LANGUAGE sql STABLE
    AS $$
  with instances as (
    select
      ea.person_id,
      p.name as person_name,
      ei.id as event_id,
      ei.parent_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from event_registration ea
    join event ei on ei.id = ea.event_id
    join person p on p.id = ea.person_id
    where not ei.is_cancelled
      and ea.person_id is not null
      and ea.registration_status = 'active'
      and app_private.tstzrange_overlaps(ei.range, tstzrange(coalesce(p_since, '-infinity'::timestamptz), coalesce(p_until, 'infinity'::timestamptz), '[]'))
  )
  select
    i1.person_id,
    i1.person_name,
    i1.event_id as first_event_id,
    i1.event_name as first_event_name,
    i1.since as first_since,
    i1.until as first_until,
    i2.event_id as second_event_id,
    i2.event_name as second_event_name,
    i2.since as second_since,
    i2.until as second_until,
    tstzrange(greatest(i1.since, i2.since), least(i1.until, i2.until), '[]') as overlap_range
  from instances i1
  join instances i2 on i1.person_id = i2.person_id
    and i1.event_id < i2.event_id
    and i1.parent_id is distinct from i2.event_id
    and i2.parent_id is distinct from i1.event_id
    and app_private.tstzrange_overlaps(i1.range, i2.range)
    and greatest(i1.since, i2.since) < least(i1.until, i2.until);
$$;


--
-- Name: FUNCTION event_overlaps_attendee_report(p_since timestamp with time zone, p_until timestamp with time zone); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.event_overlaps_attendee_report(p_since timestamp with time zone, p_until timestamp with time zone) IS '@behavior +list -connection';


--
-- Name: event_overlaps_trainer_report(timestamp with time zone, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_overlaps_trainer_report(p_since timestamp with time zone, p_until timestamp with time zone) RETURNS SETOF public.event_conflict
    LANGUAGE sql STABLE
    AS $$
  with trainer_instances as (
    select
      eit.person_id,
      p.name as person_name,
      ei.id as event_id,
      ei.parent_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from event ei
    join event_trainer eit on ei.id = eit.event_id
    join person p on p.id = eit.person_id
    where not ei.is_cancelled
      and app_private.tstzrange_overlaps(ei.range, tstzrange(coalesce(p_since, '-infinity'::timestamptz), coalesce(p_until, 'infinity'::timestamptz), '[]'))
  )
  select
    ti1.person_id,
    ti1.person_name,
    ti1.event_id as first_event_id,
    ti1.event_name as first_event_name,
    ti1.since as first_since,
    ti1.until as first_until,
    ti2.event_id as second_event_id,
    ti2.event_name as second_event_name,
    ti2.since as second_since,
    ti2.until as second_until,
    tstzrange(greatest(ti1.since, ti2.since), least(ti1.until, ti2.until), '[]') as overlap_range
  from trainer_instances ti1
  join trainer_instances ti2 on ti1.person_id = ti2.person_id
    and ti1.event_id < ti2.event_id
    and ti1.parent_id is distinct from ti2.event_id
    and ti2.parent_id is distinct from ti1.event_id
    and app_private.tstzrange_overlaps(ti1.range, ti2.range)
    and greatest(ti1.since, ti2.since) < least(ti1.until, ti2.until);
$$;


--
-- Name: FUNCTION event_overlaps_trainer_report(p_since timestamp with time zone, p_until timestamp with time zone); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.event_overlaps_trainer_report(p_since timestamp with time zone, p_until timestamp with time zone) IS '@behavior +list -connection';


--
-- Name: event_registration_info(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_registration_info(inst public.event) RETURNS public.event_registration_info
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select row(
    count(*) filter (where parent_registration_id is null)::integer,
    count(person_id)::integer,
    case
      when inst.capacity is null or inst.capacity <= 0 then null
      else inst.capacity
        - case inst.capacity_unit
            when 'people' then count(person_id)::integer
            when 'registrations' then count(*) filter (where parent_registration_id is null)::integer
          end
    end,
    count(*) filter (where person_id = any(current_person_ids())) > 0
  )::event_registration_info
  from event_registration
  where event_id = inst.id and registration_status = 'active';
$$;


--
-- Name: event_registration_last_attended(public.event_registration); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_registration_last_attended(reg public.event_registration) RETURNS timestamp with time zone
    LANGUAGE sql STABLE
    AS $$
  select max(attended.since)
  from event_registration r
  join event current on current.id = reg.event_id
  join event attended on attended.id = r.event_id
  where current.series_id is not null
    and attended.series_id = current.series_id
    and r.person_id = reg.person_id
    and r.status = 'attended'
$$;


--
-- Name: event_remaining_person_spots(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_remaining_person_spots(inst public.event) RETURNS integer
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select case
    when inst.capacity is null or inst.capacity <= 0 then null
    else inst.capacity - case inst.capacity_unit
        when 'people' then (
          select count(*)::integer
          from event_registration registration
          where registration.event_id = inst.id
            and registration.person_id is not null
            and registration.registration_status = 'active'
        )
        when 'registrations' then (
          select count(*)::integer
          from event_registration registration
          where registration.event_id = inst.id
            and registration.parent_registration_id is null
            and registration.registration_status = 'active'
        )
      end
  end;
$$;


--
-- Name: event_share_token(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_share_token(i public.event) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
  select share_token
  from event
  where id = $1.id
    and (pg_has_role(current_user, 'administrator', 'member')
      or (pg_has_role(current_user, 'trainer', 'member')
      and app_private.can_trainer_edit_event($1.id)))
$_$;


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
    lessons_offered integer DEFAULT 0
);


--
-- Name: TABLE event_trainer; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_trainer IS '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';


--
-- Name: event_trainer_lessons_remaining(public.event_trainer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_trainer_lessons_remaining(e public.event_trainer) RETURNS integer
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select case
    when e.lessons_offered is null then null
    else e.lessons_offered - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_request demand
      join event_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      where demand.trainer_id = e.id
    )
  end;
$$;


--
-- Name: event_trainer_name(public.event_trainer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_trainer_name(trainer public.event_trainer) RETURNS text
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT (concat_ws(' '::text, p.prefix_title, p.first_name, p.last_name) ||
         CASE p.suffix_title
             WHEN ''::text THEN ''::text
             ELSE (', '::text || p.suffix_title)
         END)
    FROM public.person p
   WHERE (p.id = (event_trainer_name.trainer).person_id);
END;


--
-- Name: event_trainer_tenant_trainer(public.event_trainer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_trainer_tenant_trainer(trainer public.event_trainer) RETURNS public.tenant_trainer
    LANGUAGE sql STABLE
    AS $$
  select tt.*
  from event e
  join tenant_trainer tt on tt.tenant_id = e.tenant_id
    and tt.person_id = trainer.person_id
    and tt.active_range @> e.since
  where e.id = trainer.event_id and e.tenant_id = trainer.tenant_id;
$$;


--
-- Name: FUNCTION event_trainer_tenant_trainer(trainer public.event_trainer); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.event_trainer_tenant_trainer(trainer public.event_trainer) IS '@behavior -queryField';


--
-- Name: event_trainers(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_trainers(v_instance public.event) RETURNS SETOF public.tenant_trainer
    LANGUAGE sql STABLE
    AS $$
select * from app_private.event_trainers_at(v_instance, v_instance.since);
$$;


--
-- Name: FUNCTION event_trainers(v_instance public.event); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.event_trainers(v_instance public.event) IS '@behavior +list -connection
@deprecated Use eventTrainersList.';


--
-- Name: federation_person_candidates(text, text, text, public.gender_type, date, integer, real); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.federation_person_candidates(first_name text, last_name text, federation text, gender public.gender_type DEFAULT NULL::public.gender_type, birth_date date DEFAULT NULL::date, "limit" integer DEFAULT 10, threshold real DEFAULT 0.4) RETURNS SETOF public.person_federation_candidates_record
    LANGUAGE sql STABLE
    AS $_$
  with input as materialized (
    select app_private.normalize_name(concat_ws(' ', trim($1), trim($2))) as name,
      app_private.normalize_name(trim($1)) as first_name,
      app_private.normalize_name(trim($2)) as last_name,
      extract(year from $5)::integer as birth_year,
      extract(year from current_date)::integer - extract(year from $5)::integer as age,
      case $4 when 'man' then 'male'::federated.gender
        when 'woman' then 'female'::federated.gender end as gender
  ), matches as (
    select fp.*, similarity(fp.search_name, i.name) as name_score,
      nullif(fp.gender, 'unknown') = i.gender as gender_match,
      coalesce(fp.birth_year, extract(year from fp.dob)::integer) = i.birth_year
        or (fp.birth_year is null and fp.dob is null
          and coalesce(g.age, g.person_age) @> i.age) as age_match
    from federated.person fp
    cross join input i
    left join federated.age_group g on g.code = fp.age_group_code
    where fp.federation = $3
      and nullif(i.first_name, '') is not null and nullif(i.last_name, '') is not null
      and fp.external_id between 1 and 2147483647
      and fp.search_name % i.name
      and similarity(fp.search_name, i.name)
        >= greatest(0.3::real, least(coalesce(threshold, 0.4), 1::real))
      -- Indexed word matching uses pg_trgm's default 0.6 threshold for each name part.
      and fp.search_name %> i.first_name
      and fp.search_name %> i.last_name
      and (i.gender is null or fp.gender is null or fp.gender = 'unknown' or fp.gender = i.gender)
      and (
        i.birth_year is null
        or coalesce(fp.birth_year, extract(year from fp.dob)::integer) = i.birth_year
        or (fp.birth_year is null and fp.dob is null and (
          coalesce(g.age, g.person_age) is null
          or coalesce(g.age, g.person_age) @> i.age
          -- Adult is also a broad label for people eligible for senior categories.
          or (g.code = 'Adult' and i.age >= lower(g.age))
        ))
      )
  ), candidates as (
    select * from matches
    order by gender_match desc nulls last, age_match desc nulls last,
      name_score desc, canonical_name, external_id
    limit greatest(0, least(coalesce($6, 10), 50))
  )
  select fp.external_id::integer, fp.canonical_name, fp.age_group_code, fp.name_score,
    coalesce(fp.birth_year, extract(year from fp.dob)::integer),
    case fp.federation when 'csts' then fp.external_id::integer else linked.external_id end,
    case fp.federation when 'wdsf' then fp.external_id::integer else linked.external_id end,
    fp.name_score = 1 and fp.gender_match is true and fp.age_match is true
      and not exists (
        select 1 from matches other
        where other.external_id <> fp.external_id and other.name_score = 1
      )
  from candidates fp
  left join lateral (
    select min(other.external_id)::integer as external_id
    from federated.person other
    where (
      (fp.federation = 'wdsf' and other.federation = 'csts'
        and other.external_id::text = fp.national_reference)
      or (fp.federation = 'csts' and other.federation = 'wdsf'
        and other.national_reference = fp.external_id::text)
    )
      and other.external_id between 1 and 2147483647
      and similarity(other.search_name, fp.search_name) >= 0.8
      and (other.gender is null or other.gender = 'unknown'
        or fp.gender is null or fp.gender = 'unknown' or other.gender = fp.gender)
      and (coalesce(other.birth_year, extract(year from other.dob)::integer) is null
        or coalesce(fp.birth_year, extract(year from fp.dob)::integer) is null
        or coalesce(other.birth_year, extract(year from other.dob)::integer)
          = coalesce(fp.birth_year, extract(year from fp.dob)::integer))
    having count(*) = 1
  ) linked on true
  order by fp.gender_match desc nulls last, fp.age_match desc nulls last,
    fp.name_score desc, fp.canonical_name, fp.external_id;
$_$;


--
-- Name: FUNCTION federation_person_candidates(first_name text, last_name text, federation text, gender public.gender_type, birth_date date, "limit" integer, threshold real); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.federation_person_candidates(first_name text, last_name text, federation text, gender public.gender_type, birth_date date, "limit" integer, threshold real) IS '@behavior +list -connection
@notNull';


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
    description text DEFAULT ''::text NOT NULL,
    bank_account text DEFAULT ''::text NOT NULL
);


--
-- Name: TABLE tenant; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant IS '@behavior +list -connection -insert -delete -singularRelation:resource:single -query:resource:connection';


--
-- Name: get_current_tenant(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.get_current_tenant() RETURNS public.tenant
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  SELECT * FROM tenant WHERE id = current_tenant_id();
$$;


--
-- Name: get_current_user(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.get_current_user(version_id text DEFAULT NULL::text) RETURNS public.users
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  with updated_user as (
    update users
    set
      last_active_at = now(),
      last_version = version_id
    where id = current_user_id()
    returning *
  )
  select * from updated_user
  union all
  select *
  from users
  where id = current_user_id()
    and not exists (select 1 from updated_user);
$$;


--
-- Name: FUNCTION get_current_user(version_id text); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.get_current_user(version_id text) IS '@behavior -*';


--
-- Name: invitation_info(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.invitation_info(token uuid) RETURNS text
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select email from person_invitation where access_token=token and used_at is null;
$$;


--
-- Name: invitation_name(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.invitation_name(token uuid) RETURNS text
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
select person.name
from person_invitation join person on person.id=person_id
where access_token=token and used_at is null;
$$;


--
-- Name: log_in_as(bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.log_in_as(id bigint) RETURNS public.login_result
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $_$
declare
  v_id alias for $1;
  usr users;
  jwt jwt_token;
begin
  select * into strict usr from users where users.id = v_id;
  jwt := app_private.create_jwt_token(usr);
  insert into security_event (user_id, kind, method)
  values (usr.id, 'impersonation', 'manual');
  return (usr, jwt);
end;
$_$;


--
-- Name: login(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.login(login text, passwd text) RETURNS public.login_result
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');

  select u.* into usr
  from users u
  where (lower(u.u_login) = lower(trim(login)) or lower(u.u_email) = lower(trim(login)))
    and u.u_pass = encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex');

  if usr is null then
    raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
  end if;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  update users set last_login = now() where id = usr.id;
  insert into security_event (user_id, kind, method)
  values (usr.id, 'login_succeeded', 'password');
  return (usr, jwt);
end;
$$;


--
-- Name: move_event(bigint, timestamp with time zone, timestamp with time zone, bigint, bigint, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.move_event(id bigint, since timestamp with time zone, until timestamp with time zone, trainer_person_id bigint DEFAULT NULL::bigint, location_id bigint DEFAULT NULL::bigint, location_text text DEFAULT NULL::text) RETURNS public.event
    LANGUAGE plpgsql
    AS $$
declare
  inst event;
begin
  update event
  set since = move_event.since,
      until = move_event.until,
      location_id = case
        when move_event.location_id is not null then move_event.location_id
        when move_event.location_text is not null then null
        else event.location_id
      end,
      location_text = case
        when move_event.location_id is not null then ''
        else coalesce(move_event.location_text, event.location_text)
      end
  where event.id = move_event.id
  returning * into inst;

  if not found then
    raise exception 'Event instance % not found', move_event.id;
  end if;

  if trainer_person_id is not null then
    update event_trainer
    set person_id = trainer_person_id
    where event_id = inst.id
      and 1 = (select count(*) from event_trainer where event_id = inst.id);

    if not found and not exists (select 1 from event_trainer where event_id = inst.id)
    then
      insert into event_trainer (event_id, person_id)
      values (inst.id, trainer_person_id);
    end if;
  end if;

  return (select instance from event instance where instance.id = inst.id);
end;
$$;


--
-- Name: my_tenants_array(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.my_tenants_array() RETURNS bigint[]
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_tenant_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;


--
-- Name: FUNCTION my_tenants_array(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.my_tenants_array() IS '@behavior -*';


--
-- Name: otp_login(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.otp_login(token uuid) RETURNS public.login_result
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  v_token otp_token;
  usr users;
  jwt jwt_token;
begin
  update otp_token t set used_at = now()
  where t.access_token = token and t.used_at is null and t.expires_at > now()
    and t.user_id is not null
    and (
      t.intent = 'password_reset'
      or (t.intent = 'event_handoff' and t.tenant_id = current_tenant_id())
    )
  returning t.* into v_token;
  if not found then
    raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
  end if;
  select * into usr from users where id = v_token.user_id;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);

  update users set last_login = now() where id = usr.id;
  insert into security_event (user_id, kind, method)
  values (usr.id, 'login_succeeded', 'otp');
  return (usr, jwt);
end;
$$;


--
-- Name: payment_debtor; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.payment_debtor (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    payment_id bigint NOT NULL,
    person_id bigint NOT NULL
);


--
-- Name: TABLE payment_debtor; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.payment_debtor IS '@behavior +list -connection -insert -update -delete';


--
-- Name: payment_debtor_is_unpaid(public.payment_debtor); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.payment_debtor_is_unpaid(p public.payment_debtor) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select status = 'unpaid' from payment where p.payment_id=payment.id;
$$;


--
-- Name: FUNCTION payment_debtor_is_unpaid(p public.payment_debtor); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.payment_debtor_is_unpaid(p public.payment_debtor) IS '@behavior +filter +filterProc +filterBy';


--
-- Name: payment_debtor_price(public.payment_debtor); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.payment_debtor_price(p public.payment_debtor, OUT amount numeric, OUT currency text) RETURNS record
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
select
  sum(payment_recipient.amount) / (
    select count(*) as count
    from payment_debtor
    where p.payment_id = payment_debtor.payment_id
  )::numeric(19,4) as amount,
  min(account.currency)::text as currency
from payment_recipient
  join account on payment_recipient.account_id = account.id
where payment_recipient.payment_id = p.payment_id
  -- Calculate the complete bill only for an authorized stored debtor.
  and exists (
    select from payment_debtor d
    where d.id = p.id and d.payment_id = p.payment_id
      and d.tenant_id = (select current_tenant_id())
      and (
        (select pg_has_role(coalesce(nullif(current_setting('role'), 'none'), session_user), 'administrator', 'member'))
        or (
          (select pg_has_role(coalesce(nullif(current_setting('role'), 'none'), session_user), 'member', 'member'))
          and d.person_id = any ((select current_person_ids())::bigint[])
        )
      )
  );
$$;


--
-- Name: FUNCTION payment_debtor_price(p public.payment_debtor, OUT amount numeric, OUT currency text); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.payment_debtor_price(p public.payment_debtor, OUT amount numeric, OUT currency text) IS '@behavior +list -connection';


--
-- Name: people_without_access_or_invitation(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.people_without_access_or_invitation() RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
select * from person
where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and not exists (select 1 from person_invitation where email = person.email)
  and not exists (select 1 from users where users.u_email = person.email)
  and (exists (select 1 from current_tenant_membership where person_id = person.id)
    or exists (select 1 from current_tenant_trainer where person_id = person.id)
    or exists (select 1 from current_tenant_administrator where person_id = person.id));
$$;


--
-- Name: FUNCTION people_without_access_or_invitation(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.people_without_access_or_invitation() IS '@behavior +list -connection';


--
-- Name: people_without_access_with_existing_account(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.people_without_access_with_existing_account() RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
select * from person
where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and exists (select 1 from users where users.u_email = person.email)
  and (exists (select 1 from current_tenant_membership where person_id = person.id)
  or exists (select 1 from current_tenant_trainer where person_id = person.id)
  or exists (select 1 from current_tenant_administrator where person_id = person.id));
$$;


--
-- Name: FUNCTION people_without_access_with_existing_account(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.people_without_access_with_existing_account() IS '@behavior +list -connection';


--
-- Name: people_without_access_with_invitation(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.people_without_access_with_invitation() RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
select * from person
where not exists (select 1 from user_proxy where person_id = person.id)
  and exists (select 1 from person_invitation where person_id = person.id)
  and (exists (select 1 from current_tenant_membership where person_id = person.id)
  or exists (select 1 from current_tenant_trainer where person_id = person.id)
  or exists (select 1 from current_tenant_administrator where person_id = person.id));
$$;


--
-- Name: FUNCTION people_without_access_with_invitation(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.people_without_access_with_invitation() IS '@behavior +list -connection';


--
-- Name: person_account(bigint, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_account(p_id bigint, in_currency text, OUT acc public.account) RETURNS public.account
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  WITH ins AS (
    INSERT INTO account (tenant_id, person_id, currency)
      VALUES ((select current_tenant_id()), p_id, COALESCE(in_currency, 'CZK'))
      ON CONFLICT ON CONSTRAINT account_tenant_id_person_id_currency_idx DO NOTHING
      RETURNING *
  )
  SELECT * FROM ins
  UNION ALL
  SELECT account.* FROM account
  WHERE tenant_id = (select current_tenant_id()) AND person_id = p_id AND currency = COALESCE(in_currency, 'CZK')
  LIMIT 1;
$$;


--
-- Name: FUNCTION person_account(p_id bigint, in_currency text, OUT acc public.account); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.person_account(p_id bigint, in_currency text, OUT acc public.account) IS '@behavior -*';


--
-- Name: person_active_couples(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_active_couples(p public.person) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    AS $$
  (select * from couple where man_id = p.id and status = 'active')
  union all
  (select * from couple where woman_id = p.id and status = 'active')
$$;


--
-- Name: FUNCTION person_active_couples(p public.person); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.person_active_couples(p public.person) IS '@behavior +list -connection';


--
-- Name: person_age_group(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_age_group(p public.person) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select code from federated.age_group
  where person_age @> (extract(year from current_date)::int - extract(year from p.birth_date)::int);
$$;


--
-- Name: FUNCTION person_age_group(p public.person); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.person_age_group(p public.person) IS '@behavior -queryField';


--
-- Name: person_all_couples(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_all_couples(p public.person) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    AS $$
  (select * from couple where man_id = p.id)
  union all
  (select * from couple where woman_id = p.id)
$$;


--
-- Name: FUNCTION person_all_couples(p public.person); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.person_all_couples(p public.person) IS '@behavior +list -connection';


--
-- Name: person_cohort_ids(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_cohort_ids(p public.person) RETURNS bigint[]
    LANGUAGE sql STABLE
    AS $$
  select array_agg(cohort_id) from cohort_membership where now() <@ active_range and person_id = p.id;
$$;


--
-- Name: person_facets(text[], bigint[], boolean, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_facets(membership_states text[] DEFAULT ARRAY['current'::text], in_cohorts bigint[] DEFAULT NULL::bigint[], is_trainer boolean DEFAULT NULL::boolean, is_admin boolean DEFAULT NULL::boolean) RETURNS TABLE(facet text, value text, count integer)
    LANGUAGE sql STABLE
    AS $_$
  with candidates as (
    select s.state, m.*,
      in_cohorts is null or case when cardinality(in_cohorts) = 0
        then cardinality(m.cohort_ids) = 0 else m.cohort_ids && in_cohorts end as cohort_matches,
       $3 is null or m.is_trainer = $3 as trainer_matches,
       $4 is null or m.is_admin = $4 as admin_matches
    from (values ('current'), ('former')) s(state)
    cross join lateral app_private.person_membership_candidates(s.state) m
    where m.in_state
  )
  select option.facet, option.value, count(distinct c.person_id)::integer
  from candidates c
  cross join lateral (
    select 'cohort' as facet, unnest(case when cardinality(c.cohort_ids) = 0
      then array['none'] else c.cohort_ids::text[] end) as value
    where (coalesce(cardinality(membership_states), 0) = 0 or c.state = any(membership_states)) and c.trainer_matches and c.admin_matches
    union all
    select 'trainer', c.is_trainer::text
    where (coalesce(cardinality(membership_states), 0) = 0 or c.state = any(membership_states)) and c.cohort_matches and c.admin_matches
    union all
    select 'admin', c.is_admin::text
    where (coalesce(cardinality(membership_states), 0) = 0 or c.state = any(membership_states)) and c.cohort_matches and c.trainer_matches
    union all
    select 'state', c.state
    where c.cohort_matches and c.trainer_matches and c.admin_matches
  ) option
  group by option.facet, option.value
  order by option.facet, option.value;
$_$;


--
-- Name: FUNCTION person_facets(membership_states text[], in_cohorts bigint[], is_trainer boolean, is_admin boolean); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.person_facets(membership_states text[], in_cohorts bigint[], is_trainer boolean, is_admin boolean) IS '@behavior +list -connection';


--
-- Name: person_federation_candidates(public.person, text, integer, real); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_federation_candidates(in_person public.person, federation text, "limit" integer DEFAULT 10, threshold real DEFAULT 0.4) RETURNS SETOF public.person_federation_candidates_record
    LANGUAGE sql STABLE
    AS $_$
  select * from federation_person_candidates(
    in_person.first_name, in_person.last_name, $2, in_person.gender, in_person.birth_date, $3, $4
  );
$_$;


--
-- Name: FUNCTION person_federation_candidates(in_person public.person, federation text, "limit" integer, threshold real); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.person_federation_candidates(in_person public.person, federation text, "limit" integer, threshold real) IS '@behavior +list -connection
@notNull';


--
-- Name: person_has_access(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_has_access(p public.person) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select exists (select 1 from user_proxy where person_id = p.id);
$$;


--
-- Name: person_is_admin(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_is_admin(p public.person) RETURNS boolean
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  select exists (select 1 from current_tenant_administrator where person_id = p.id);
$$;


--
-- Name: person_is_member(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_is_member(p public.person) RETURNS boolean
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  select exists (select 1 from current_tenant_membership where person_id = p.id);
$$;


--
-- Name: person_is_trainer(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_is_trainer(p public.person) RETURNS boolean
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  select exists (select 1 from current_tenant_trainer where person_id = p.id);
$$;


--
-- Name: refresh_jwt(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.refresh_jwt() RETURNS public.jwt_token
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select app_private.create_jwt_token(users)
  from users
  where id = current_user_id();
$$;


--
-- Name: register_using_invitation(text, text, uuid, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.register_using_invitation(email text, passwd text, token uuid, login text DEFAULT NULL::text) RETURNS public.login_result
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  invitation person_invitation;
  v_user_id bigint := current_user_id();
  v_salt text;
  usr users;
  jwt jwt_token;
  v_registered boolean := false;
begin
  select * into invitation from person_invitation where access_token=token for update;

  if invitation is null then
    raise exception 'INVITATION_NOT_FOUND' using errcode = '28000';
  end if;
  if invitation.used_at is not null then
    raise exception 'INVITATION_ALREADY_USED' using errcode = '28P01';
  end if;

  if v_user_id is null then
    if email is null or email = '' then
      raise exception 'INVALID_EMAIL' using errcode = '28P01';
    end if;

    v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');
    insert into users (u_login, u_email, u_pass) values (trim(login), email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
    v_registered := true;
  else
    select * into usr from users where id=v_user_id;
    if usr is null then
      raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
    end if;
  end if;

  perform set_config('jwt.claims.user_id', usr.id::text, true);
  insert into user_proxy (user_id, person_id) values (usr.id, invitation.person_id) on conflict do nothing;
  update person_invitation set used_at=now() where access_token=token;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  if v_registered then
    insert into security_event (user_id, kind, method)
    values (usr.id, 'registration', 'manual');
  end if;
  insert into security_event (user_id, person_id, kind, method)
  values (usr.id, invitation.person_id, 'invitation_accepted', 'manual');
  return (usr, jwt);
end
$$;


--
-- Name: register_without_invitation(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.register_without_invitation(email text, passwd text) RETURNS public.login_result
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');
  insert into users (u_email, u_pass) values (email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  insert into security_event (user_id, kind, method)
  values (usr.id, 'registration', 'manual');
  return (usr, jwt);
end
$$;


--
-- Name: membership_application; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.membership_application (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    first_name text NOT NULL,
    middle_name text,
    last_name text NOT NULL,
    gender public.gender_type NOT NULL,
    birth_date date,
    nationality text NOT NULL,
    tax_identification_number text,
    national_id_number text,
    csts_id integer,
    wdsf_id integer,
    prefix_title text,
    suffix_title text,
    bio text,
    email public.citext,
    phone text,
    created_by bigint NOT NULL,
    status public.application_form_status DEFAULT 'sent'::public.application_form_status NOT NULL,
    note text DEFAULT ''::text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE membership_application; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.membership_application IS '@behavior +list -connection';


--
-- Name: reject_membership_application(bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.reject_membership_application(application_id bigint) RETURNS public.membership_application
    LANGUAGE sql
    AS $$
  update membership_application set status='rejected' where id=application_id returning *;
$$;


--
-- Name: reset_password(character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.reset_password(email character varying) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  v_tenant tenant;
  v_user users;
  v_token otp_token;
  v_payload jsonb := jsonb_build_array();
begin
  for v_user in (select * from users where u_email = email) loop
    insert into otp_token (user_id)
    values (v_user.id) returning * into v_token;

    insert into security_event (user_id, kind, method)
    values (v_user.id, 'password_reset_requested', 'manual');

    v_payload := v_payload || jsonb_build_object(
      'login', v_user.u_login,
      'email', v_user.u_email,
      'token', v_token.access_token,
      'people', (
        select jsonb_agg(person.name)
        from user_proxy join person on person_id=person.id
        where status = 'active' and user_id = v_user.id
      )
    );
  end loop;

  select * into v_tenant from tenant where id = current_tenant_id();

  if v_payload is not null then
    perform graphile_worker.add_job('forgotten_password_generate', json_build_object(
      'origin', v_tenant.origins[1],
      'intent', '/zapomenute-heslo',
      'users', v_payload
    ));
  end if;
end;
$$;


--
-- Name: resolve_payment_with_credit(public.payment); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.resolve_payment_with_credit(p public.payment) RETURNS public.payment
    LANGUAGE plpgsql
    AS $$
declare
  trans transaction;
  recipient payment_recipient;
  acc account;
  trainer tenant_trainer;
  remaining_amount numeric(19, 4);
  total_amount numeric(19, 4);
  actual_payout numeric(19, 4);
begin
  if p.status <> 'unpaid' or not p.is_auto_credit_allowed then
    return null;
  end if;

  insert into transaction (payment_id, accounting_period_id, source)
  values (p.id, p.accounting_period_id, 'auto-credit')
  returning * into trans;

  total_amount := 0;

  for recipient in select * from payment_recipient where payment_id=p.id loop
      remaining_amount := recipient.amount;
      total_amount := total_amount + remaining_amount;

      select account.* into acc from account where id=recipient.account_id;
      select tenant_trainer.* into trainer from tenant_trainer where acc.person_id=tenant_trainer.person_id and tenant_id=acc.tenant_id;

      if trainer is null or trainer.create_payout_payments then
        if trainer.member_payout_45min_amount is not null then
          actual_payout := remaining_amount * trainer.member_payout_45min_amount / trainer.member_price_45min_amount;
          insert into posting (transaction_id, original_account_id, account_id, amount)
          values (trans.id, recipient.account_id, (select id from tenant_account(acc.currency)), remaining_amount - actual_payout);
          remaining_amount := actual_payout;
        end if;

        insert into posting (transaction_id, account_id, amount)
        values (trans.id, recipient.account_id, remaining_amount);
      end if;
    end loop;

  remaining_amount := total_amount / (select coalesce(nullif(count(*), 0), 1) from payment_debtor where payment_id = p.id);
  for acc in select a.* from payment_debtor d join lateral person_account(d.person_id, 'CZK') a on true where payment_id = p.id loop
      insert into posting (transaction_id, account_id, amount) values (trans.id, acc.id, 0.0 - remaining_amount);
    end loop;

  update payment set status = 'paid', paid_at = now() where id=p.id returning * into p;
  return p;
end
$$;


--
-- Name: FUNCTION resolve_payment_with_credit(p public.payment); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.resolve_payment_with_credit(p public.payment) IS '@behavior -*';


--
-- Name: save_events(public.event_details_input, public.event_input[], public.event_trainer_input[], bigint[], public.event_series_input); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.save_events(details public.event_details_input, events public.event_input[], trainers public.event_trainer_input[] DEFAULT '{}'::public.event_trainer_input[], cohort_ids bigint[] DEFAULT '{}'::bigint[], series public.event_series_input DEFAULT NULL::public.event_series_input) RETURNS SETOF public.event
    LANGUAGE plpgsql
    AS $$
declare
  event_to_save event_input;
  v_saved_event event;
  v_saved_event_ids bigint[] := '{}'::bigint[];
  v_tenant_id bigint := current_tenant_id();
  v_series_id bigint;
  v_assign_series boolean := (series).id is not null or (series).name is not null;
  v_is_visible boolean := coalesce((details).is_visible, true);
  v_is_public boolean := coalesce((details).is_public, false);
  v_has_public_details boolean := v_is_public and coalesce((details).has_public_details, false);
  v_is_locked boolean := coalesce((details).is_locked, false);
  v_enable_notes boolean := coalesce((details).enable_notes, false);
  v_expected_existing_count bigint;
  v_locked_existing_count bigint;
begin
  if details is null then
    raise exception 'event details are required';
  end if;

  if (details).type is null
    or (details).capacity is null
    or (details).capacity < 0
    or (details).capacity_unit is null then
    raise exception 'event details are incomplete';
  end if;

  if cardinality(coalesce(events, '{}'::event_input[])) = 0 then
    raise exception 'at least one event is required';
  end if;

  if exists (
    select 1 from unnest(events) i
    where i is null or i.since is null or i.until is null or i.until <= i.since
  ) then
    raise exception 'every event requires a valid time range';
  end if;

  if exists (select i.id from unnest(events) i where i.id is not null group by i.id having count(*) > 1) then
    raise exception 'an event may only be submitted once';
  end if;

  if exists (
    select 1
    from unnest(events) i
    cross join lateral unnest(coalesce(i.registrations, '{}'::event_registration_input[])) registration
    where (registration.person_id is null) = (registration.couple_id is null)
  ) then
    raise exception 'an event registration requires exactly one person or couple';
  end if;

  if exists (
    select 1
    from unnest(coalesce(trainers, '{}'::event_trainer_input[])) trainer
    where trainer.person_id is null or trainer.lessons_offered < 0
  ) then
    raise exception 'an event trainer requires a person and a non-negative lesson limit';
  end if;

  if exists (
    select 1 from unnest(events) i
    cross join lateral unnest(i.registrations) registration
    where registration.target_cohort_id is not null and (
      registration.person_id is null or registration.is_cancelled
      or not registration.target_cohort_id = any(coalesce(cohort_ids, '{}'))
    )
  ) then
    raise exception 'a cohort registration requires an active person and a selected cohort' using errcode = '22023';
  end if;

  if exists (
    select 1
    from unnest(coalesce(cohort_ids, '{}'::bigint[])) i(cohort_id)
    left join cohort cohort on cohort.id = i.cohort_id and cohort.tenant_id = v_tenant_id
    where i.cohort_id is not null and cohort.id is null
  ) then
    raise exception 'event cohort not found';
  end if;

  if exists (
    select 1 from unnest(events) i where i.id is null
  ) and (details).parent_id is not null and not exists (
    select 1
    from event parent
    where parent.id = (details).parent_id and parent.tenant_id = v_tenant_id
  ) then
    raise exception 'event parent % not found or not editable', (details).parent_id;
  end if;

  if v_assign_series then
    if (series).id is null then
      insert into event_series (name)
      values (coalesce((series).name, (details).name))
      returning id into v_series_id;
    else
      select e.id into v_series_id
      from event_series e where e.id = (series).id and e.tenant_id = v_tenant_id
      for update;

      if not found then
        raise exception 'event series % not found or not editable', (series).id;
      end if;
    end if;
  end if;

  select count(*) into v_expected_existing_count from unnest(events) i where i.id is not null;

  perform e.id
  from event e
  join unnest(events) i on i.id = e.id
  where i.id is not null and e.tenant_id = v_tenant_id
  order by e.id
  for update of e;

  get diagnostics v_locked_existing_count = row_count;
  if v_locked_existing_count <> v_expected_existing_count then
    raise exception 'one or more events were not found or are not editable';
  end if;

  foreach event_to_save in array events loop
    if event_to_save.id is null then
      insert into event (
        parent_id,
        series_id,
        since,
        until,
        is_cancelled,
        name,
        type,
        location_id,
        location_text,
        capacity,
        capacity_unit,
        is_visible,
        is_public,
        has_public_details,
        is_locked,
        enable_notes,
        description,
        summary,
        files_legacy
      ) values (
        (details).parent_id,
        v_series_id,
        event_to_save.since,
        event_to_save.until,
        coalesce(event_to_save.is_cancelled, false),
        (details).name,
        (details).type,
        (details).location_id,
        coalesce((details).location_text, ''),
        (details).capacity,
        (details).capacity_unit,
        v_is_visible,
        v_is_public,
        v_has_public_details,
        v_is_locked,
        v_enable_notes,
        '',
        '',
        ''
      )
      returning * into v_saved_event;
    else
      update event e
      set since = event_to_save.since,
          until = event_to_save.until,
          is_cancelled = coalesce(event_to_save.is_cancelled, false),
          name = (details).name,
          type = (details).type,
          location_id = (details).location_id,
          location_text = coalesce((details).location_text, ''),
          capacity = (details).capacity,
          capacity_unit = (details).capacity_unit,
          is_visible = v_is_visible,
          is_public = v_is_public,
          has_public_details = v_has_public_details,
          is_locked = v_is_locked,
          enable_notes = v_enable_notes,
          series_id = case
            when v_assign_series then v_series_id
            else e.series_id
          end
      where e.id = event_to_save.id and e.tenant_id = v_tenant_id
      returning * into v_saved_event;

      if not found then
        raise exception 'event % not found or not editable', event_to_save.id;
      end if;
    end if;

    v_saved_event_ids := array_append(v_saved_event_ids, v_saved_event.id);

    perform registration.id
    from event_registration registration
    where registration.event_id = v_saved_event.id
    order by registration.id
    for update;

    -- Release person slots before replacing couples with individuals or vice versa.
    with removed as (
      select registration.id
      from event_registration registration
      where registration.event_id = v_saved_event.id
        and registration.parent_registration_id is null
        and not exists (
          select 1 from unnest(event_to_save.registrations) desired
          where desired.person_id is not distinct from registration.person_id
            and desired.couple_id is not distinct from registration.couple_id
            and not coalesce(desired.is_cancelled, false)
        )
    )
    update event_registration registration
    set registration_status = 'cancelled',
        source = case when registration.parent_registration_id is null and registration.source is distinct from 'cohort'
          then 'manager'::event_registration_source else registration.source end
    from removed
    where registration.registration_status = 'active'
      and (registration.id = removed.id or registration.parent_registration_id = removed.id);

    -- Detach person rows from cancelled registrations when submitted as individuals.
    update event_registration registration
    set parent_registration_id = null
    where registration.id in (
      select distinct on (r.person_id) r.id
      from event_registration r
      join event_registration parent on parent.id = r.parent_registration_id
      where r.event_id = v_saved_event.id and parent.registration_status = 'cancelled'
        and exists (select 1 from unnest(event_to_save.registrations) desired where desired.person_id = r.person_id)
        and not exists (select 1 from event_registration standalone
          where standalone.event_id = r.event_id and standalone.person_id = r.person_id
            and standalone.parent_registration_id is null)
      order by r.person_id, r.id desc
    );

    -- Save the displayed list and explicit removals; reuse rows to preserve attendance.
    insert into event_registration as registration (
      event_id, person_id, couple_id, target_cohort_id, source, registration_status, status
    )
    select v_saved_event.id, choice.person_id, choice.couple_id, choice.target_cohort_id,
      case when choice.target_cohort_id is null then 'manager'::event_registration_source else 'cohort'::event_registration_source end,
      case when choice.is_cancelled then 'cancelled'::event_registration_status else 'active'::event_registration_status end,
      case when choice.person_id is not null then 'unknown'::attendance_type end
    from (
      select distinct on (choice.person_id, choice.couple_id) choice.*
      from unnest(event_to_save.registrations) with ordinality
        choice(person_id, couple_id, target_cohort_id, is_cancelled, position)
      order by choice.person_id, choice.couple_id, choice.position
    ) choice
    on conflict (event_id, couple_id, person_id) where parent_registration_id is null
    do update set registration_status = excluded.registration_status,
      source = excluded.source,
      target_cohort_id = excluded.target_cohort_id
    where (registration.registration_status, registration.target_cohort_id)
      is distinct from (excluded.registration_status, excluded.target_cohort_id);

    -- Cancel children of cancelled registrations; reuse person rows for active couple registrations.
    update event_registration child
    set registration_status = parent.registration_status, source = parent.source
    from event_registration parent
    where parent.event_id = v_saved_event.id
      and child.parent_registration_id = parent.id
      and parent.registration_status = 'cancelled'
      and (child.registration_status, child.source) is distinct from (parent.registration_status, parent.source);

    perform app_private.ensure_couple_person_registrations(registration.id)
    from event_registration registration
    where registration.event_id = v_saved_event.id
      and registration.couple_id is not null
      and registration.registration_status = 'active';
    insert into event_target_cohort (tenant_id, event_id, cohort_id)
    select v_tenant_id, v_saved_event.id, id
    from unnest(cohort_ids) cohort(id)
    where id is not null
    on conflict (event_id, cohort_id) do nothing;

    delete from event_target_cohort target
    where target.event_id = v_saved_event.id
      and not exists (select 1 from unnest(cohort_ids) cohort(id) where id = target.cohort_id);

  end loop;

  -- Keep the caller's trainer assignment until all edits and replacements are saved.
  with desired as (
    select distinct on (trainer.person_id) trainer.person_id, trainer.lessons_offered
    from unnest(coalesce(trainers, '{}'::event_trainer_input[]))
      with ordinality trainer(person_id, lessons_offered, position)
    order by trainer.person_id, trainer.position
  )
  insert into event_trainer (tenant_id, event_id, person_id, lessons_offered)
  select stored_event.tenant_id, stored_event.id, desired.person_id, desired.lessons_offered
  from event stored_event
  join unnest(v_saved_event_ids) saved(id) on saved.id = stored_event.id
  cross join desired
  on conflict (event_id, person_id) do update
  set lessons_offered = excluded.lessons_offered;

  delete from event_trainer e
  where e.event_id = any(v_saved_event_ids)
    and not exists (
      select 1
      from unnest(coalesce(trainers, '{}'::event_trainer_input[])) trainer
      where trainer.person_id = e.person_id
    );

  return query
  select stored_event.*
  from unnest(v_saved_event_ids) with ordinality saved(event_id, position)
  join event stored_event on stored_event.id = saved.event_id
  order by saved.position;
end;
$$;


--
-- Name: FUNCTION save_events(details public.event_details_input, events public.event_input[], trainers public.event_trainer_input[], cohort_ids bigint[], series public.event_series_input); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.save_events(details public.event_details_input, events public.event_input[], trainers public.event_trainer_input[], cohort_ids bigint[], series public.event_series_input) IS '@behavior +list -connection';


--
-- Name: scoreboard_entries(date, date, bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.scoreboard_entries(since date, until date, cohort_id bigint DEFAULT NULL::bigint) RETURNS SETOF public.scoreboard_record
    LANGUAGE sql STABLE
    AS $$
  with membership as (
    select cm.person_id, cm.cohort_id
    from current_cohort_membership cm
    where (cm.cohort_id = scoreboard_entries.cohort_id or scoreboard_entries.cohort_id is null)
  ),
  instances as (
    select
      i.id as event_id,
      i.since,
      i.until,
      i.type as event_type
    from event i
    where not i.is_cancelled
      and i.since >= scoreboard_entries.since::timestamptz
      and i.until  < scoreboard_entries.until::timestamptz
      and i.type <> 'reservation'
  ),
  member_people as (
    select distinct person_id
    from membership
  ),
  attendance as materialized (
    select
      ea.person_id,
      scoreboard_entries.cohort_id as cohort_id,
      case when inst.event_type = 'lesson' then 1 else 0 end as lesson_score,
      case when inst.event_type = 'group' then floor((extract(epoch from (inst.until - inst.since)) / 60)::numeric / 45::numeric) else 0 end as group_score,
      case when inst.event_type = 'camp'  then 3 + 2 * ((extract(epoch from (inst.until - inst.since)) > 86400)::int) else 0 end as event_score,
      date_trunc('day', inst.since)::date as day
    from instances inst
    join event_registration ea
      on ea.event_id = inst.event_id and ea.person_id is not null
    left join event_registration registration
      on registration.id = ea.parent_registration_id
    left join cohort tc
      on tc.id = coalesce(ea.target_cohort_id, registration.target_cohort_id)
    where
      ea.registration_status = 'active'
      and (ea.status = 'attended' or inst.event_type = 'lesson')
      and ea.person_id = any (select person_id from member_people)
      and (scoreboard_entries.cohort_id is null or tc.id = scoreboard_entries.cohort_id)
  ),
  per_day as (
    select
      person_id,
      cohort_id,
      least(sum(lesson_score), 4) as lesson_score,
      sum(group_score) as group_score,
      sum(event_score) as event_score
    from attendance
    group by person_id, cohort_id, day
  ),
  aggregated as (
    select
      person_id,
      cohort_id,
      coalesce(sum(lesson_score), 0)::bigint as lesson_total_score,
      coalesce(sum(group_score), 0)::bigint as group_total_score,
      coalesce(sum(event_score), 0)::bigint as event_total_score
    from per_day
    group by person_id, cohort_id
  ),
  manual as (
    select
      sma.person_id,
      case
        when scoreboard_entries.cohort_id is null then null
        else coalesce(sma.cohort_id, scoreboard_entries.cohort_id)
      end as cohort_id,
      sum(sma.points)::bigint as manual_total_score
    from scoreboard_manual_adjustment sma
    where sma.tenant_id = current_tenant_id()
      and sma.awarded_at >= scoreboard_entries.since
      and sma.awarded_at < scoreboard_entries.until
      and (
        scoreboard_entries.cohort_id is null or
        sma.cohort_id is null or
        sma.cohort_id = scoreboard_entries.cohort_id
      )
      and exists (
        select 1
        from membership mem
        where mem.person_id = sma.person_id
          and (
            (scoreboard_entries.cohort_id is null and sma.cohort_id is null)
            or mem.cohort_id is not distinct from coalesce(sma.cohort_id, scoreboard_entries.cohort_id)
          )
      )
    group by 1, 2
  ),
  totals as (
    select
      coalesce(a.person_id, m.person_id) as person_id,
      coalesce(a.cohort_id, m.cohort_id) as cohort_id,
      coalesce(a.lesson_total_score, 0)::bigint as lesson_total_score,
      coalesce(a.group_total_score, 0)::bigint as group_total_score,
      coalesce(a.event_total_score, 0)::bigint as event_total_score,
      coalesce(m.manual_total_score, 0)::bigint as manual_total_score,
      (
        coalesce(a.lesson_total_score, 0)::bigint +
        coalesce(a.group_total_score, 0)::bigint +
        coalesce(a.event_total_score, 0)::bigint +
        coalesce(m.manual_total_score, 0)::bigint
      ) as total_score
    from aggregated a
    full join manual m using (person_id, cohort_id)
  )
  select
    person_id,
    cohort_id,
    lesson_total_score,
    group_total_score,
    event_total_score,
    manual_total_score,
    total_score,
    rank() over (order by total_score desc, person_id) as ranking
  from totals
  order by total_score desc, person_id;
$$;


--
-- Name: FUNCTION scoreboard_entries(since date, until date, cohort_id bigint); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.scoreboard_entries(since date, until date, cohort_id bigint) IS '@behavior +list -connection';


--
-- Name: set_event_instance_registration(bigint, bigint, bigint, boolean, text, bigint[], integer[], public.event_registration_person_input); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.set_event_instance_registration(p_instance_id bigint, p_person_id bigint, p_couple_id bigint, p_is_registered boolean, p_note text DEFAULT NULL::text, p_lesson_trainer_ids bigint[] DEFAULT NULL::bigint[], p_lesson_counts integer[] DEFAULT NULL::integer[], p_person public.event_registration_person_input DEFAULT NULL::public.event_registration_person_input) RETURNS public.event_registration
    LANGUAGE sql
    AS $_$
  select public.set_event_registration($1, $2, $3, $4, $5, $6, $7, $8);
$_$;


--
-- Name: FUNCTION set_event_instance_registration(p_instance_id bigint, p_person_id bigint, p_couple_id bigint, p_is_registered boolean, p_note text, p_lesson_trainer_ids bigint[], p_lesson_counts integer[], p_person public.event_registration_person_input); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.set_event_instance_registration(p_instance_id bigint, p_person_id bigint, p_couple_id bigint, p_is_registered boolean, p_note text, p_lesson_trainer_ids bigint[], p_lesson_counts integer[], p_person public.event_registration_person_input) IS '@deprecated Use setEventRegistration.
@resultFieldName eventInstanceRegistration';


--
-- Name: set_event_registration(bigint, bigint, bigint, boolean, text, bigint[], integer[], public.event_registration_person_input); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.set_event_registration(event_id bigint, person_id bigint, couple_id bigint, is_registered boolean, note text DEFAULT NULL::text, lesson_trainer_ids bigint[] DEFAULT NULL::bigint[], lesson_counts integer[] DEFAULT NULL::integer[], person public.event_registration_person_input DEFAULT NULL::public.event_registration_person_input) RETURNS public.event_registration
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $_$
declare
  target_instance event;
  registration event_registration;
  registration_found boolean;
  is_self boolean;
  is_manager boolean;
  registration_source event_registration_source;
  required_capacity integer;
  remaining_capacity integer;
  request record;
begin
  if is_registered is null or num_nonnulls(person_id, couple_id, person) <> 1
    or (person is distinct from null and not is_registered) then
    raise exception 'INVALID_REGISTRANT' using errcode = '22023';
  end if;

  if num_nonnulls(lesson_trainer_ids, lesson_counts) = 1
    or cardinality(lesson_trainer_ids) <> cardinality(lesson_counts)
    or exists (
      select 1
      from unnest(lesson_trainer_ids, lesson_counts) demand(trainer_id, lesson_count)
      where demand.trainer_id is null
        or demand.lesson_count is null
        or demand.lesson_count < 0
    ) then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;
  if not is_registered and lesson_trainer_ids is not null then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;

  select * into target_instance
  from event
  where id = event_id
    and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;

  is_manager := pg_has_role(current_setting('role'), 'administrator', 'member')
    or (pg_has_role(current_setting('role'), 'trainer', 'member') and app_private.can_trainer_edit_event(event_id));

  if person is distinct from null then
    if nullif(btrim(person.first_name), '') is null
      or nullif(btrim(person.last_name), '') is null
      or person.gender is null
      or nullif(btrim(person.nationality), '') is null
      or person.email is null or person.email !~ '^[^[:space:]@]+@[^[:space:]@]+\.[^[:space:]@]+$'
      or nullif(btrim(person.phone), '') is null
      or person.birth_date > current_date
      or person.csts_id <= 0 or person.wdsf_id <= 0 then
      raise exception 'INVALID_PERSON_DETAILS' using errcode = '22023';
    end if;

    select r.* into registration
    from event_registration r
    join person p on p.id = r.person_id
    where r.event_id = set_event_registration.event_id
      and r.tenant_id = current_tenant_id()
      and r.created_by is not distinct from current_user_id()
      and r.registration_status = 'active'
      and r.parent_registration_id is null
      and r.note is not distinct from set_event_registration.note
      and row(
        p.first_name, p.last_name, p.gender, p.nationality,
        p.email, p.phone, p.birth_date, p.csts_id, p.wdsf_id
      ) is not distinct from row(
        btrim((set_event_registration.person).first_name), btrim((set_event_registration.person).last_name),
        (set_event_registration.person).gender, (set_event_registration.person).nationality,
        btrim((set_event_registration.person).email), btrim((set_event_registration.person).phone),
        (set_event_registration.person).birth_date, (set_event_registration.person).csts_id,
        (set_event_registration.person).wdsf_id
      )
      and coalesce((
        select jsonb_object_agg(d.trainer_id, d.lesson_count)
        from event_lesson_request d where d.registration_id = r.id
      ), '{}'::jsonb) = coalesce((
        select jsonb_object_agg(d.trainer_id, d.lesson_count)
        from unnest(lesson_trainer_ids, lesson_counts) d(trainer_id, lesson_count)
        where d.lesson_count > 0
      ), '{}'::jsonb)
    order by r.id
    limit 1;

    if found then
      return registration;
    end if;

    insert into person (
      first_name, last_name, gender, nationality, email, phone, birth_date, csts_id, wdsf_id
    ) values (
      btrim((set_event_registration.person).first_name), btrim((set_event_registration.person).last_name),
      (set_event_registration.person).gender, (set_event_registration.person).nationality,
      btrim((set_event_registration.person).email), btrim((set_event_registration.person).phone),
      (set_event_registration.person).birth_date, (set_event_registration.person).csts_id,
      (set_event_registration.person).wdsf_id
    )
    returning id into person_id;

    is_self := not is_manager;
  else
    is_self := coalesce(person_id = any(current_person_ids()), false)
      or coalesce(couple_id = any(current_couple_ids()), false);
  end if;

  if not is_self and not is_manager then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;
  if target_instance.is_locked and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  registration_source := case when is_self
    then 'self'::event_registration_source
    else 'manager'::event_registration_source
  end;

  select * into registration
  from event_registration r
  where r.event_id = set_event_registration.event_id
    and r.person_id is not distinct from set_event_registration.person_id
    and r.couple_id is not distinct from set_event_registration.couple_id
  order by (r.registration_status = 'active') desc, (r.parent_registration_id is null) desc, r.id desc
  limit 1;
  registration_found := found;

  if not is_registered then
    if not registration_found then
      raise exception 'REGISTRATION_NOT_FOUND' using errcode = '22023';
    end if;

    update event_registration
    set registration_status = 'cancelled',
        target_cohort_id = null,
        source = registration_source
    where id = registration.id or parent_registration_id = registration.id;

    select * into registration from event_registration where id = registration.id;
    return registration;
  end if;

  if registration_found and registration.registration_status = 'active' then
    if note is not null then
      update event_registration set note = set_event_registration.note where id = registration.id
      returning * into registration;
    end if;
  else
    if not is_manager and (
      target_instance.is_cancelled
        or target_instance.until <= now()
        or not (
          coalesce(target_instance.is_public, false)
          or (
            coalesce(target_instance.is_visible, false)
            and current_tenant_id() = any(my_tenants_array())
          )
        )
    ) then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;

    if not is_manager then
      remaining_capacity := event_remaining_person_spots(target_instance);
      required_capacity := case
        when target_instance.capacity_unit = 'people' and couple_id is not null then 2
        when target_instance.capacity_unit = 'registrations' and exists (
          select 1 from event_registration parent
          where parent.id = registration.parent_registration_id and parent.registration_status = 'active'
        ) then 0
        else 1
      end;
      if couple_id is not null then
        required_capacity := required_capacity - (
          select count(*)::integer from event_registration r
          join couple c on c.id = set_event_registration.couple_id and r.person_id in (c.man_id, c.woman_id)
          where r.event_id = set_event_registration.event_id and r.registration_status = 'active'
            and (target_instance.capacity_unit = 'people' or r.parent_registration_id is null)
        );
      end if;
      if remaining_capacity is not null and remaining_capacity < required_capacity then
        raise exception 'CAPACITY_EXCEEDED' using errcode = '22023';
      end if;
    end if;

    if registration_found then
      update event_registration
      set registration_status = 'active',
          target_cohort_id = null,
          source = registration_source,
          parent_registration_id = (
            select parent.id from event_registration parent
            where parent.id = registration.parent_registration_id and parent.registration_status = 'active'
          )
      where id = registration.id;

      if note is not null then
        update event_registration set note = set_event_registration.note where id = registration.id;
      end if;
    else
      insert into event_registration (
        event_id, person_id, couple_id, source, status, note
      ) values (
        event_id, set_event_registration.person_id, set_event_registration.couple_id, registration_source,
        case when set_event_registration.person_id is not null then 'unknown'::attendance_type end,
        set_event_registration.note
      ) returning * into registration;
    end if;
  end if;

  if couple_id is not null then
    perform app_private.ensure_couple_person_registrations(registration.id);
  end if;

  select * into registration from event_registration where id = registration.id;

  if lesson_trainer_ids is not null then
    delete from event_lesson_request
    where registration_id = registration.id
      and trainer_id <> all(lesson_trainer_ids);

    for request in
      select demand.trainer_id, demand.lesson_count
      from unnest(lesson_trainer_ids, lesson_counts)
        demand(trainer_id, lesson_count)
      order by demand.trainer_id
    loop
      perform app_private.set_lesson_request(registration, request.trainer_id, request.lesson_count);
    end loop;
  end if;

  if person is distinct from null then
    insert into person_invitation (person_id, email, event_id)
    values (registration.person_id, btrim((set_event_registration.person).email), event_id);
  end if;

  return registration;
end;
$_$;


--
-- Name: set_event_sharing(bigint, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.set_event_sharing(id bigint, p_enabled boolean) RETURNS text
    LANGUAGE plpgsql
    AS $_$
declare
  v_share_token text;
begin
  update event instance
  set share_token = case
    when not coalesce(p_enabled, false) then null
    when instance.share_token is null
      then translate(encode(gen_random_bytes(24), 'base64'), '+/', '-_')
    else instance.share_token
  end
  where instance.id = $1
  returning instance.share_token into v_share_token;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;

  return v_share_token;
end;
$_$;


--
-- Name: set_lesson_demand(bigint, bigint, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.set_lesson_demand(instance_registration_id bigint, instance_trainer_id bigint, lesson_count integer) RETURNS public.event_lesson_request
    LANGUAGE sql STRICT
    AS $_$
  select public.set_lesson_request($1, $2, $3);
$_$;


--
-- Name: FUNCTION set_lesson_demand(instance_registration_id bigint, instance_trainer_id bigint, lesson_count integer); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.set_lesson_demand(instance_registration_id bigint, instance_trainer_id bigint, lesson_count integer) IS '@deprecated Use setLessonRequest.
@resultFieldName eventLessonDemand';


--
-- Name: set_lesson_request(bigint, bigint, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.set_lesson_request(registration_id bigint, trainer_id bigint, lesson_count integer) RETURNS public.event_lesson_request
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $_$
declare
  registration event_registration;
  instance event;
  is_manager boolean;
  is_self boolean;
begin
  select * into registration
  from event_registration
  where id = $1;

  if not found then
    raise exception 'REGISTRATION_NOT_FOUND' using errcode = '28000';
  end if;

  select * into instance
  from event
  where id = registration.event_id and tenant_id = current_tenant_id();

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '28000';
  end if;

  is_self := registration.person_id = any(current_person_ids())
    or registration.couple_id = any(current_couple_ids());
  is_manager := pg_has_role(current_setting('role'), 'administrator', 'member')
    or (pg_has_role(current_setting('role'), 'trainer', 'member') and app_private.can_trainer_edit_event(instance.id));

  if not coalesce(is_self, false) and not coalesce(is_manager, false) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  if instance.is_locked and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  return app_private.set_lesson_request(registration, $2, $3);
end;
$_$;


--
-- Name: submit_form(text, jsonb, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.submit_form(type text, data jsonb, url text) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  v_email text;
begin
  insert into form_responses (type, data, url) values (type, data, url);

  if current_tenant_id() = 1 then
    foreach v_email in array (array['m.hyzova96@seznam.cz', 'miroslav.hyza@tkolymp.cz', 'hyzam@tkolymp.cz', 'filip.karasek@tkolymp.cz']) loop
      perform graphile_worker.add_job(
        'send_email',
        json_build_object(
          'template', 'notify_submitted_form.mjml',
          'options', json_build_object(
          'to', v_email,
          'subject', 'Nový vyplněný formulář z webu'
        ),
        'variables', json_build_object(
          'url', url,
          'data', data
        )
      ));
    end loop;
  end if;
end;
$$;


--
-- Name: suggested_competition_matches(date, date, bigint[], bigint, bigint, bigint[], boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.suggested_competition_matches(since date, until date DEFAULT NULL::date, person_ids bigint[] DEFAULT NULL::bigint[], couple_id bigint DEFAULT NULL::bigint, cohort_id bigint DEFAULT NULL::bigint, event_ids bigint[] DEFAULT NULL::bigint[], registered_only boolean DEFAULT false) RETURNS TABLE(competition_id bigint, competitor_id text, local_person_id bigint, local_couple_id bigint, status text)
    LANGUAGE sql STABLE
    AS $$
  with scoped_people as materialized (
    select p.id
    from person p
    where (person_ids is not null or couple_id is not null or p.id in (
        select person_id from current_tenant_membership
        union all
        select person_id from current_tenant_trainer
        union all
        select person_id from current_tenant_administrator
      ))
      and (person_ids is null or p.id = any(person_ids))
      and (cohort_id is null or exists (
        select 1 from current_cohort_membership cm
        where cm.person_id = p.id and cm.cohort_id = suggested_competition_matches.cohort_id
      ))
      and (couple_id is null or exists (
        select 1 from couple c
        where c.id = suggested_competition_matches.couple_id and p.id in (c.man_id, c.woman_id)
      ))
  ), scoped_couples as materialized (
    select c.id
    from couple c
    where c.status = 'active'
      and (couple_id is null or c.id = couple_id)
      and (c.man_id in (select id from scoped_people) or c.woman_id in (select id from scoped_people))
  ), scoped_competitors as materialized (
    select fc.id, fc.federation, fc.competitor_type, array(
      select distinct case
        when license.discipline::text like 'pd_%' then 'Professional'
        when license.discipline = 'hiphop' then 'HipHop'
        when license.discipline in ('breaking', 'caribbean', 'stage', 'smooth', 'disco')
          then initcap(license.discipline::text)
        else 'General'
      end
      from federated.person_license license
      where license.person_id = fp.id and license.federation = 'wdsf'
        and license.kind = 'athlete' and license.discipline <> 'unknown'
    ) as divisions, fp.age_group_code, link.person_id as local_person_id, null::bigint as local_couple_id
    from person_federation_link link
    join federated.person fp on (fp.federation, fp.external_id) = (link.federation, link.external_id)
    join federated.competitor_component member on member.person_id = fp.id
    join federated.competitor fc on fc.id = member.competitor_id
    where couple_id is null and fc.competitor_type = 'solo'
      and link.person_id = any(array(select id from scoped_people))
    union
    select fc.id, fc.federation, fc.competitor_type, fc.divisions, fc.age_group_code,
      null::bigint, link.couple_id
    from couple_federation_link link
    join federated.competitor fc on fc.id = link.competitor_id
    where link.couple_id = any(array(select id from scoped_couples))
  ), profiles as materialized (
    select sc.local_person_id, sc.local_couple_id, sc.id, sc.federation, sc.competitor_type, sc.divisions, sc.age_group_code,
      count(*) = count(coalesce(extract(year from p.birth_date)::int, extract(year from fp.dob)::int, fp.birth_year)) as known_birth_years,
      min(coalesce(extract(year from p.birth_date)::int, extract(year from fp.dob)::int, fp.birth_year)) as older_birth_year,
      max(coalesce(extract(year from p.birth_date)::int, extract(year from fp.dob)::int, fp.birth_year)) as younger_birth_year,
      bool_and(coalesce(fp.gender = 'female', false)) as all_female,
      bool_and(coalesce(fp.gender = 'male', false)) as all_male
    from scoped_competitors sc
    join federated.competitor_component cc on cc.competitor_id = sc.id
    join federated.person fp on fp.id = cc.person_id
    left join person_federation_link link on (link.federation, link.external_id) = (fp.federation, fp.external_id)
    left join person p on p.id = link.person_id
    group by sc.local_person_id, sc.local_couple_id, sc.id, sc.federation, sc.competitor_type, sc.divisions, sc.age_group_code
  ), upcoming as materialized (
    select array_agg(c.id) as competition_ids, c.federation, c.competition_type, extract(year from c.start_date)::int as year,
      cat.series, cat.discipline, cat.class, cat.gender_group, cat.competitor_type, cat.age_group_code
    from federated.competition c
    join federated.event e on e.id = c.event_id
    join federated.category cat on cat.id = c.category_id
    where c.federation in ('csts', 'wdsf') and c.start_date >= since and (until is null or c.start_date < until)
      and (event_ids is null or c.event_id = any(event_ids))
      and c.status <> 'cancelled' and e.status <> 'cancelled'
    group by c.federation, c.competition_type, extract(year from c.start_date)::int, cat.id
  ), participations as materialized (
    select p.*
    from federated.competition_participation p
    where p.competition_id = any(array(select unnest(competition_ids) from upcoming))
      and p.competitor_id = any(array(select id from scoped_competitors))
  ), source_profiles as materialized (
    select p.*, source.series, source.discipline, source.class, source.age_group_code as reported_age
    from profiles p
    join federated.competitor_category_progress progress on progress.competitor_id = p.id
    join federated.category source on source.id = progress.category_id
      and source.competitor_type = p.competitor_type
    where p.federation = 'csts'
    union all
    -- WDSF divisions are independent of ČSTS proficiency classes and progress.
    select p.*, division, null, null, p.age_group_code
    from profiles p cross join unnest(p.divisions) division
    where p.federation = 'wdsf'
  ), category_profiles as materialized (
    select p.*, y.year, age.code as age_group, age.allowed_to_dance_in
    from source_profiles p
    cross join (select distinct year from upcoming) y
    left join lateral (
      select r.age_group_code as code
      from federated.couple_age_rule r
      where p.competitor_type = 'couple' and p.known_birth_years
        and r.division = 'General' and r.age_group_code <> 'Under_21'
        and r.older_age @> (y.year - p.older_birth_year)
        and r.younger_age @> (y.year - p.younger_birth_year)
      order by lower(r.older_age) desc nulls last, upper(r.older_age)
      limit 1
    ) couple_age on true
    left join federated.age_group person_age on p.competitor_type = 'solo' and p.known_birth_years
      and person_age.person_age @> (y.year - p.older_birth_year)
    join federated.age_group age on age.code = case
      when p.known_birth_years then coalesce(couple_age.code, person_age.code)
      when y.year = extract(year from current_date)::int then p.reported_age
    end
  ), eligible as (
  select distinct competition_id, p.id, p.local_person_id, p.local_couple_id
  from upcoming target
  join category_profiles p on p.year = target.year and p.federation = target.federation
    and (p.series = target.series or (p.federation = 'wdsf' and target.series = 'Any'))
    and (p.federation = 'wdsf' or p.discipline = target.discipline)
    and p.competitor_type = target.competitor_type
  cross join lateral unnest(target.competition_ids) competition_id
  where (target.gender_group in ('', 'mixed', 'SOLO')
      or (target.gender_group in ('female', 'F', '1 F') and p.all_female)
      or (target.gender_group in ('male', 'M', '1 M') and p.all_male))
    -- ČSTS leagues require B or higher, even though their categories say Open.
    and (p.federation <> 'csts' or target.competition_type not in ('league', 'super_league')
      or p.class in ('B', 'A', 'M', 'S'))
    and (p.federation = 'wdsf' or
      target.class in ('', 'Open') or target.class = p.class
      or (federated.class_rank(p.class) > 0
        and federated.class_rank(p.class) = federated.class_rank(target.class))
      or (target.class like '%-%'
        and federated.class_rank(split_part(target.class, '-', 1)) > 0
        and federated.class_rank(split_part(target.class, '-', 2)) > 0
        -- A range cannot cross between medal and letter classes.
        and (federated.class_rank(split_part(target.class, '-', 1)) > 10)
          = (federated.class_rank(split_part(target.class, '-', 2)) > 10)
        and federated.class_rank(p.class) between
          federated.class_rank(split_part(target.class, '-', 1)) and
          federated.class_rank(split_part(target.class, '-', 2)))
    )
    and (target.age_group_code = p.age_group or target.age_group_code = any(p.allowed_to_dance_in))
    -- Adult's allowances include U21; that is only valid below its upper age limit.
    and (target.age_group_code <> 'Under_21' or
      (p.known_birth_years and target.year - p.older_birth_year < 21) or
      (not p.known_birth_years and p.age_group in ('Youth', 'Under_21')))
  )
  select eligible.*, case
    when participation.cancelled then 'cancelled'
    when participation.competition_id is not null then 'registered'
    else 'suggested'
  end
  from eligible
  left join participations participation
    on (participation.competition_id, participation.competitor_id) = (eligible.competition_id, eligible.id)
  where not registered_only
  union
  select participation.competition_id, sc.id, sc.local_person_id, sc.local_couple_id,
    case when participation.cancelled then 'cancelled' else 'registered' end
  from scoped_competitors sc
  join participations participation on participation.competitor_id = sc.id
  where not registered_only or not participation.cancelled;
$$;


--
-- Name: FUNCTION suggested_competition_matches(since date, until date, person_ids bigint[], couple_id bigint, cohort_id bigint, event_ids bigint[], registered_only boolean); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.suggested_competition_matches(since date, until date, person_ids bigint[], couple_id bigint, cohort_id bigint, event_ids bigint[], registered_only boolean) IS '@behavior -*';


--
-- Name: sync_cohort_memberships(bigint, bigint[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.sync_cohort_memberships(person_id bigint, cohort_ids bigint[]) RETURNS void
    LANGUAGE sql
    AS $_$
  update cohort_membership set until = now(), status = 'expired'
  where status = 'active' and person_id = $1 and cohort_id <> all (cohort_ids);

  insert into cohort_membership (status, since, person_id, cohort_id)
  select 'active', NOW(), $1, new_cohort_id
  from unnest(cohort_ids) as x(new_cohort_id)
  where not exists (select 1 from cohort_membership where status = 'active' and person_id = $1 and cohort_id = new_cohort_id);
$_$;


--
-- Name: system_admin_tenants(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.system_admin_tenants() RETURNS TABLE(id bigint, name text, description text, bank_account text, origins text[], cz_ico text, cz_dic text, address public.address_domain, settings text, membership_count bigint, trainer_count bigint, administrator_count bigint, session_count_last_30_days bigint, session_count_per_trainer_last_30_days double precision)
    LANGUAGE plpgsql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  if not app_private.is_system_admin(current_user_id()) then
    raise exception 'permission denied for system admin tenant overview'
      using errcode = '42501';
  end if;

  return query
  select
    t.id,
    t.name,
    t.description,
    t.bank_account,
    t.origins,
    t.cz_ico,
    t.cz_dic,
    t.address,
    coalesce(ts.settings::text, '{}'),
    membership_counts.membership_count,
    staffing.trainer_count,
    administrators.administrator_count,
    load.session_count_last_30_days,
    load.session_count_per_trainer_last_30_days
  from tenant t
  left join tenant_settings ts on ts.tenant_id = t.id
  cross join lateral (
    select count(*) as membership_count from tenant_membership tm where tm.tenant_id = t.id and tm.status = 'active'
  ) as membership_counts
  cross join lateral (
    select count(*) as trainer_count from tenant_trainer tt where tt.tenant_id = t.id and tt.status = 'active'
  ) as staffing
  cross join lateral (
    select count(*) as administrator_count from tenant_administrator ta where ta.tenant_id = t.id and ta.status = 'active'
  ) as administrators
  cross join lateral (
    select
      count(*) as session_count_last_30_days,
      case
        when coalesce(staffing.trainer_count, 0) > 0 then count(*)::double precision / staffing.trainer_count::double precision
        else 0::double precision
      end as session_count_per_trainer_last_30_days
    from event ei
    where ei.tenant_id = t.id
      and coalesce(ei.is_cancelled, false) = false
      and ei.since >= now() - interval '30 days'
  ) as load
  order by t.name;
end;
$$;


--
-- Name: system_admin_update_tenant(bigint, text, text, text, text[], public.address_domain, text, text, jsonb); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.system_admin_update_tenant(tenant_id bigint, name text DEFAULT NULL::text, description text DEFAULT NULL::text, bank_account text DEFAULT NULL::text, origins text[] DEFAULT NULL::text[], address public.address_domain DEFAULT NULL::public.address_type, cz_ico text DEFAULT NULL::text, cz_dic text DEFAULT NULL::text, settings jsonb DEFAULT NULL::jsonb) RETURNS public.tenant
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'public', 'pg_temp'
    AS $$
declare
  v_tenant tenant;
begin
  if not app_private.is_system_admin(current_user_id()) then
    raise exception 'permission denied for system admin tenant update'
      using errcode = '42501';
  end if;

  update tenant t
  set
    name = coalesce(system_admin_update_tenant.name, t.name),
    description = coalesce(system_admin_update_tenant.description, t.description),
    bank_account = coalesce(system_admin_update_tenant.bank_account, t.bank_account),
    origins = coalesce(system_admin_update_tenant.origins, t.origins),
    address = coalesce(system_admin_update_tenant.address, t.address),
    cz_ico = coalesce(system_admin_update_tenant.cz_ico, t.cz_ico),
    cz_dic = coalesce(system_admin_update_tenant.cz_dic, t.cz_dic)
  where t.id = tenant_id
  returning t.* into v_tenant;

  if not found then
    raise exception 'tenant % not found', tenant_id using errcode = 'P0002';
  end if;

  if settings is not null then
    update tenant_settings ts
    set settings = system_admin_update_tenant.settings
    where ts.tenant_id = system_admin_update_tenant.tenant_id;
  end if;

  return v_tenant;
end;
$$;


--
-- Name: tenant_account(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.tenant_account(in_currency text, OUT acc public.account) RETURNS public.account
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  WITH ins AS (
    INSERT INTO account (tenant_id, person_id, currency)
      VALUES ((select current_tenant_id()), NULL::bigint, COALESCE(in_currency, 'CZK'))
      ON CONFLICT ON CONSTRAINT account_tenant_id_person_id_currency_idx DO NOTHING
      RETURNING *
  )
  SELECT * FROM ins
  UNION ALL
  SELECT account.* FROM account
    WHERE tenant_id = (select current_tenant_id()) AND person_id IS NULL AND currency = COALESCE(in_currency, 'CZK')
  LIMIT 1;
$$;


--
-- Name: FUNCTION tenant_account(in_currency text, OUT acc public.account); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.tenant_account(in_currency text, OUT acc public.account) IS '@behavior -*';


--
-- Name: tenant_administrator; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tenant_administrator (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    is_visible boolean DEFAULT true NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    CONSTRAINT tenant_administrator_until_gt_since CHECK ((until > since))
);


--
-- Name: TABLE tenant_administrator; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant_administrator IS '@behavior +list +connection -query:resource:list -query:resource:connection';


--
-- Name: COLUMN tenant_administrator.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.tenant_administrator.active_range IS '@behavior -*';


--
-- Name: tenant_administrator_tenant_name(public.tenant_administrator); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.tenant_administrator_tenant_name(public.tenant_administrator) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
  select name from tenant where id = $1.tenant_id;
$_$;


--
-- Name: tenant_couples(public.tenant); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.tenant_couples(t public.tenant) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    AS $$
  select couple.*
  from couple
  where couple.status = 'active'
    and (exists (select 1 from current_tenant_membership where person_id = man_id)
      or exists (select 1 from current_tenant_membership where person_id = woman_id));
$$;


--
-- Name: FUNCTION tenant_couples(t public.tenant); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.tenant_couples(t public.tenant) IS '@behavior +list -connection';


--
-- Name: tenant_membership; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tenant_membership (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    CONSTRAINT tenant_membership_until_gt_since CHECK ((until > since))
);


--
-- Name: TABLE tenant_membership; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant_membership IS '@behavior +list +connection -query:resource:list -query:resource:connection';


--
-- Name: COLUMN tenant_membership.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.tenant_membership.active_range IS '@behavior -*';


--
-- Name: tenant_membership_tenant_name(public.tenant_membership); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.tenant_membership_tenant_name(public.tenant_membership) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
  select name from tenant where id = $1.tenant_id;
$_$;


--
-- Name: tenant_trainer_tenant_name(public.tenant_trainer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.tenant_trainer_tenant_name(public.tenant_trainer) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
  select name from tenant where id = $1.tenant_id;
$_$;


--
-- Name: trainer_group_attendance_completion(timestamp with time zone, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.trainer_group_attendance_completion(since timestamp with time zone DEFAULT NULL::timestamp with time zone, until timestamp with time zone DEFAULT NULL::timestamp with time zone) RETURNS SETOF public.trainer_group_attendance_completion
    LANGUAGE sql STABLE
    AS $_$
  with filtered_instances as (
    select ei.id
    from event ei
    where ei.tenant_id = current_tenant_id()
      and not ei.is_cancelled
      and ei.type = 'group'
      and coalesce(ei.until, ei.since) < coalesce($2, now())
      and ($1 is null or coalesce(ei.since, ei.until) >= $1)
      and ($2 is null or coalesce(ei.until, ei.since) < $2)
  ),
  trainer_instances as (
    select effective_trainer.person_id, fi.id as event_id
    from filtered_instances fi
    join event instance on instance.id = fi.id
    cross join lateral app_private.event_trainers_at(instance, instance.since) effective_trainer
  ),
  attendance_stats as (
    select
      ti.person_id,
      ti.event_id,
      coalesce(stats.attendance_count, 0) as attendance_count,
      coalesce(stats.unknown_count, 0) as unknown_count
    from trainer_instances ti
    left join lateral (
      select
        count(*) as attendance_count,
        count(*) filter (where eir.status = 'unknown') as unknown_count
      from event_registration eir
      where eir.event_id = ti.event_id
        and eir.person_id is not null
        and eir.registration_status = 'active'
    ) stats on true
  ),
  per_trainer as (
    select
      person_id,
      count(*) as total_events,
      count(*) filter (where attendance_count > 0 and unknown_count = 0) as filled_events,
      count(*) filter (
        where attendance_count > 0
          and unknown_count > 0
          and unknown_count < attendance_count
      ) as partially_filled_events,
      count(*) filter (where attendance_count = 0 or unknown_count = attendance_count) as unfilled_events,
      coalesce(sum(attendance_count), 0) as total_attendances,
      coalesce(sum(unknown_count), 0) as pending_attendances
    from attendance_stats
    group by person_id
  )
  select
    person_id,
    total_events,
    filled_events,
    partially_filled_events,
    unfilled_events,
    case
      when total_events > 0 then (filled_events + partially_filled_events)::double precision / total_events
      else null
    end as filled_ratio,
    total_attendances,
    pending_attendances
  from per_trainer
  order by filled_ratio asc nulls last, person_id;
$_$;


--
-- Name: FUNCTION trainer_group_attendance_completion(since timestamp with time zone, until timestamp with time zone); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.trainer_group_attendance_completion(since timestamp with time zone, until timestamp with time zone) IS '@behavior +list -connection';


--
-- Name: update_attendance(bigint, public.attendance_type, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.update_attendance(eir_id bigint, status public.attendance_type, note text) RETURNS public.event_registration
    LANGUAGE sql
    AS $_$
  update event_registration set status = $2, attendance_note = $3 where id = $1 returning *;
$_$;


--
-- Name: tenant_settings; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tenant_settings (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    settings jsonb NOT NULL
);


--
-- Name: TABLE tenant_settings; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant_settings IS '@behavior -insert -delete -query:resource:list -query:resource:connection -singularRelation:resource:list';


--
-- Name: update_tenant_settings_key(text[], jsonb); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.update_tenant_settings_key(path text[], new_value jsonb) RETURNS public.tenant_settings
    LANGUAGE sql
    AS $$
  update tenant_settings
  set settings = jsonb_set(settings, path, new_value, true)
  where tenant_id=current_tenant_id()
  returning *;
$$;


--
-- Name: upsert_announcement(public.announcement_type_input, public.announcement_audience_type_input[], bigint[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.upsert_announcement(info public.announcement_type_input, audiences public.announcement_audience_type_input[] DEFAULT NULL::public.announcement_audience_type_input[], attachments bigint[] DEFAULT NULL::bigint[]) RETURNS public.announcement
    LANGUAGE plpgsql
    AS $$
declare
  result announcement;
begin
  if info.id is null then
    insert into announcement (
      title, body, status, is_sticky, scheduled_since, scheduled_until
    ) values (
      info.title,
      info.body,
      coalesce(info.status, 'draft'),
      coalesce(info.is_sticky, false),
      info.scheduled_since,
      info.scheduled_until
    ) returning * into result;
  else
    select * into result from announcement where id = info.id;
    if not found then
      raise exception 'Announcement with id % not found', info.id;
    end if;
  end if;

  if attachments is not null then
    select coalesce(array_agg(id), '{}'::bigint[])
    into attachments
    from file
    where id = any(attachments)
      and tenant_id = result.tenant_id
      and uploaded_at is not null;

    delete from announcement_attachment
    where announcement_id = result.id
      and not inline
      and file_id <> all(attachments);

    insert into announcement_attachment (tenant_id, announcement_id, file_id, inline)
    select result.tenant_id, result.id, file_id, false
    from unnest(attachments) input(file_id)
    on conflict (tenant_id, announcement_id, file_id)
    do update set inline = false;
  end if;

  if info.id is not null then
    -- Make sure the update trigger re-populates inline references
    update announcement set
      title = info.title,
      body = info.body,
      status = coalesce(info.status, status),
      is_sticky = coalesce(info.is_sticky, false),
      scheduled_since = info.scheduled_since,
      scheduled_until = info.scheduled_until
    where id = info.id
    returning * into result;
  end if;

  if audiences is not null then
    with input as (
      select distinct (item).id, (item).cohort_id, (item).audience_role
      from unnest(audiences) item
    )
    delete from announcement_audience audience
    using input
    where audience.announcement_id = result.id
      and audience.id = input.id
      and input.id is not null
      and input.cohort_id is null
      and input.audience_role is null;

    with input as (
      select distinct (item).id, (item).cohort_id, (item).audience_role
      from unnest(audiences) item
    )
    update announcement_audience audience
    set cohort_id = input.cohort_id,
        audience_role = input.audience_role
    from input
    where audience.announcement_id = result.id
      and audience.id = input.id
      and input.id is not null
      and ((input.cohort_id is not null) <> (input.audience_role is not null))
      and (audience.cohort_id, audience.audience_role)
        is distinct from (input.cohort_id, input.audience_role);

    with input as (
      select distinct (item).cohort_id
      from unnest(audiences) item
      where (item).id is null
        and (item).cohort_id is not null
        and (item).audience_role is null
    )
    insert into announcement_audience (announcement_id, cohort_id)
    select result.id, input.cohort_id
    from input
    on conflict (announcement_id, cohort_id) do nothing;

    with input as (
      select distinct (item).audience_role
      from unnest(audiences) item
      where (item).id is null
        and (item).cohort_id is null
        and (item).audience_role is not null
    )
    insert into announcement_audience (announcement_id, audience_role)
    select result.id, input.audience_role
    from input
    on conflict (announcement_id, audience_role) do nothing;
  end if;

  return result;
end;
$$;


--
-- Name: aktuality; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.aktuality (
    id bigint CONSTRAINT aktuality_at_id_not_null NOT NULL,
    at_kdo bigint,
    at_kat text DEFAULT '1'::text NOT NULL,
    at_jmeno text NOT NULL,
    at_text text NOT NULL,
    at_preview text NOT NULL,
    at_foto bigint,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now(),
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    title_photo_url text,
    is_visible boolean DEFAULT true NOT NULL,
    published_at timestamp with time zone DEFAULT now(),
    CONSTRAINT aktuality_visible_requires_published_at CHECK (((NOT is_visible) OR (published_at IS NOT NULL)))
);


--
-- Name: TABLE aktuality; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.aktuality IS '@behavior -insert -update -query:resource:list';


--
-- Name: COLUMN aktuality.at_kat; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.aktuality.at_kat IS '@deprecated';


--
-- Name: upsert_article(public.article_type_input, bigint[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.upsert_article(info public.article_type_input, attachments bigint[] DEFAULT NULL::bigint[]) RETURNS public.aktuality
    LANGUAGE plpgsql
    AS $$
declare
  result aktuality;
begin
  if info.id is null then
    insert into aktuality (
      at_jmeno, at_preview, at_text, title_photo_url, is_visible, published_at
    ) values (
      info.title,
      coalesce(info.preview, ''),
      coalesce(info.body, ''),
      info.title_photo_url,
      coalesce(info.is_visible, true),
      coalesce(info.published_at, case when coalesce(info.is_visible, true) then now() end)
    ) returning * into result;
  else
    select * into result from aktuality where id = info.id;

    if not found then
      raise exception 'Article with id % not found', info.id;
    end if;
  end if;

  if attachments is not null then
    select coalesce(array_agg(id), '{}'::bigint[])
    into attachments
    from file
    where id = any(attachments)
      and tenant_id = result.tenant_id
      and uploaded_at is not null;

    delete from article_attachment
    where aktuality_id = result.id
      and not inline
      and file_id <> all(attachments);

    insert into article_attachment (tenant_id, aktuality_id, file_id, inline)
    select result.tenant_id, result.id, file_id, false
    from unnest(attachments) input(file_id)
    on conflict (tenant_id, aktuality_id, file_id)
    do update set inline = false;
  end if;

  if info.id is not null then
    -- Make sure the update trigger re-populates inline references
    update aktuality set
      at_jmeno = info.title,
      at_preview = coalesce(info.preview, ''),
      at_text = coalesce(info.body, ''),
      title_photo_url = info.title_photo_url,
      is_visible = coalesce(info.is_visible, true),
      published_at = coalesce(
        info.published_at,
        published_at,
        case when coalesce(info.is_visible, true) then now() end
      )
    where id = info.id
    returning * into result;
  end if;

  return result;
end;
$$;


--
-- Name: location; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.location (
    id bigint CONSTRAINT tenant_location_id_not_null NOT NULL,
    name text CONSTRAINT tenant_location_name_not_null NOT NULL,
    description text DEFAULT ''::text CONSTRAINT tenant_location_description_not_null NOT NULL,
    address public.address_domain,
    show_in_lists boolean DEFAULT false CONSTRAINT tenant_location_is_public_not_null NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() CONSTRAINT tenant_location_tenant_id_not_null NOT NULL,
    created_at timestamp with time zone DEFAULT now() CONSTRAINT tenant_location_created_at_not_null NOT NULL,
    updated_at timestamp with time zone DEFAULT now() CONSTRAINT tenant_location_updated_at_not_null NOT NULL,
    cover_image_id bigint,
    is_public boolean GENERATED ALWAYS AS (show_in_lists) STORED,
    long_name text,
    latitude double precision,
    longitude double precision,
    ordering integer DEFAULT 1 CONSTRAINT tenant_location_ordering_not_null NOT NULL
);


--
-- Name: TABLE location; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.location IS '@behavior +list -connection -query:resource:list -query:resource:connection -queryField:resource:connection';


--
-- Name: upsert_location(public.location_details_input, bigint[], bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.upsert_location(details public.location_details_input, image_ids bigint[] DEFAULT NULL::bigint[], cover_image_id bigint DEFAULT NULL::bigint) RETURNS public.location
    LANGUAGE plpgsql
    AS $$
declare
  result location;
begin
  if details.id is null then
    insert into location (
      name,
      long_name,
      description,
      address,
      show_in_lists,
      ordering,
      latitude,
      longitude,
      cover_image_id
    )
    values (
      details.name,
      nullif(nullif(btrim(details.long_name), ''), details.name),
      coalesce(details.description, ''),
      details.address,
      coalesce(details.show_in_lists, false),
      coalesce(details.ordering, 1),
      details.latitude,
      details.longitude,
      cover_image_id
    )
    returning * into result;
  else
    update location
    set name = details.name,
        long_name = nullif(nullif(btrim(details.long_name), ''), details.name),
        description = coalesce(details.description, ''),
        address = details.address,
        show_in_lists = coalesce(details.show_in_lists, location.show_in_lists),
        ordering = coalesce(details.ordering, location.ordering),
        latitude = details.latitude,
        longitude = details.longitude,
        cover_image_id = upsert_location.cover_image_id
    where id = details.id
    returning * into result;

    if not found then
      raise exception 'Location with id % not found', details.id;
    end if;
  end if;

  if image_ids is not null then
    select coalesce(array_agg(id), '{}'::bigint[])
    into image_ids
    from file
    where id = any(image_ids)
      and tenant_id = result.tenant_id
      and uploaded_at is not null
      and content_type like 'image/%';

    delete from location_image image
    where image.tenant_id = result.tenant_id
      and image.location_id = result.id
      and image.file_id <> all(image_ids);

    insert into location_image (tenant_id, location_id, file_id)
    select result.tenant_id, result.id, file_id
    from unnest(image_ids) input(file_id)
    on conflict do nothing;
  end if;

  return result;
end;
$$;


--
-- Name: verify_function(regproc, regclass); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.verify_function(f regproc, relid regclass DEFAULT 0) RETURNS void
    LANGUAGE plpgsql
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  issues text;
begin
  select string_agg(
    concat_ws(
      E'\n',
      format(
        '%s:%s:%s:%s: %s',
        issue.level,
        issue.sqlstate,
        coalesce(issue.lineno::text, '?'),
        issue.statement,
        issue.message
      ),
      'Query: ' || issue.query,
      'Detail: ' || issue.detail,
      'Hint: ' || issue.hint,
      'Context: ' || issue.context
    ),
    E'\n\n' order by issue.lineno nulls last, issue.level, issue.message
  ) into issues
  from plpgsql_check_function_tb(
    funcoid => f,
    relid => relid,
    fatal_errors => false,
    all_warnings => true
  ) issue;

  if issues is not null then
    raise exception 'Error when checking function %', f using detail = issues;
  end if;
end;
$$;


--
-- Name: FUNCTION verify_function(f regproc, relid regclass); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.verify_function(f regproc, relid regclass) IS '@behavior -*';


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
-- Name: event_people; Type: MATERIALIZED VIEW; Schema: app_private; Owner: -
--

CREATE MATERIALIZED VIEW app_private.event_people AS
 WITH RECURSIVE participants AS (
         SELECT p_1.tenant_id,
            p_1.event_id,
            array_agg(DISTINCT p_1.person_id ORDER BY p_1.person_id) FILTER (WHERE p_1.is_attendee) AS viewers,
            array_agg(DISTINCT p_1.person_id ORDER BY p_1.person_id) AS people
           FROM ( SELECT event_registration.tenant_id,
                    event_registration.event_id,
                    event_registration.person_id,
                    true AS is_attendee
                   FROM public.event_registration
                  WHERE ((event_registration.registration_status = 'active'::public.event_registration_status) AND (event_registration.person_id IS NOT NULL))
                UNION ALL
                 SELECT event_trainer.tenant_id,
                    event_trainer.event_id,
                    event_trainer.person_id,
                    false
                   FROM public.event_trainer) p_1
          GROUP BY p_1.tenant_id, p_1.event_id
        ), attended(tenant_id, viewers, event_id) AS (
         SELECT participants.tenant_id,
            participants.viewers,
            participants.event_id
           FROM participants
          WHERE (participants.viewers IS NOT NULL)
        UNION
         SELECT parent.tenant_id,
            parent.viewers,
            child.id
           FROM (attended parent
             JOIN public.event child ON (((child.parent_id = parent.event_id) AND (child.tenant_id = parent.tenant_id))))
          WHERE (child.parent_id IS NOT NULL)
        ), rosters AS (
         SELECT DISTINCT a.tenant_id,
            a.viewers,
            p_1.people
           FROM (attended a
             JOIN participants p_1 USING (tenant_id, event_id))
        )
 SELECT DISTINCT rosters.tenant_id,
    v.viewer_person_id,
    p.visible_person_id
   FROM ((rosters
     CROSS JOIN LATERAL unnest(rosters.viewers) v(viewer_person_id))
     CROSS JOIN LATERAL unnest(rosters.people) p(visible_person_id))
  WITH NO DATA;


--
-- Name: meta_fks; Type: VIEW; Schema: app_private; Owner: -
--

CREATE VIEW app_private.meta_fks AS
 SELECT table_schema,
    table_name,
    column_name,
    constraint_name,
    confupdtype,
    confdeltype,
    pg_get_constraintdef
   FROM ( SELECT ((c.connamespace)::regnamespace)::text AS table_schema,
            ((c.conrelid)::regclass)::text AS table_name,
            con.column_name,
            c.conname AS constraint_name,
            c.confupdtype,
            c.confdeltype,
            pg_get_constraintdef(c.oid) AS pg_get_constraintdef
           FROM (((pg_constraint c
             JOIN pg_namespace ON ((pg_namespace.oid = c.connamespace)))
             JOIN pg_class ON ((c.conrelid = pg_class.oid)))
             LEFT JOIN information_schema.constraint_column_usage con ON (((c.conname = (con.constraint_name)::name) AND (pg_namespace.nspname = (con.constraint_schema)::name))))) all_constraints
  WHERE (table_schema = ANY (ARRAY['public'::text, 'app_private'::text]))
  ORDER BY table_schema, table_name, column_name, constraint_name;


--
-- Name: platby_category; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.platby_category (
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
    id bigint GENERATED ALWAYS AS (pc_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: TABLE platby_category; Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON TABLE app_private.platby_category IS '@omit create,update,delete';


--
-- Name: platby_category_group; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.platby_category_group (
    pcg_id bigint NOT NULL,
    pcg_id_group bigint NOT NULL,
    pcg_id_category bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pcg_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: TABLE platby_category_group; Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON TABLE app_private.platby_category_group IS '@omit';


--
-- Name: platby_category_group_pcg_id_seq; Type: SEQUENCE; Schema: app_private; Owner: -
--

CREATE SEQUENCE app_private.platby_category_group_pcg_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: platby_category_group_pcg_id_seq; Type: SEQUENCE OWNED BY; Schema: app_private; Owner: -
--

ALTER SEQUENCE app_private.platby_category_group_pcg_id_seq OWNED BY app_private.platby_category_group.pcg_id;


--
-- Name: platby_category_pc_id_seq; Type: SEQUENCE; Schema: app_private; Owner: -
--

CREATE SEQUENCE app_private.platby_category_pc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: platby_category_pc_id_seq; Type: SEQUENCE OWNED BY; Schema: app_private; Owner: -
--

ALTER SEQUENCE app_private.platby_category_pc_id_seq OWNED BY app_private.platby_category.pc_id;


--
-- Name: platby_group; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.platby_group (
    pg_id bigint NOT NULL,
    pg_type numeric DEFAULT '1'::numeric NOT NULL,
    pg_name text NOT NULL,
    pg_description text NOT NULL,
    pg_base bigint DEFAULT '0'::bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pg_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: TABLE platby_group; Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON TABLE app_private.platby_group IS '@omit';


--
-- Name: platby_group_pg_id_seq; Type: SEQUENCE; Schema: app_private; Owner: -
--

CREATE SEQUENCE app_private.platby_group_pg_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: platby_group_pg_id_seq; Type: SEQUENCE OWNED BY; Schema: app_private; Owner: -
--

ALTER SEQUENCE app_private.platby_group_pg_id_seq OWNED BY app_private.platby_group.pg_id;


--
-- Name: platby_group_skupina; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.platby_group_skupina (
    pgs_id bigint NOT NULL,
    pgs_id_skupina bigint NOT NULL,
    pgs_id_group bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pgs_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: TABLE platby_group_skupina; Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON TABLE app_private.platby_group_skupina IS '@omit';


--
-- Name: platby_group_skupina_pgs_id_seq; Type: SEQUENCE; Schema: app_private; Owner: -
--

CREATE SEQUENCE app_private.platby_group_skupina_pgs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: platby_group_skupina_pgs_id_seq; Type: SEQUENCE OWNED BY; Schema: app_private; Owner: -
--

ALTER SEQUENCE app_private.platby_group_skupina_pgs_id_seq OWNED BY app_private.platby_group_skupina.pgs_id;


--
-- Name: platby_item; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.platby_item (
    pi_id bigint NOT NULL,
    pi_id_user bigint,
    pi_id_category bigint NOT NULL,
    pi_id_raw bigint,
    pi_amount numeric(10,2) NOT NULL,
    pi_date date NOT NULL,
    pi_prefix integer DEFAULT 2000 NOT NULL,
    id bigint GENERATED ALWAYS AS (pi_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    status public.payment_status DEFAULT 'paid'::public.payment_status NOT NULL
);


--
-- Name: TABLE platby_item; Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON TABLE app_private.platby_item IS '@omit create,update,delete';


--
-- Name: platby_item_pi_id_seq; Type: SEQUENCE; Schema: app_private; Owner: -
--

CREATE SEQUENCE app_private.platby_item_pi_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: platby_item_pi_id_seq; Type: SEQUENCE OWNED BY; Schema: app_private; Owner: -
--

ALTER SEQUENCE app_private.platby_item_pi_id_seq OWNED BY app_private.platby_item.pi_id;


--
-- Name: platby_raw; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.platby_raw (
    pr_id bigint NOT NULL,
    pr_raw bytea NOT NULL,
    pr_hash text NOT NULL,
    pr_sorted boolean DEFAULT true NOT NULL,
    pr_discarded boolean DEFAULT true NOT NULL,
    id bigint GENERATED ALWAYS AS (pr_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: TABLE platby_raw; Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON TABLE app_private.platby_raw IS '@omit';


--
-- Name: platby_raw_pr_id_seq; Type: SEQUENCE; Schema: app_private; Owner: -
--

CREATE SEQUENCE app_private.platby_raw_pr_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: platby_raw_pr_id_seq; Type: SEQUENCE OWNED BY; Schema: app_private; Owner: -
--

ALTER SEQUENCE app_private.platby_raw_pr_id_seq OWNED BY app_private.platby_raw.pr_id;


--
-- Name: system_admin_user; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.system_admin_user (
    user_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by bigint DEFAULT public.current_user_id() NOT NULL
);


--
-- Name: TABLE system_admin_user; Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON TABLE app_private.system_admin_user IS '@omit all';


--
-- Name: frontier; Type: TABLE; Schema: crawler; Owner: -
--

CREATE TABLE crawler.frontier (
    id bigint NOT NULL,
    federation text NOT NULL,
    kind text NOT NULL,
    key text NOT NULL,
    discovered_at timestamp with time zone DEFAULT now() NOT NULL,
    last_fetched_at timestamp with time zone,
    fetch_status crawler.fetch_status DEFAULT 'pending'::crawler.fetch_status NOT NULL,
    process_status crawler.process_status DEFAULT 'pending'::crawler.process_status NOT NULL,
    error_count integer DEFAULT 0 NOT NULL,
    next_fetch_at timestamp with time zone DEFAULT now() NOT NULL,
    meta jsonb DEFAULT '{}'::jsonb NOT NULL,
    last_process_error text,
    last_process_error_at timestamp with time zone,
    last_response_id bigint,
    last_successful_response_id bigint
);


--
-- Name: TABLE frontier; Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON TABLE crawler.frontier IS '@name crawler_target
@behavior -insert -update -delete -list';


--
-- Name: json_response; Type: TABLE; Schema: crawler; Owner: -
--

CREATE TABLE crawler.json_response (
    id bigint NOT NULL,
    frontier_id bigint NOT NULL,
    url text NOT NULL,
    fetched_at timestamp with time zone DEFAULT now() NOT NULL,
    http_status integer,
    error text,
    content_hash text,
    sys_period tstzrange,
    last_observed_at timestamp with time zone,
    observation_count bigint DEFAULT 1 NOT NULL,
    CONSTRAINT json_response_observation_check CHECK ((((sys_period IS NULL) AND (last_observed_at IS NULL)) OR ((sys_period IS NOT NULL) AND (last_observed_at IS NOT NULL) AND (NOT isempty(sys_period)) AND (NOT lower_inf(sys_period)) AND lower_inc(sys_period) AND (NOT upper_inc(sys_period)) AND (lower(sys_period) = fetched_at) AND (sys_period @> last_observed_at)))),
    CONSTRAINT json_response_observation_count_check CHECK ((observation_count > 0))
);


--
-- Name: TABLE json_response; Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON TABLE crawler.json_response IS '@name crawler_response
@behavior -insert -update -delete -list';


--
-- Name: frontier_failure; Type: VIEW; Schema: crawler; Owner: -
--

CREATE VIEW crawler.frontier_failure WITH (security_invoker='true') AS
 SELECT f.id,
    f.federation,
    f.kind,
    f.key,
    f.fetch_status,
    f.process_status,
    f.error_count,
    f.next_fetch_at,
        CASE
            WHEN ((f.fetch_status = ANY (ARRAY['error'::crawler.fetch_status, 'transient'::crawler.fetch_status])) AND (f.process_status = 'error'::crawler.process_status)) THEN GREATEST(COALESCE(jr.last_observed_at, jr.fetched_at, f.last_fetched_at, f.discovered_at), COALESCE(f.last_process_error_at, f.last_fetched_at, f.discovered_at))
            WHEN (f.process_status = 'error'::crawler.process_status) THEN COALESCE(f.last_process_error_at, f.last_fetched_at, f.discovered_at)
            ELSE COALESCE(jr.last_observed_at, jr.fetched_at, f.last_fetched_at, f.discovered_at)
        END AS failed_at,
    jr.url,
    jr.http_status,
    jr.error AS response_error,
    f.last_process_error AS process_error,
    concat_ws('+'::text,
        CASE
            WHEN (f.fetch_status = ANY (ARRAY['error'::crawler.fetch_status, 'transient'::crawler.fetch_status])) THEN 'fetch'::text
            ELSE NULL::text
        END,
        CASE
            WHEN (f.process_status = 'error'::crawler.process_status) THEN 'process'::text
            ELSE NULL::text
        END) AS failure,
    COALESCE(
        CASE
            WHEN (f.process_status = 'error'::crawler.process_status) THEN f.last_process_error
            ELSE NULL::text
        END, jr.error, f.last_process_error, ''::text) AS error_text
   FROM (crawler.frontier f
     LEFT JOIN crawler.json_response jr ON ((jr.id = f.last_response_id)))
  WHERE ((f.fetch_status = ANY (ARRAY['error'::crawler.fetch_status, 'transient'::crawler.fetch_status])) OR (f.process_status = 'error'::crawler.process_status));


--
-- Name: VIEW frontier_failure; Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON VIEW crawler.frontier_failure IS '@name crawler_target_failure
@behavior -insert -update -delete -list';


--
-- Name: frontier_failure_group; Type: VIEW; Schema: crawler; Owner: -
--

CREATE VIEW crawler.frontier_failure_group WITH (security_invoker='true') AS
 SELECT federation,
    kind,
    failure,
    http_status,
    "left"(regexp_replace(COALESCE(NULLIF(error_text, ''::text), '-'::text), '\s+'::text, ' '::text, 'g'::text), 180) AS error_fingerprint,
    (count(*))::integer AS count,
    max(failed_at) AS failed_at
   FROM crawler.frontier_failure
  GROUP BY federation, kind, failure, http_status, ("left"(regexp_replace(COALESCE(NULLIF(error_text, ''::text), '-'::text), '\s+'::text, ' '::text, 'g'::text), 180));


--
-- Name: VIEW frontier_failure_group; Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON VIEW crawler.frontier_failure_group IS '@name crawler_target_failure_group
@behavior +list -connection -insert -update -delete';


--
-- Name: frontier_fetch_job; Type: VIEW; Schema: crawler; Owner: -
--

CREATE VIEW crawler.frontier_fetch_job AS
 WITH jobs AS (
         SELECT j_1.id,
            j_1.key,
            "substring"(j_1.key, '^fetch:(.*):[0-9]+$'::text) AS host,
                CASE
                    WHEN ((pj.payload ->> 'id'::text) ~ '^[0-9]+$'::text) THEN ((pj.payload ->> 'id'::text))::bigint
                    ELSE NULL::bigint
                END AS frontier_id,
            j_1.run_at,
            j_1.locked_at,
            j_1.attempts,
            j_1.max_attempts,
            j_1.last_error,
            j_1.updated_at
           FROM (graphile_worker.jobs j_1
             JOIN graphile_worker._private_jobs pj ON ((pj.id = j_1.id)))
          WHERE (j_1.task_identifier = 'frontier_fetch'::text)
        )
 SELECT j.id AS job_id,
    j.key AS job_key,
    j.host,
    j.run_at,
    j.locked_at,
    j.attempts,
    j.max_attempts,
    j.last_error AS job_error,
    j.updated_at AS job_updated_at,
    f.id AS frontier_id,
    f.federation,
    f.kind,
    f.key AS frontier_key,
    f.fetch_status,
    f.process_status,
    f.last_process_error AS process_error,
        CASE
            WHEN (j.attempts >= j.max_attempts) THEN 'failed'::text
            WHEN (j.locked_at IS NOT NULL) THEN 'locked'::text
            WHEN (j.run_at <= now()) THEN 'ready'::text
            ELSE 'delayed'::text
        END AS state
   FROM (jobs j
     JOIN crawler.frontier f ON ((f.id = j.frontier_id)))
  WHERE (j.frontier_id IS NOT NULL);


--
-- Name: VIEW frontier_fetch_job; Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON VIEW crawler.frontier_fetch_job IS '@name crawler_fetch_job
@behavior -insert -update -delete -list';


--
-- Name: frontier_id_seq; Type: SEQUENCE; Schema: crawler; Owner: -
--

CREATE SEQUENCE crawler.frontier_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: frontier_id_seq; Type: SEQUENCE OWNED BY; Schema: crawler; Owner: -
--

ALTER SEQUENCE crawler.frontier_id_seq OWNED BY crawler.frontier.id;


--
-- Name: frontier_summary; Type: VIEW; Schema: crawler; Owner: -
--

CREATE VIEW crawler.frontier_summary WITH (security_invoker='true') AS
 SELECT federation,
    kind,
    (count(*))::integer AS total,
    (count(*) FILTER (WHERE (fetch_status = 'pending'::crawler.fetch_status)))::integer AS fetch_pending,
    (count(*) FILTER (WHERE (fetch_status = 'transient'::crawler.fetch_status)))::integer AS fetch_transient,
    (count(*) FILTER (WHERE (fetch_status = 'error'::crawler.fetch_status)))::integer AS fetch_error,
    (count(*) FILTER (WHERE (process_status = 'ok'::crawler.process_status)))::integer AS done,
    (count(*) FILTER (WHERE (process_status = 'error'::crawler.process_status)))::integer AS process_error,
    (count(*) FILTER (WHERE ((process_status = 'pending'::crawler.process_status) AND (fetch_status = 'ok'::crawler.fetch_status))))::integer AS process_ready,
    max(last_fetched_at) AS latest,
    (count(*) FILTER (WHERE ((next_fetch_at <= now()) AND (fetch_status = ANY (ARRAY['pending'::crawler.fetch_status, 'transient'::crawler.fetch_status])))))::integer AS fetch_due,
    (count(*) FILTER (WHERE ((next_fetch_at <= now()) AND (fetch_status = 'ok'::crawler.fetch_status) AND (process_status = 'ok'::crawler.process_status))))::integer AS refetch_due
   FROM crawler.frontier
  GROUP BY federation, kind;


--
-- Name: VIEW frontier_summary; Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON VIEW crawler.frontier_summary IS '@name crawler_target_summary
@behavior +list -connection -insert -update -delete';


--
-- Name: incremental_ranges; Type: TABLE; Schema: crawler; Owner: -
--

CREATE TABLE crawler.incremental_ranges (
    federation text NOT NULL,
    kind text NOT NULL,
    last_known integer DEFAULT 0 NOT NULL,
    last_checked integer DEFAULT 0 NOT NULL
);


--
-- Name: TABLE incremental_ranges; Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON TABLE crawler.incremental_ranges IS '@name crawler_incremental_range
@behavior -insert -update -delete -list';


--
-- Name: json_response_cache; Type: TABLE; Schema: crawler; Owner: -
--

CREATE TABLE crawler.json_response_cache (
    content_hash text GENERATED ALWAYS AS (encode(public.digest((content)::text, 'sha256'::text), 'hex'::text)) STORED NOT NULL,
    content jsonb NOT NULL
);


--
-- Name: TABLE json_response_cache; Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON TABLE crawler.json_response_cache IS '@name crawler_response_cache
@behavior -insert -update -delete -list';


--
-- Name: json_response_id_seq; Type: SEQUENCE; Schema: crawler; Owner: -
--

CREATE SEQUENCE crawler.json_response_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: json_response_id_seq; Type: SEQUENCE OWNED BY; Schema: crawler; Owner: -
--

ALTER SEQUENCE crawler.json_response_id_seq OWNED BY crawler.json_response.id;


--
-- Name: rate_limit_rule; Type: TABLE; Schema: crawler; Owner: -
--

CREATE TABLE crawler.rate_limit_rule (
    host text NOT NULL,
    max_requests integer NOT NULL,
    per_interval interval NOT NULL,
    spacing interval GENERATED ALWAYS AS (((per_interval / (max_requests)::double precision) + '00:00:00.02'::interval)) STORED NOT NULL,
    next_available_at timestamp with time zone DEFAULT '1970-01-01 00:00:00+00'::timestamp with time zone NOT NULL,
    CONSTRAINT rate_limit_rule_max_requests_check CHECK ((max_requests > 0)),
    CONSTRAINT rate_limit_rule_per_interval_check CHECK ((per_interval > '00:00:00'::interval))
);


--
-- Name: TABLE rate_limit_rule; Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON TABLE crawler.rate_limit_rule IS '@name crawler_rate_limit_rule
@behavior -insert -update -delete -list';


--
-- Name: age_group; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.age_group (
    code text NOT NULL,
    name text NOT NULL,
    aliases text[] DEFAULT '{}'::text[] NOT NULL,
    allowed_to_dance_in text[] DEFAULT '{}'::text[] NOT NULL,
    age int4range,
    person_age int4range,
    is_person boolean DEFAULT false NOT NULL,
    is_couple boolean DEFAULT false NOT NULL,
    is_team boolean DEFAULT false NOT NULL,
    is_competition boolean DEFAULT false NOT NULL,
    divisions text[] DEFAULT '{}'::text[] NOT NULL,
    CONSTRAINT age_group_age_check CHECK ((NOT isempty(age))),
    CONSTRAINT age_group_person_age_check CHECK ((NOT isempty(person_age)))
);


--
-- Name: TABLE age_group; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.age_group IS '@name age_group
@behavior -insert -update -delete +list -connection';


--
-- Name: COLUMN age_group.aliases; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.age_group.aliases IS '@behavior -filterBy -orderBy';


--
-- Name: COLUMN age_group.allowed_to_dance_in; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.age_group.allowed_to_dance_in IS '@behavior -filterBy -orderBy';


--
-- Name: COLUMN age_group.age; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.age_group.age IS '@behavior -filterBy -orderBy';


--
-- Name: COLUMN age_group.person_age; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.age_group.person_age IS '@behavior -filterBy -orderBy';


--
-- Name: COLUMN age_group.divisions; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.age_group.divisions IS '@behavior -filterBy -orderBy';


--
-- Name: category_id_seq; Type: SEQUENCE; Schema: federated; Owner: -
--

ALTER TABLE federated.category ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME federated.category_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: competition_entry; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.competition_entry (
    competition_id bigint NOT NULL,
    competitor_id text NOT NULL,
    cancelled boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE competition_entry; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.competition_entry IS '@name competition_entry
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: competition_id_seq; Type: SEQUENCE; Schema: federated; Owner: -
--

ALTER TABLE federated.competition ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME federated.competition_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: competition_official; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.competition_official (
    competition_id bigint NOT NULL,
    person_id text NOT NULL,
    role federated.official_role NOT NULL,
    external_id text,
    judge_label text
);


--
-- Name: TABLE competition_official; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.competition_official IS '@name competition_official
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: COLUMN competition_official.role; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.competition_official.role IS '@behavior +attribute:orderBy';


--
-- Name: competition_result; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.competition_result (
    competition_id bigint NOT NULL,
    competitor_id text NOT NULL,
    start_number text,
    ranking integer NOT NULL,
    ranking_to integer,
    point_gain numeric(10,3),
    final_gain numeric(10,3),
    is_final boolean,
    completion_status text,
    last_round text,
    last_dance text,
    CONSTRAINT competition_result_check CHECK (((ranking_to IS NULL) OR (ranking_to >= ranking)))
);


--
-- Name: TABLE competition_result; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.competition_result IS '@name competition_result
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: competition_participation; Type: VIEW; Schema: federated; Owner: -
--

CREATE VIEW federated.competition_participation WITH (security_invoker='true') AS
 SELECT r.competition_id,
    r.competitor_id,
    COALESCE(e.cancelled, false) AS cancelled
   FROM (federated.competition_result r
     LEFT JOIN federated.competition_entry e USING (competition_id, competitor_id))
UNION ALL
 SELECT e.competition_id,
    e.competitor_id,
    e.cancelled
   FROM federated.competition_entry e
  WHERE (NOT (EXISTS ( SELECT 1
           FROM federated.competition_result r
          WHERE ((r.competition_id = e.competition_id) AND (r.competitor_id = e.competitor_id)))));


--
-- Name: VIEW competition_participation; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON VIEW federated.competition_participation IS '@name competition_participation
@primaryKey competition_id,competitor_id
@foreignKey (competition_id) references federated.competition (id)|@fieldName competition|@foreignFieldName participations
@foreignKey (competitor_id) references federated.competitor (id)|@fieldName competitor|@foreignFieldName competitionParticipations
@behavior -single -list -connection -insert -update -delete';


--
-- Name: competition_round; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.competition_round (
    id bigint NOT NULL,
    competition_id bigint NOT NULL,
    round_label text,
    round_key text NOT NULL,
    round_index integer,
    dance_program_id bigint NOT NULL,
    scoring_method federated.scoring_method NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE competition_round; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.competition_round IS '@name competition_round
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list';


--
-- Name: competition_round_id_seq; Type: SEQUENCE; Schema: federated; Owner: -
--

ALTER TABLE federated.competition_round ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME federated.competition_round_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: competition_round_judge; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.competition_round_judge (
    round_id bigint NOT NULL,
    person_judge_id text NOT NULL,
    judge_index integer NOT NULL,
    judge_label text
);


--
-- Name: TABLE competition_round_judge; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.competition_round_judge IS '@name competition_round_judge
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: competition_round_result; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.competition_round_result (
    round_id bigint NOT NULL,
    competitor_id text NOT NULL,
    overall_ranking integer,
    overall_ranking_to integer,
    qualified_next boolean,
    overall_score numeric(10,3),
    dance_results real[],
    CONSTRAINT competition_round_result_check CHECK (((overall_ranking IS NULL) OR (overall_ranking_to IS NULL) OR (overall_ranking_to >= overall_ranking)))
);


--
-- Name: TABLE competition_round_result; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.competition_round_result IS '@name competition_round_result
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: competitor; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.competitor (
    id text GENERATED ALWAYS AS (((federation || ':'::text) || external_id)) STORED NOT NULL,
    federation text NOT NULL,
    competitor_type federated.competitor_type NOT NULL,
    age_group text,
    name text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    age_group_code text,
    search_name text GENERATED ALWAYS AS (app_private.normalize_name(name)) STORED,
    external_id text CONSTRAINT competitor_external_id_text_not_null NOT NULL,
    latest_result_at date,
    latest_entry_at date,
    divisions text[] DEFAULT '{}'::text[] NOT NULL
);


--
-- Name: TABLE competitor; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.competitor IS '@name federation_competitor
@behavior -insert -update -delete -list
@ref people via:federated.competitor_component;federated.person;public.person_federation_link;public.person behavior:"+list -connection"
@ref couples via:public.couple_federation_link;public.couple
@ref competitions via:federated.competition_participation;federated.competition';


--
-- Name: COLUMN competitor.age_group; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.competitor.age_group IS '@behavior -*';


--
-- Name: COLUMN competitor.age_group_code; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.competitor.age_group_code IS '@name ageGroup
@behavior -insert -update';


--
-- Name: COLUMN competitor.search_name; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.competitor.search_name IS '@behavior -*';


--
-- Name: COLUMN competitor.divisions; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.competitor.divisions IS '@behavior -filterBy -orderBy';


--
-- Name: competitor_category_progress; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.competitor_category_progress (
    competitor_id text NOT NULL,
    category_id bigint NOT NULL,
    points numeric(10,3) DEFAULT 0 NOT NULL,
    domestic_finals integer DEFAULT 0 NOT NULL,
    foreign_finals integer DEFAULT 0 NOT NULL
);


--
-- Name: TABLE competitor_category_progress; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.competitor_category_progress IS '@name federation_competitor_category_progress
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: competitor_club_affiliation; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.competitor_club_affiliation (
    competitor_id text NOT NULL,
    club_id bigint NOT NULL,
    valid_from date NOT NULL,
    valid_to date,
    CONSTRAINT competitor_club_affiliation_check CHECK (((valid_to IS NULL) OR (valid_to >= valid_from)))
);


--
-- Name: TABLE competitor_club_affiliation; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.competitor_club_affiliation IS '@name federation_competitor_club_affiliation
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: competitor_component; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.competitor_component (
    competitor_id text NOT NULL,
    person_id text NOT NULL,
    role federated.competitor_role NOT NULL
);


--
-- Name: TABLE competitor_component; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.competitor_component IS '@name federation_competitor_component
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: COLUMN competitor_component.role; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.competitor_component.role IS '@behavior +attribute:orderBy';


--
-- Name: couple_age_rule; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.couple_age_rule (
    age_group_code text NOT NULL,
    division text DEFAULT 'General'::text NOT NULL,
    older_age int4range NOT NULL,
    younger_age int4range NOT NULL,
    CONSTRAINT couple_age_rule_older_age_check CHECK ((NOT isempty(older_age))),
    CONSTRAINT couple_age_rule_younger_age_check CHECK ((NOT isempty(younger_age)))
);


--
-- Name: TABLE couple_age_rule; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.couple_age_rule IS '@name federation_couple_age_rule
@behavior -insert -update -delete +list -connection -query:resource:single -query:resource:list -query:resource:connection';


--
-- Name: COLUMN couple_age_rule.older_age; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.couple_age_rule.older_age IS '@behavior -filterBy -orderBy';


--
-- Name: COLUMN couple_age_rule.younger_age; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.couple_age_rule.younger_age IS '@behavior -filterBy -orderBy';


--
-- Name: dance; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.dance (
    code text NOT NULL,
    name text NOT NULL,
    discipline text NOT NULL
);


--
-- Name: TABLE dance; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.dance IS '@name dance
@behavior -insert -update -delete +list -connection';


--
-- Name: dance_program; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.dance_program (
    id bigint NOT NULL,
    code text,
    name text NOT NULL,
    discipline text,
    is_default boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE dance_program; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.dance_program IS '@name dance_program
@behavior -insert -update -delete +list -connection';


--
-- Name: dance_program_dance; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.dance_program_dance (
    program_id bigint NOT NULL,
    dance_code text NOT NULL,
    dance_order integer NOT NULL
);


--
-- Name: TABLE dance_program_dance; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.dance_program_dance IS '@name dance_program_dance
@behavior -insert -update -delete +list -connection';


--
-- Name: dance_program_id_seq; Type: SEQUENCE; Schema: federated; Owner: -
--

ALTER TABLE federated.dance_program ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME federated.dance_program_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: event; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.event (
    id bigint NOT NULL,
    federation text NOT NULL,
    external_id text NOT NULL,
    name text,
    start_date date NOT NULL,
    end_date date,
    location text,
    city text,
    country text,
    street_address text,
    postal_code text,
    address_note text,
    geo_reference text,
    floor_size text,
    contact_name text,
    contact_phone text,
    contact_email text,
    website_url text,
    organizing_club_id bigint,
    range daterange GENERATED ALWAYS AS (daterange(start_date, (COALESCE(end_date, start_date) + 1), '[)'::text)) STORED,
    venue_lat double precision,
    venue_lng double precision,
    venue_location_source text,
    venue_location_ref text,
    status federated.event_status DEFAULT 'unknown'::federated.event_status NOT NULL,
    CONSTRAINT event_check CHECK (((end_date IS NULL) OR (end_date >= start_date)))
);


--
-- Name: TABLE event; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.event IS '@name federation_event
@behavior -insert -update -delete -list';


--
-- Name: event_id_seq; Type: SEQUENCE; Schema: federated; Owner: -
--

ALTER TABLE federated.event ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME federated.event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: event_official; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.event_official (
    event_id bigint NOT NULL,
    person_id text NOT NULL,
    role federated.official_role NOT NULL,
    discipline text DEFAULT ''::text NOT NULL,
    grade text
);


--
-- Name: TABLE event_official; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.event_official IS '@name federation_event_official
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: COLUMN event_official.role; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.event_official.role IS '@behavior +attribute:orderBy';


--
-- Name: federation_club; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.federation_club (
    id bigint NOT NULL,
    federation text NOT NULL,
    external_id text NOT NULL,
    name text NOT NULL,
    city text,
    country text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE federation_club; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.federation_club IS '@name federation_club
@behavior -insert -update -delete -list';


--
-- Name: federation_club_id_seq; Type: SEQUENCE; Schema: federated; Owner: -
--

ALTER TABLE federated.federation_club ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME federated.federation_club_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: judge_score; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.judge_score (
    round_id bigint NOT NULL,
    dance_order integer NOT NULL,
    dance_code text NOT NULL,
    judge_person_id bigint NOT NULL,
    competitor_id text NOT NULL,
    component federated.score_component NOT NULL,
    score numeric(10,3) NOT NULL,
    raw_score text
);


--
-- Name: TABLE judge_score; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.judge_score IS '@name judge_score
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: COLUMN judge_score.component; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.judge_score.component IS '@behavior +attribute:orderBy';


--
-- Name: person; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.person (
    id text GENERATED ALWAYS AS (((federation || ':'::text) || (external_id)::text)) STORED NOT NULL,
    federation text NOT NULL,
    external_id bigint NOT NULL,
    canonical_name text,
    first_name text,
    last_name text,
    gender federated.gender,
    dob date,
    nationality text,
    age_group text,
    medical_checkup_expiration date,
    medical_checkup_type text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    search_name text GENERATED ALWAYS AS (app_private.normalize_name(COALESCE(canonical_name, public.immutable_concat_ws(' '::text, VARIADIC ARRAY[first_name, last_name])))) STORED,
    age_group_code text,
    birth_year integer,
    national_reference text
);


--
-- Name: TABLE person; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.person IS '@name federation_person
@behavior -insert -update -delete -list
@ref people via:public.person_federation_link;public.person behavior:"+list -connection"';


--
-- Name: COLUMN person.age_group; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.person.age_group IS '@behavior -*';


--
-- Name: COLUMN person.age_group_code; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.person.age_group_code IS '@name ageGroup
@behavior -insert -update';


--
-- Name: COLUMN person.national_reference; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.person.national_reference IS '@behavior -*';


--
-- Name: person_club_membership; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.person_club_membership (
    person_id text NOT NULL,
    club_id bigint NOT NULL,
    valid_from date NOT NULL,
    valid_to date,
    CONSTRAINT person_club_membership_check CHECK (((valid_to IS NULL) OR (valid_to >= valid_from)))
);


--
-- Name: TABLE person_club_membership; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.person_club_membership IS '@name federation_person_club_membership
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: person_license; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.person_license (
    person_id text NOT NULL,
    federation text NOT NULL,
    kind federated.person_license_kind NOT NULL,
    discipline federated.person_license_discipline DEFAULT 'general'::federated.person_license_discipline NOT NULL,
    grade text,
    valid_until date,
    status federated.person_license_status DEFAULT 'unknown'::federated.person_license_status NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE person_license; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.person_license IS '@name federation_person_license
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: COLUMN person_license.kind; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.person_license.kind IS '@behavior +attribute:orderBy';


--
-- Name: COLUMN person_license.discipline; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON COLUMN federated.person_license.discipline IS '@behavior +attribute:orderBy';


--
-- Name: ranklist; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.ranklist (
    id bigint NOT NULL,
    federation text NOT NULL,
    category_id bigint NOT NULL,
    name text NOT NULL
);


--
-- Name: TABLE ranklist; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.ranklist IS '@name ranklist
@behavior -insert -update -delete -list';


--
-- Name: ranklist_entry; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.ranklist_entry (
    snapshot_id bigint NOT NULL,
    competitor_id text NOT NULL,
    ranking integer NOT NULL,
    ranking_to integer,
    points numeric(10,3),
    CONSTRAINT ranklist_entry_check CHECK (((ranking_to IS NULL) OR (ranking_to >= ranking)))
);


--
-- Name: TABLE ranklist_entry; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.ranklist_entry IS '@name ranklist_entry
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: ranklist_id_seq; Type: SEQUENCE; Schema: federated; Owner: -
--

ALTER TABLE federated.ranklist ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME federated.ranklist_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: ranklist_snapshot; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.ranklist_snapshot (
    id bigint NOT NULL,
    ranklist_id bigint NOT NULL,
    kind text DEFAULT 'default'::text NOT NULL,
    as_of_date date NOT NULL
);


--
-- Name: TABLE ranklist_snapshot; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.ranklist_snapshot IS '@name ranklist_snapshot
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: ranklist_snapshot_id_seq; Type: SEQUENCE; Schema: federated; Owner: -
--

ALTER TABLE federated.ranklist_snapshot ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME federated.ranklist_snapshot_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: round_dance; Type: TABLE; Schema: federated; Owner: -
--

CREATE TABLE federated.round_dance (
    round_id bigint NOT NULL,
    dance_program_id bigint NOT NULL,
    dance_code text NOT NULL,
    dance_order integer NOT NULL
);


--
-- Name: TABLE round_dance; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON TABLE federated.round_dance IS '@name competition_round_dance
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single
@ref dance singular via:federated.dance_program_dance;federated.dance';


--
-- Name: access_credential_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.access_credential ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.access_credential_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: access_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.access_event (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    external_id text NOT NULL,
    device text NOT NULL,
    kind public.access_credential_kind NOT NULL,
    code text NOT NULL,
    person_id bigint,
    occurred_at timestamp with time zone NOT NULL,
    received_at timestamp with time zone DEFAULT now() NOT NULL,
    allowed boolean NOT NULL,
    reason text DEFAULT ''::text NOT NULL,
    location_id bigint,
    CONSTRAINT access_event_code_check CHECK (((code <> ''::text) AND (code = btrim(code)))),
    CONSTRAINT access_event_device_check CHECK (((device <> ''::text) AND (device = btrim(device)))),
    CONSTRAINT access_event_external_id_check CHECK (((external_id <> ''::text) AND (external_id = btrim(external_id))))
);


--
-- Name: TABLE access_event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.access_event IS '@behavior +list -connection -insert -update -delete';


--
-- Name: access_event_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.access_event ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.access_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: account_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.account ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.account_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: accounting_period; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.accounting_period (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    since timestamp with time zone NOT NULL,
    until timestamp with time zone NOT NULL,
    range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE accounting_period; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.accounting_period IS '@behavior -*';


--
-- Name: accounting_period_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.accounting_period ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.accounting_period_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
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

ALTER SEQUENCE public.aktuality_at_id_seq OWNED BY public.aktuality.id;


--
-- Name: announcement_attachment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.announcement_attachment (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    announcement_id bigint NOT NULL,
    file_id bigint NOT NULL,
    inline boolean DEFAULT false NOT NULL
);


--
-- Name: TABLE announcement_attachment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.announcement_attachment IS '@behavior -insert -update -delete -query:resource:connection -query:resource:list -query:resource:single';


--
-- Name: announcement_audience; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.announcement_audience (
    id bigint NOT NULL,
    announcement_id bigint NOT NULL,
    cohort_id bigint,
    audience_role public.announcement_audience_role,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    CONSTRAINT announcement_audience_audience_check CHECK (((cohort_id IS NULL) <> (audience_role IS NULL)))
);


--
-- Name: TABLE announcement_audience; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.announcement_audience IS '@behavior -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';


--
-- Name: announcement_audience_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.announcement_audience ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.announcement_audience_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: announcement_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.announcement ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.announcement_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: article_attachment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.article_attachment (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    aktuality_id bigint NOT NULL,
    file_id bigint NOT NULL,
    inline boolean DEFAULT false NOT NULL
);


--
-- Name: TABLE article_attachment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.article_attachment IS '@behavior -insert -update -delete -query:resource:connection -query:resource:list -query:resource:single';


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
-- Name: TABLE cohort_group; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.cohort_group IS '@behavior -insert -update -delete';


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
-- Name: cohort_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.cohort ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.cohort_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


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
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    CONSTRAINT cohort_membership_until_gt_since CHECK ((until > since))
);


--
-- Name: TABLE cohort_membership; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.cohort_membership IS '@behavior +list -connection -query:resource:list -query:resource:connection';


--
-- Name: COLUMN cohort_membership.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.cohort_membership.active_range IS '@behavior -*';


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
-- Name: cohort_subscription_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.cohort_subscription ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.cohort_subscription_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: person_federation_link; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.person_federation_link WITH (security_invoker='true') AS
 SELECT person.id AS person_id,
    'csts'::text AS federation,
    (person.csts_id)::bigint AS external_id
   FROM public.person
  WHERE (person.csts_id <> 0)
UNION ALL
 SELECT person.id AS person_id,
    'wdsf'::text AS federation,
    (person.wdsf_id)::bigint AS external_id
   FROM public.person
  WHERE (person.wdsf_id <> 0);


--
-- Name: VIEW person_federation_link; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.person_federation_link IS '@primaryKey person_id,federation
@foreignKey (person_id) references public.person (id)
@foreignKey (federation,external_id) references federated.person (federation,external_id)
@behavior -single -list -connection -insert -update -delete';


--
-- Name: couple_federation_link; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.couple_federation_link WITH (security_invoker='true') AS
 WITH links AS MATERIALIZED (
         SELECT person_federation_link.person_id,
            person_federation_link.federation,
            person_federation_link.external_id
           FROM public.person_federation_link
        )
 SELECT c.id AS couple_id,
    fc.id AS competitor_id
   FROM (((((((public.couple c
     JOIN links ml ON ((ml.person_id = c.man_id)))
     JOIN federated.person mp ON (((mp.federation = ml.federation) AND (mp.external_id = ml.external_id))))
     JOIN federated.competitor_component mc ON (((mc.person_id = mp.id) AND (mc.role = 'lead'::federated.competitor_role))))
     JOIN federated.competitor fc ON (((fc.id = mc.competitor_id) AND (fc.competitor_type = 'couple'::federated.competitor_type))))
     JOIN federated.competitor_component wc ON (((wc.competitor_id = fc.id) AND (wc.role = 'follow'::federated.competitor_role))))
     JOIN federated.person wp ON ((wp.id = wc.person_id)))
     JOIN links wl ON (((wl.person_id = c.woman_id) AND ((wl.federation = wp.federation) AND (wl.external_id = wp.external_id)))));


--
-- Name: VIEW couple_federation_link; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.couple_federation_link IS '@primaryKey couple_id,competitor_id
@foreignKey (couple_id) references public.couple (id)
@foreignKey (competitor_id) references federated.competitor (id)
@behavior -single -list -connection -insert -update -delete';


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
-- Name: current_cohort_membership; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.current_cohort_membership AS
 SELECT cohort_id,
    person_id,
    since,
    until,
    created_at,
    updated_at,
    id,
    tenant_id,
    active_range,
    status
   FROM public.cohort_membership
  WHERE ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (status = 'active'::public.relationship_status));


--
-- Name: VIEW current_cohort_membership; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.current_cohort_membership IS '@behavior -*';


--
-- Name: current_tenant_administrator; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.current_tenant_administrator AS
 SELECT tenant_id,
    person_id,
    since,
    until,
    created_at,
    updated_at,
    id,
    is_visible,
    description,
    active_range,
    status
   FROM public.tenant_administrator
  WHERE ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (status = 'active'::public.relationship_status));


--
-- Name: VIEW current_tenant_administrator; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.current_tenant_administrator IS '@behavior -*';


--
-- Name: current_tenant_membership; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.current_tenant_membership AS
 SELECT tenant_id,
    person_id,
    since,
    until,
    created_at,
    updated_at,
    id,
    active_range,
    status
   FROM public.tenant_membership
  WHERE ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (status = 'active'::public.relationship_status));


--
-- Name: VIEW current_tenant_membership; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.current_tenant_membership IS '@behavior -*';


--
-- Name: current_tenant_trainer; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.current_tenant_trainer AS
 SELECT tenant_id,
    person_id,
    since,
    until,
    created_at,
    updated_at,
    id,
    is_visible,
    description,
    active_range,
    member_price_45min,
    member_payout_45min,
    guest_price_45min,
    guest_payout_45min,
    create_payout_payments,
    status
   FROM public.tenant_trainer
  WHERE ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (status = 'active'::public.relationship_status));


--
-- Name: VIEW current_tenant_trainer; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.current_tenant_trainer IS '@behavior -*';


--
-- Name: dokumenty; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.dokumenty (
    id bigint CONSTRAINT dokumenty_d_id_not_null NOT NULL,
    d_path text NOT NULL,
    d_name text NOT NULL,
    d_filename text NOT NULL,
    d_kategorie smallint NOT NULL,
    d_kdo bigint NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    d_timestamp timestamp with time zone GENERATED ALWAYS AS (updated_at) STORED
);


--
-- Name: TABLE dokumenty; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.dokumenty IS '@behavior -*';


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

ALTER SEQUENCE public.dokumenty_d_id_seq OWNED BY public.dokumenty.id;


--
-- Name: event_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.event ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: event_lesson_request_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.event_lesson_request ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.event_lesson_request_id_seq
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
-- Name: event_series; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_series (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    name text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE event_series; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_series IS '@behavior -query:resource:list -query:resource:connection';


--
-- Name: event_series_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.event_series ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.event_series_id_seq
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

COMMENT ON TABLE public.event_target_cohort IS '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';


--
-- Name: event_target_cohort_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.event_target_cohort ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
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
-- Name: file; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.file (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    object_key text NOT NULL,
    name text NOT NULL,
    content_type text,
    byte_size bigint,
    uploaded_by bigint DEFAULT public.current_user_id(),
    uploaded_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    display_name text,
    is_public boolean DEFAULT false NOT NULL,
    url text GENERATED ALWAYS AS (((('/f/'::text || id) || '/'::text) || name)) STORED NOT NULL
);


--
-- Name: TABLE file; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.file IS '@behavior -insert -update -delete';


--
-- Name: file_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.file ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.file_id_seq
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

COMMENT ON TABLE public.form_responses IS '@behavior -insert -update -delete';


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
-- Name: location_image; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.location_image (
    tenant_id bigint DEFAULT public.current_tenant_id() CONSTRAINT tenant_location_image_tenant_id_not_null NOT NULL,
    location_id bigint CONSTRAINT tenant_location_image_location_id_not_null NOT NULL,
    file_id bigint CONSTRAINT tenant_location_image_file_id_not_null NOT NULL
);


--
-- Name: TABLE location_image; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.location_image IS '@behavior +list -connection -insert -update -delete -query:resource:connection -query:resource:list';


--
-- Name: membership_application_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.membership_application ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.membership_application_id_seq
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
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    intent text DEFAULT 'password_reset'::text NOT NULL,
    event_id bigint,
    CONSTRAINT otp_token_intent_check CHECK ((((intent = 'password_reset'::text) AND (event_id IS NULL)) OR ((intent = 'event_handoff'::text) AND (event_id IS NOT NULL))))
);


--
-- Name: TABLE otp_token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.otp_token IS '@behavior -*';


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
-- Name: payment_debtor_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.payment_debtor ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.payment_debtor_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: payment_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.payment ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.payment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: payment_recipient; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.payment_recipient (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    payment_id bigint NOT NULL,
    account_id bigint NOT NULL,
    amount numeric(19,4) NOT NULL
);


--
-- Name: TABLE payment_recipient; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.payment_recipient IS '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection';


--
-- Name: payment_recipient_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.payment_recipient ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.payment_recipient_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


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
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    email public.citext NOT NULL,
    event_id bigint
);


--
-- Name: TABLE person_invitation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.person_invitation IS '@behavior +list -connection -update -query:resource:list -query:resource:connection';


--
-- Name: COLUMN person_invitation.event_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.person_invitation.event_id IS '@behavior -*';


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
-- Name: user_proxy; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_proxy (
    user_id bigint NOT NULL,
    person_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    since timestamp with time zone DEFAULT now(),
    until timestamp with time zone,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    CONSTRAINT user_proxy_until_gt_since CHECK ((until > since))
);


--
-- Name: TABLE user_proxy; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_proxy IS '@behavior +list -connection -query:resource:list -query:resource:connection';


--
-- Name: COLUMN user_proxy.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_proxy.active_range IS '@behavior -*';


--
-- Name: person_last_activity; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.person_last_activity WITH (security_barrier='false') AS
 SELECT ( SELECT public.current_tenant_id() AS current_tenant_id) AS tenant_id,
    p.id AS person_id,
    web.web_at,
    COALESCE(card.card_entry_at, ( SELECT '1970-01-01 00:00:00+00'::timestamp with time zone AS timestamptz
           FROM public.access_credential c
          WHERE ((c.person_id = p.id) AND (c.tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)))
         LIMIT 1)) AS card_entry_at,
    training.training_at,
    competition.competition_on
   FROM ((((public.person p
     LEFT JOIN LATERAL ( SELECT COALESCE(max(u.last_active_at), '1970-01-01 00:00:00+00'::timestamp with time zone) AS web_at
           FROM (public.user_proxy up
             JOIN public.users u ON ((u.id = up.user_id)))
          WHERE ((up.person_id = p.id) AND (up.status = 'active'::public.relationship_status))
         HAVING (count(*) > 0)) web ON (true))
     LEFT JOIN LATERAL ( SELECT e.occurred_at AS card_entry_at
           FROM public.access_event e
          WHERE ((e.tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (e.person_id = p.id) AND e.allowed AND (e.occurred_at <= now()))
          ORDER BY e.occurred_at DESC
         LIMIT 1) card ON (true))
     LEFT JOIN LATERAL ( SELECT COALESCE(max(e.since) FILTER (WHERE (((r.status = 'attended'::public.attendance_type) OR (e.type = 'lesson'::public.event_type)) AND (NOT e.is_cancelled) AND (e.since <= now()))), '1970-01-01 00:00:00+00'::timestamp with time zone) AS training_at
           FROM (public.event_registration r
             JOIN public.event e ON (((e.id = r.event_id) AND (e.tenant_id = r.tenant_id))))
          WHERE ((r.person_id = p.id) AND (r.tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (r.registration_status = 'active'::public.event_registration_status) AND (e.type = ANY (ARRAY['lesson'::public.event_type, 'group'::public.event_type, 'camp'::public.event_type])))
         HAVING (count(*) > 0)) training ON (true))
     LEFT JOIN LATERAL ( SELECT COALESCE(max(
                CASE
                    WHEN (c.latest_result_at > CURRENT_DATE) THEN ( SELECT max(comp.start_date) AS max
                       FROM (federated.competition_result r
                         JOIN federated.competition comp ON ((comp.id = r.competition_id)))
                      WHERE ((r.competitor_id = c.id) AND (comp.start_date <= CURRENT_DATE)))
                    ELSE c.latest_result_at
                END), '1970-01-01'::date) AS competition_on
           FROM (((( VALUES ('csts'::text,(p.csts_id)::bigint), ('wdsf'::text,(p.wdsf_id)::bigint)) l(federation, external_id)
             JOIN federated.person fp ON (((fp.federation = l.federation) AND (fp.external_id = l.external_id))))
             LEFT JOIN federated.competitor_component cc ON ((cc.person_id = fp.id)))
             LEFT JOIN federated.competitor c ON ((c.id = cc.competitor_id)))
          WHERE (l.external_id <> 0)
         HAVING (count(*) > 0)) competition ON (true))
  WHERE ((( SELECT pg_has_role(CURRENT_USER, 'trainer'::name, 'member'::text) AS pg_has_role) OR (p.id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[]))) AND ((EXISTS ( SELECT
           FROM public.current_tenant_membership m
          WHERE (m.person_id = p.id))) OR (EXISTS ( SELECT
           FROM public.current_tenant_trainer t
          WHERE (t.person_id = p.id))) OR (EXISTS ( SELECT
           FROM public.current_tenant_administrator a
          WHERE (a.person_id = p.id))) OR (EXISTS ( SELECT
           FROM public.current_cohort_membership cm
          WHERE (cm.person_id = p.id)))));


--
-- Name: VIEW person_last_activity; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.person_last_activity IS '@primaryKey person_id
@unique tenant_id,person_id
@foreignKey (person_id) references public.person (id)|@foreignFieldName lastActivity
@behavior -query:resource:single -query:resource:list -query:resource:connection -insert -update -delete';


--
-- Name: COLUMN person_last_activity.tenant_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.person_last_activity.tenant_id IS '@notNull';


--
-- Name: COLUMN person_last_activity.person_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.person_last_activity.person_id IS '@notNull';


--
-- Name: posting_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.posting ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.posting_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: scoreboard_manual_adjustment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.scoreboard_manual_adjustment (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    person_id bigint NOT NULL,
    cohort_id bigint,
    points integer NOT NULL,
    reason text,
    awarded_at date DEFAULT CURRENT_DATE NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE scoreboard_manual_adjustment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.scoreboard_manual_adjustment IS '@behavior +list -connection -query:resource:list -query:resource:connection';


--
-- Name: scoreboard_manual_adjustment_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.scoreboard_manual_adjustment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: scoreboard_manual_adjustment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.scoreboard_manual_adjustment_id_seq OWNED BY public.scoreboard_manual_adjustment.id;


--
-- Name: security_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.security_event (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    user_id bigint,
    person_id bigint,
    actor_user_id bigint DEFAULT public.current_user_id(),
    kind public.security_event_kind NOT NULL,
    method public.security_event_method NOT NULL,
    occurred_at timestamp with time zone DEFAULT now() NOT NULL,
    effective_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE security_event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.security_event IS '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';


--
-- Name: security_event_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.security_event ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.security_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


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
-- Name: tenant_location_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.location ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
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
-- Name: transaction_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.transaction ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.transaction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


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

ALTER SEQUENCE public.users_u_id_seq OWNED BY public.users.id;


--
-- Name: platby_category pc_id; Type: DEFAULT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_category ALTER COLUMN pc_id SET DEFAULT nextval('app_private.platby_category_pc_id_seq'::regclass);


--
-- Name: platby_category_group pcg_id; Type: DEFAULT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_category_group ALTER COLUMN pcg_id SET DEFAULT nextval('app_private.platby_category_group_pcg_id_seq'::regclass);


--
-- Name: platby_group pg_id; Type: DEFAULT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_group ALTER COLUMN pg_id SET DEFAULT nextval('app_private.platby_group_pg_id_seq'::regclass);


--
-- Name: platby_group_skupina pgs_id; Type: DEFAULT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_group_skupina ALTER COLUMN pgs_id SET DEFAULT nextval('app_private.platby_group_skupina_pgs_id_seq'::regclass);


--
-- Name: platby_item pi_id; Type: DEFAULT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_item ALTER COLUMN pi_id SET DEFAULT nextval('app_private.platby_item_pi_id_seq'::regclass);


--
-- Name: platby_raw pr_id; Type: DEFAULT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_raw ALTER COLUMN pr_id SET DEFAULT nextval('app_private.platby_raw_pr_id_seq'::regclass);


--
-- Name: frontier id; Type: DEFAULT; Schema: crawler; Owner: -
--

ALTER TABLE ONLY crawler.frontier ALTER COLUMN id SET DEFAULT nextval('crawler.frontier_id_seq'::regclass);


--
-- Name: json_response id; Type: DEFAULT; Schema: crawler; Owner: -
--

ALTER TABLE ONLY crawler.json_response ALTER COLUMN id SET DEFAULT nextval('crawler.json_response_id_seq'::regclass);


--
-- Name: aktuality id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality ALTER COLUMN id SET DEFAULT nextval('public.aktuality_at_id_seq'::regclass);


--
-- Name: dokumenty id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dokumenty ALTER COLUMN id SET DEFAULT nextval('public.dokumenty_d_id_seq'::regclass);


--
-- Name: scoreboard_manual_adjustment id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scoreboard_manual_adjustment ALTER COLUMN id SET DEFAULT nextval('public.scoreboard_manual_adjustment_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_u_id_seq'::regclass);


--
-- Name: account account_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_pkey PRIMARY KEY (id);


--
-- Name: account_balances; Type: MATERIALIZED VIEW; Schema: public; Owner: -
--

CREATE MATERIALIZED VIEW public.account_balances AS
 SELECT account.id,
    (COALESCE(account.opening_balance, 0.0) + COALESCE(sum(posting.amount), 0.0)) AS balance
   FROM (public.account
     LEFT JOIN public.posting ON ((account.id = posting.account_id)))
  GROUP BY account.id
  WITH NO DATA;


--
-- Name: MATERIALIZED VIEW account_balances; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON MATERIALIZED VIEW public.account_balances IS '@behavior -*';


--
-- Name: platby_category idx_23855_primary; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_category
    ADD CONSTRAINT idx_23855_primary PRIMARY KEY (pc_id);


--
-- Name: platby_category_group idx_23868_primary; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_category_group
    ADD CONSTRAINT idx_23868_primary PRIMARY KEY (pcg_id);


--
-- Name: platby_group idx_23874_primary; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_group
    ADD CONSTRAINT idx_23874_primary PRIMARY KEY (pg_id);


--
-- Name: platby_group_skupina idx_23885_primary; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_group_skupina
    ADD CONSTRAINT idx_23885_primary PRIMARY KEY (pgs_id);


--
-- Name: platby_item idx_23891_primary; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_item
    ADD CONSTRAINT idx_23891_primary PRIMARY KEY (pi_id);


--
-- Name: platby_raw idx_23898_primary; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_raw
    ADD CONSTRAINT idx_23898_primary PRIMARY KEY (pr_id);


--
-- Name: platby_category_group platby_category_group_unique_id; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_category_group
    ADD CONSTRAINT platby_category_group_unique_id UNIQUE (id);


--
-- Name: platby_category platby_category_unique_id; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_category
    ADD CONSTRAINT platby_category_unique_id UNIQUE (id);


--
-- Name: platby_group_skupina platby_group_skupina_unique_id; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_unique_id UNIQUE (id);


--
-- Name: platby_group platby_group_unique_id; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_group
    ADD CONSTRAINT platby_group_unique_id UNIQUE (id);


--
-- Name: platby_item platby_item_unique_id; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_item
    ADD CONSTRAINT platby_item_unique_id UNIQUE (id);


--
-- Name: platby_raw platby_raw_unique_id; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_raw
    ADD CONSTRAINT platby_raw_unique_id UNIQUE (id);


--
-- Name: system_admin_user system_admin_user_pkey; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.system_admin_user
    ADD CONSTRAINT system_admin_user_pkey PRIMARY KEY (user_id);


--
-- Name: frontier frontier_federation_kind_key_key; Type: CONSTRAINT; Schema: crawler; Owner: -
--

ALTER TABLE ONLY crawler.frontier
    ADD CONSTRAINT frontier_federation_kind_key_key UNIQUE (federation, kind, key);


--
-- Name: frontier frontier_pkey; Type: CONSTRAINT; Schema: crawler; Owner: -
--

ALTER TABLE ONLY crawler.frontier
    ADD CONSTRAINT frontier_pkey PRIMARY KEY (id);


--
-- Name: incremental_ranges incremental_ranges_pkey; Type: CONSTRAINT; Schema: crawler; Owner: -
--

ALTER TABLE ONLY crawler.incremental_ranges
    ADD CONSTRAINT incremental_ranges_pkey PRIMARY KEY (federation, kind);


--
-- Name: json_response_cache json_response_cache_pkey; Type: CONSTRAINT; Schema: crawler; Owner: -
--

ALTER TABLE ONLY crawler.json_response_cache
    ADD CONSTRAINT json_response_cache_pkey PRIMARY KEY (content_hash);


--
-- Name: json_response json_response_pkey; Type: CONSTRAINT; Schema: crawler; Owner: -
--

ALTER TABLE ONLY crawler.json_response
    ADD CONSTRAINT json_response_pkey PRIMARY KEY (id);


--
-- Name: rate_limit_rule rate_limit_rule_pkey; Type: CONSTRAINT; Schema: crawler; Owner: -
--

ALTER TABLE ONLY crawler.rate_limit_rule
    ADD CONSTRAINT rate_limit_rule_pkey PRIMARY KEY (host);


--
-- Name: age_group age_group_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.age_group
    ADD CONSTRAINT age_group_pkey PRIMARY KEY (code);


--
-- Name: category category_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.category
    ADD CONSTRAINT category_pkey PRIMARY KEY (id);


--
-- Name: category category_series_discipline_age_group_gender_group_class_com_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.category
    ADD CONSTRAINT category_series_discipline_age_group_gender_group_class_com_key UNIQUE (series, discipline, age_group, gender_group, class, competitor_type);


--
-- Name: competition_entry competition_entry_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_entry
    ADD CONSTRAINT competition_entry_pkey PRIMARY KEY (competition_id, competitor_id);


--
-- Name: competition competition_federation_external_id_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_federation_external_id_key UNIQUE (federation, external_id);


--
-- Name: competition competition_federation_id_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_federation_id_key UNIQUE (federation, id);


--
-- Name: CONSTRAINT competition_federation_id_key ON competition; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_federation_id_key ON federated.competition IS '@behavior -query:resource:single';


--
-- Name: competition competition_id_category_id_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_id_category_id_key UNIQUE (id, category_id);


--
-- Name: CONSTRAINT competition_id_category_id_key ON competition; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_id_category_id_key ON federated.competition IS '@behavior -query:resource:single';


--
-- Name: competition competition_id_event_id_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_id_event_id_key UNIQUE (id, event_id);


--
-- Name: CONSTRAINT competition_id_event_id_key ON competition; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_id_event_id_key ON federated.competition IS '@behavior -query:resource:single';


--
-- Name: competition_official competition_official_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_official
    ADD CONSTRAINT competition_official_pkey PRIMARY KEY (competition_id, person_id, role);


--
-- Name: competition competition_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_pkey PRIMARY KEY (id);


--
-- Name: competition_result competition_result_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_result
    ADD CONSTRAINT competition_result_pkey PRIMARY KEY (competition_id, competitor_id);


--
-- Name: competition_round competition_round_competition_id_round_key_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_competition_id_round_key_key UNIQUE (competition_id, round_key);


--
-- Name: competition_round competition_round_id_competition_id_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_id_competition_id_key UNIQUE (id, competition_id);


--
-- Name: CONSTRAINT competition_round_id_competition_id_key ON competition_round; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_round_id_competition_id_key ON federated.competition_round IS '@behavior -query:resource:single';


--
-- Name: competition_round competition_round_id_dance_program_id_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_id_dance_program_id_key UNIQUE (id, dance_program_id);


--
-- Name: CONSTRAINT competition_round_id_dance_program_id_key ON competition_round; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_round_id_dance_program_id_key ON federated.competition_round IS '@behavior -query:resource:single';


--
-- Name: competition_round_judge competition_round_judge_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_round_judge
    ADD CONSTRAINT competition_round_judge_pkey PRIMARY KEY (round_id, person_judge_id);


--
-- Name: competition_round_judge competition_round_judge_round_id_judge_index_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_round_judge
    ADD CONSTRAINT competition_round_judge_round_id_judge_index_key UNIQUE (round_id, judge_index);


--
-- Name: competition_round competition_round_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_pkey PRIMARY KEY (id);


--
-- Name: competition_round_result competition_round_result_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_round_result
    ADD CONSTRAINT competition_round_result_pkey PRIMARY KEY (round_id, competitor_id);


--
-- Name: competitor_category_progress competitor_category_progress_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor_category_progress
    ADD CONSTRAINT competitor_category_progress_pkey PRIMARY KEY (competitor_id, category_id);


--
-- Name: competitor_club_affiliation competitor_club_affiliation_competitor_id_club_id_daterang_excl; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor_club_affiliation
    ADD CONSTRAINT competitor_club_affiliation_competitor_id_club_id_daterang_excl EXCLUDE USING gist (competitor_id WITH =, club_id WITH =, daterange(valid_from, COALESCE(valid_to, 'infinity'::date), '[)'::text) WITH &&);


--
-- Name: competitor_club_affiliation competitor_club_affiliation_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor_club_affiliation
    ADD CONSTRAINT competitor_club_affiliation_pkey PRIMARY KEY (competitor_id, club_id, valid_from);


--
-- Name: competitor_component competitor_component_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor_component
    ADD CONSTRAINT competitor_component_pkey PRIMARY KEY (competitor_id, person_id);


--
-- Name: competitor competitor_federation_external_id_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_federation_external_id_key UNIQUE (federation, external_id);


--
-- Name: competitor competitor_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_pkey PRIMARY KEY (id);


--
-- Name: couple_age_rule couple_age_rule_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.couple_age_rule
    ADD CONSTRAINT couple_age_rule_pkey PRIMARY KEY (age_group_code, division);


--
-- Name: dance dance_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.dance
    ADD CONSTRAINT dance_pkey PRIMARY KEY (code);


--
-- Name: dance_program dance_program_code_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.dance_program
    ADD CONSTRAINT dance_program_code_key UNIQUE (code);


--
-- Name: dance_program_dance dance_program_dance_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.dance_program_dance
    ADD CONSTRAINT dance_program_dance_pkey PRIMARY KEY (program_id, dance_code);


--
-- Name: dance_program_dance dance_program_dance_program_id_dance_order_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.dance_program_dance
    ADD CONSTRAINT dance_program_dance_program_id_dance_order_key UNIQUE (program_id, dance_order);


--
-- Name: dance_program dance_program_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.dance_program
    ADD CONSTRAINT dance_program_pkey PRIMARY KEY (id);


--
-- Name: event event_federation_external_id_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_federation_external_id_key UNIQUE (federation, external_id);


--
-- Name: event event_federation_id_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_federation_id_key UNIQUE (federation, id);


--
-- Name: CONSTRAINT event_federation_id_key ON event; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT event_federation_id_key ON federated.event IS '@behavior -query:resource:single';


--
-- Name: event_official event_official_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.event_official
    ADD CONSTRAINT event_official_pkey PRIMARY KEY (event_id, person_id, role, discipline);


--
-- Name: event event_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_pkey PRIMARY KEY (id);


--
-- Name: federation_club federation_club_federation_external_id_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.federation_club
    ADD CONSTRAINT federation_club_federation_external_id_key UNIQUE (federation, external_id);


--
-- Name: federation_club federation_club_federation_id_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.federation_club
    ADD CONSTRAINT federation_club_federation_id_key UNIQUE (federation, id);


--
-- Name: CONSTRAINT federation_club_federation_id_key ON federation_club; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT federation_club_federation_id_key ON federated.federation_club IS '@behavior -query:resource:single';


--
-- Name: federation_club federation_club_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.federation_club
    ADD CONSTRAINT federation_club_pkey PRIMARY KEY (id);


--
-- Name: federation federation_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.federation
    ADD CONSTRAINT federation_pkey PRIMARY KEY (code);


--
-- Name: judge_score judge_score_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_pkey PRIMARY KEY (round_id, dance_order, judge_person_id, competitor_id, component);


--
-- Name: person_club_membership person_club_membership_person_id_club_id_daterange_excl; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.person_club_membership
    ADD CONSTRAINT person_club_membership_person_id_club_id_daterange_excl EXCLUDE USING gist (person_id WITH =, club_id WITH =, daterange(valid_from, COALESCE(valid_to, 'infinity'::date), '[)'::text) WITH &&);


--
-- Name: person_club_membership person_club_membership_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.person_club_membership
    ADD CONSTRAINT person_club_membership_pkey PRIMARY KEY (person_id, club_id, valid_from);


--
-- Name: person person_federation_external_id_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.person
    ADD CONSTRAINT person_federation_external_id_key UNIQUE (federation, external_id);


--
-- Name: person_license person_license_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.person_license
    ADD CONSTRAINT person_license_pkey PRIMARY KEY (person_id, kind, discipline);


--
-- Name: person person_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.person
    ADD CONSTRAINT person_pkey PRIMARY KEY (id);


--
-- Name: ranklist_entry ranklist_entry_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.ranklist_entry
    ADD CONSTRAINT ranklist_entry_pkey PRIMARY KEY (snapshot_id, competitor_id);


--
-- Name: ranklist ranklist_federation_category_id_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.ranklist
    ADD CONSTRAINT ranklist_federation_category_id_key UNIQUE (federation, category_id);


--
-- Name: ranklist ranklist_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.ranklist
    ADD CONSTRAINT ranklist_pkey PRIMARY KEY (id);


--
-- Name: ranklist_snapshot ranklist_snapshot_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.ranklist_snapshot
    ADD CONSTRAINT ranklist_snapshot_pkey PRIMARY KEY (id);


--
-- Name: ranklist_snapshot ranklist_snapshot_ranklist_id_as_of_date_kind_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.ranklist_snapshot
    ADD CONSTRAINT ranklist_snapshot_ranklist_id_as_of_date_kind_key UNIQUE (ranklist_id, as_of_date, kind);


--
-- Name: round_dance round_dance_pkey; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.round_dance
    ADD CONSTRAINT round_dance_pkey PRIMARY KEY (round_id, dance_order);


--
-- Name: round_dance round_dance_round_id_dance_order_dance_code_key; Type: CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.round_dance
    ADD CONSTRAINT round_dance_round_id_dance_order_dance_code_key UNIQUE (round_id, dance_order, dance_code);


--
-- Name: CONSTRAINT round_dance_round_id_dance_order_dance_code_key ON round_dance; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT round_dance_round_id_dance_order_dance_code_key ON federated.round_dance IS '@behavior -query:resource:single';


--
-- Name: access_credential access_credential_no_overlap; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.access_credential
    ADD CONSTRAINT access_credential_no_overlap EXCLUDE USING gist (tenant_id WITH =, kind WITH =, code WITH =, valid_range WITH &&);


--
-- Name: access_credential access_credential_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.access_credential
    ADD CONSTRAINT access_credential_pkey PRIMARY KEY (id);


--
-- Name: access_event access_event_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.access_event
    ADD CONSTRAINT access_event_pkey PRIMARY KEY (id);


--
-- Name: access_event access_event_tenant_id_external_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.access_event
    ADD CONSTRAINT access_event_tenant_id_external_id_key UNIQUE (tenant_id, external_id);


--
-- Name: account account_tenant_id_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_tenant_id_id_key UNIQUE (tenant_id, id);


--
-- Name: account account_tenant_id_person_id_currency_idx; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_tenant_id_person_id_currency_idx UNIQUE NULLS NOT DISTINCT (tenant_id, person_id, currency);


--
-- Name: accounting_period accounting_period__no_overlap__excl; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.accounting_period
    ADD CONSTRAINT accounting_period__no_overlap__excl EXCLUDE USING gist (tenant_id WITH =, range WITH &&);


--
-- Name: accounting_period accounting_period__tenant_since__uk; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.accounting_period
    ADD CONSTRAINT accounting_period__tenant_since__uk UNIQUE (tenant_id, since);


--
-- Name: accounting_period accounting_period_no_overlap; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.accounting_period
    ADD CONSTRAINT accounting_period_no_overlap EXCLUDE USING gist (tenant_id WITH =, range WITH &&);


--
-- Name: accounting_period accounting_period_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.accounting_period
    ADD CONSTRAINT accounting_period_pkey PRIMARY KEY (id);


--
-- Name: accounting_period accounting_period_tenant_id_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.accounting_period
    ADD CONSTRAINT accounting_period_tenant_id_id_key UNIQUE (tenant_id, id);


--
-- Name: announcement_attachment announcement_attachment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.announcement_attachment
    ADD CONSTRAINT announcement_attachment_pkey PRIMARY KEY (tenant_id, announcement_id, file_id);


--
-- Name: announcement_audience announcement_audience_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.announcement_audience
    ADD CONSTRAINT announcement_audience_pkey PRIMARY KEY (id);


--
-- Name: announcement announcement_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.announcement
    ADD CONSTRAINT announcement_pkey PRIMARY KEY (id);


--
-- Name: article_attachment article_attachment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.article_attachment
    ADD CONSTRAINT article_attachment_pkey PRIMARY KEY (tenant_id, aktuality_id, file_id);


--
-- Name: cohort_group cohort_group_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_group
    ADD CONSTRAINT cohort_group_pkey PRIMARY KEY (id);


--
-- Name: cohort_group cohort_group_tenant_id_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_group
    ADD CONSTRAINT cohort_group_tenant_id_id_key UNIQUE (tenant_id, id);


--
-- Name: CONSTRAINT cohort_group_tenant_id_id_key ON cohort_group; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT cohort_group_tenant_id_id_key ON public.cohort_group IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';


--
-- Name: cohort_membership cohort_membership_no_overlap; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_no_overlap EXCLUDE USING gist (cohort_id WITH =, person_id WITH =, active_range WITH &&);


--
-- Name: cohort_membership cohort_membership_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_pkey PRIMARY KEY (id);


--
-- Name: cohort cohort_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_pkey PRIMARY KEY (id);


--
-- Name: cohort_subscription cohort_subscription_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_subscription
    ADD CONSTRAINT cohort_subscription_pkey PRIMARY KEY (id);


--
-- Name: cohort cohort_tenant_id_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_tenant_id_id_key UNIQUE (tenant_id, id);


--
-- Name: CONSTRAINT cohort_tenant_id_id_key ON cohort; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT cohort_tenant_id_id_key ON public.cohort IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';


--
-- Name: couple couple_no_overlap; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_no_overlap EXCLUDE USING gist (man_id WITH =, woman_id WITH =, active_range WITH &&);


--
-- Name: couple couple_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_pkey PRIMARY KEY (id);


--
-- Name: event_lesson_request event_lesson_request_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_lesson_request
    ADD CONSTRAINT event_lesson_request_pkey PRIMARY KEY (id);


--
-- Name: event_lesson_request event_lesson_request_registration_id_trainer_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_lesson_request
    ADD CONSTRAINT event_lesson_request_registration_id_trainer_id_key UNIQUE (registration_id, trainer_id);


--
-- Name: event event_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_pkey PRIMARY KEY (id);


--
-- Name: event_registration event_registration_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_pkey PRIMARY KEY (id);


--
-- Name: event_series event_series_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_series
    ADD CONSTRAINT event_series_pkey PRIMARY KEY (id);


--
-- Name: event_series event_series_tenant_id_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_series
    ADD CONSTRAINT event_series_tenant_id_id_key UNIQUE (tenant_id, id);


--
-- Name: CONSTRAINT event_series_tenant_id_id_key ON event_series; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_series_tenant_id_id_key ON public.event_series IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';


--
-- Name: event_target_cohort event_target_cohort_event_id_cohort_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_event_id_cohort_id_key UNIQUE (event_id, cohort_id);


--
-- Name: event_target_cohort event_target_cohort_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_pkey PRIMARY KEY (id);


--
-- Name: event event_tenant_id_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_tenant_id_id_key UNIQUE (tenant_id, id);


--
-- Name: CONSTRAINT event_tenant_id_id_key ON event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_tenant_id_id_key ON public.event IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';


--
-- Name: event_trainer event_trainer_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_pkey PRIMARY KEY (id);


--
-- Name: event_trainer event_trainer_trainer_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_trainer_id_key UNIQUE (event_id, person_id);


--
-- Name: file file_object_key_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.file
    ADD CONSTRAINT file_object_key_key UNIQUE (object_key);


--
-- Name: file file_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.file
    ADD CONSTRAINT file_pkey PRIMARY KEY (id);


--
-- Name: file file_tenant_id_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.file
    ADD CONSTRAINT file_tenant_id_id_key UNIQUE (tenant_id, id);


--
-- Name: CONSTRAINT file_tenant_id_id_key ON file; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT file_tenant_id_id_key ON public.file IS '@behavior -query:resource:single';


--
-- Name: form_responses form_responses_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.form_responses
    ADD CONSTRAINT form_responses_pkey PRIMARY KEY (id);


--
-- Name: aktuality idx_23753_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT idx_23753_primary PRIMARY KEY (id);


--
-- Name: dokumenty idx_23771_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dokumenty
    ADD CONSTRAINT idx_23771_primary PRIMARY KEY (id);


--
-- Name: users idx_23964_primary; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT idx_23964_primary PRIMARY KEY (id);


--
-- Name: membership_application membership_application_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.membership_application
    ADD CONSTRAINT membership_application_pkey PRIMARY KEY (id);


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
-- Name: payment_debtor payment_debtor_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_debtor
    ADD CONSTRAINT payment_debtor_pkey PRIMARY KEY (id);


--
-- Name: payment payment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_pkey PRIMARY KEY (id);


--
-- Name: payment_recipient payment_recipient_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_recipient
    ADD CONSTRAINT payment_recipient_pkey PRIMARY KEY (id);


--
-- Name: payment payment_tenant_id_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_tenant_id_id_key UNIQUE (tenant_id, id);


--
-- Name: CONSTRAINT payment_tenant_id_id_key ON payment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT payment_tenant_id_id_key ON public.payment IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';


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
-- Name: person person_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person
    ADD CONSTRAINT person_pkey PRIMARY KEY (id);


--
-- Name: posting posting_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posting
    ADD CONSTRAINT posting_pkey PRIMARY KEY (id);


--
-- Name: scoreboard_manual_adjustment scoreboard_manual_adjustment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scoreboard_manual_adjustment
    ADD CONSTRAINT scoreboard_manual_adjustment_pkey PRIMARY KEY (id);


--
-- Name: security_event security_event_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.security_event
    ADD CONSTRAINT security_event_pkey PRIMARY KEY (id);


--
-- Name: tenant_administrator tenant_administrator_no_overlap; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_administrator
    ADD CONSTRAINT tenant_administrator_no_overlap EXCLUDE USING gist (tenant_id WITH =, person_id WITH =, active_range WITH &&);


--
-- Name: tenant_administrator tenant_administrator_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_administrator
    ADD CONSTRAINT tenant_administrator_pkey PRIMARY KEY (id);


--
-- Name: location_image tenant_location_image_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location_image
    ADD CONSTRAINT tenant_location_image_pkey PRIMARY KEY (tenant_id, location_id, file_id);


--
-- Name: location tenant_location_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location
    ADD CONSTRAINT tenant_location_pkey PRIMARY KEY (id);


--
-- Name: location tenant_location_tenant_id_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location
    ADD CONSTRAINT tenant_location_tenant_id_id_key UNIQUE (tenant_id, id);


--
-- Name: CONSTRAINT tenant_location_tenant_id_id_key ON location; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT tenant_location_tenant_id_id_key ON public.location IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';


--
-- Name: tenant_membership tenant_membership_no_overlap; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_membership
    ADD CONSTRAINT tenant_membership_no_overlap EXCLUDE USING gist (tenant_id WITH =, person_id WITH =, active_range WITH &&);


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
-- Name: tenant_settings tenant_settings_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_settings
    ADD CONSTRAINT tenant_settings_pkey PRIMARY KEY (tenant_id);


--
-- Name: tenant_trainer tenant_trainer_no_overlap; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_trainer
    ADD CONSTRAINT tenant_trainer_no_overlap EXCLUDE USING gist (tenant_id WITH =, person_id WITH =, active_range WITH &&);


--
-- Name: tenant_trainer tenant_trainer_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_trainer
    ADD CONSTRAINT tenant_trainer_pkey PRIMARY KEY (id);


--
-- Name: transaction transaction_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_pkey PRIMARY KEY (id);


--
-- Name: transaction transaction_tenant_id_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_tenant_id_id_key UNIQUE (tenant_id, id);


--
-- Name: CONSTRAINT transaction_tenant_id_id_key ON transaction; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT transaction_tenant_id_id_key ON public.transaction IS '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';


--
-- Name: user_proxy user_proxy_no_overlap; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_proxy
    ADD CONSTRAINT user_proxy_no_overlap EXCLUDE USING gist (user_id WITH =, person_id WITH =, active_range WITH &&);


--
-- Name: user_proxy user_proxy_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_proxy
    ADD CONSTRAINT user_proxy_pkey PRIMARY KEY (id);


--
-- Name: event_people_key; Type: INDEX; Schema: app_private; Owner: -
--

CREATE UNIQUE INDEX event_people_key ON app_private.event_people USING btree (tenant_id, viewer_person_id, visible_person_id);


--
-- Name: idx_23855_pc_symbol; Type: INDEX; Schema: app_private; Owner: -
--

CREATE UNIQUE INDEX idx_23855_pc_symbol ON app_private.platby_category USING btree (pc_symbol);


--
-- Name: idx_23868_pcg_id_group; Type: INDEX; Schema: app_private; Owner: -
--

CREATE UNIQUE INDEX idx_23868_pcg_id_group ON app_private.platby_category_group USING btree (pcg_id_group, pcg_id_category);


--
-- Name: idx_23868_platby_category_group_pcg_id_category_fkey; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX idx_23868_platby_category_group_pcg_id_category_fkey ON app_private.platby_category_group USING btree (pcg_id_category);


--
-- Name: idx_23885_pgs_id_skupina; Type: INDEX; Schema: app_private; Owner: -
--

CREATE UNIQUE INDEX idx_23885_pgs_id_skupina ON app_private.platby_group_skupina USING btree (pgs_id_skupina, pgs_id_group);


--
-- Name: idx_23885_platby_group_skupina_pgs_id_group_fkey; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX idx_23885_platby_group_skupina_pgs_id_group_fkey ON app_private.platby_group_skupina USING btree (pgs_id_group);


--
-- Name: idx_23886_pgs_id_skupina; Type: INDEX; Schema: app_private; Owner: -
--

CREATE UNIQUE INDEX idx_23886_pgs_id_skupina ON app_private.platby_group_skupina USING btree (pgs_id_skupina, pgs_id_group);


--
-- Name: idx_23891_pi_id_raw; Type: INDEX; Schema: app_private; Owner: -
--

CREATE UNIQUE INDEX idx_23891_pi_id_raw ON app_private.platby_item USING btree (pi_id_raw);


--
-- Name: idx_23891_platby_item_pi_id_category_fkey; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX idx_23891_platby_item_pi_id_category_fkey ON app_private.platby_item USING btree (pi_id_category);


--
-- Name: idx_23891_platby_item_pi_id_user_fkey; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX idx_23891_platby_item_pi_id_user_fkey ON app_private.platby_item USING btree (pi_id_user);


--
-- Name: idx_23898_pr_hash; Type: INDEX; Schema: app_private; Owner: -
--

CREATE UNIQUE INDEX idx_23898_pr_hash ON app_private.platby_raw USING btree (pr_hash);


--
-- Name: platby_category_group_pcg_id_category_idx; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX platby_category_group_pcg_id_category_idx ON app_private.platby_category_group USING btree (pcg_id_category);


--
-- Name: platby_category_group_tenant_id_idx; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX platby_category_group_tenant_id_idx ON app_private.platby_category_group USING btree (tenant_id);


--
-- Name: platby_category_tenant_id_idx; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX platby_category_tenant_id_idx ON app_private.platby_category USING btree (tenant_id);


--
-- Name: platby_group_skupina_pgs_id_group_idx; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX platby_group_skupina_pgs_id_group_idx ON app_private.platby_group_skupina USING btree (pgs_id_group);


--
-- Name: platby_group_skupina_tenant_id_idx; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX platby_group_skupina_tenant_id_idx ON app_private.platby_group_skupina USING btree (tenant_id);


--
-- Name: platby_group_tenant_id_idx; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX platby_group_tenant_id_idx ON app_private.platby_group USING btree (tenant_id);


--
-- Name: platby_item_pi_id_category_idx; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX platby_item_pi_id_category_idx ON app_private.platby_item USING btree (pi_id_category);


--
-- Name: platby_item_tenant_id_idx; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX platby_item_tenant_id_idx ON app_private.platby_item USING btree (tenant_id);


--
-- Name: platby_raw_tenant_id_idx; Type: INDEX; Schema: app_private; Owner: -
--

CREATE INDEX platby_raw_tenant_id_idx ON app_private.platby_raw USING btree (tenant_id);


--
-- Name: frontier_failure_idx; Type: INDEX; Schema: crawler; Owner: -
--

CREATE INDEX frontier_failure_idx ON crawler.frontier USING btree (federation, kind, id) WHERE ((fetch_status = ANY (ARRAY['error'::crawler.fetch_status, 'transient'::crawler.fetch_status])) OR (process_status = 'error'::crawler.process_status));


--
-- Name: frontier_federation_kind_idx; Type: INDEX; Schema: crawler; Owner: -
--

CREATE INDEX frontier_federation_kind_idx ON crawler.frontier USING btree (federation, kind);


--
-- Name: frontier_federation_kind_next_fetch_at_idx; Type: INDEX; Schema: crawler; Owner: -
--

CREATE INDEX frontier_federation_kind_next_fetch_at_idx ON crawler.frontier USING btree (federation, kind, next_fetch_at) WHERE ((fetch_status = ANY (ARRAY['pending'::crawler.fetch_status, 'transient'::crawler.fetch_status])) OR ((fetch_status = 'ok'::crawler.fetch_status) AND (process_status = 'ok'::crawler.process_status)));


--
-- Name: frontier_last_response_id_idx; Type: INDEX; Schema: crawler; Owner: -
--

CREATE INDEX frontier_last_response_id_idx ON crawler.frontier USING btree (last_response_id) WHERE (last_response_id IS NOT NULL);


--
-- Name: frontier_last_successful_response_id_idx; Type: INDEX; Schema: crawler; Owner: -
--

CREATE INDEX frontier_last_successful_response_id_idx ON crawler.frontier USING btree (last_successful_response_id) WHERE (last_successful_response_id IS NOT NULL);


--
-- Name: frontier_process_pending_ok_pick_idx; Type: INDEX; Schema: crawler; Owner: -
--

CREATE INDEX frontier_process_pending_ok_pick_idx ON crawler.frontier USING btree (discovered_at, last_fetched_at, id) WHERE ((process_status = 'pending'::crawler.process_status) AND (fetch_status = 'ok'::crawler.fetch_status));


--
-- Name: json_response_content_hash_idx; Type: INDEX; Schema: crawler; Owner: -
--

CREATE INDEX json_response_content_hash_idx ON crawler.json_response USING btree (content_hash);


--
-- Name: json_response_current_frontier_idx; Type: INDEX; Schema: crawler; Owner: -
--

CREATE UNIQUE INDEX json_response_current_frontier_idx ON crawler.json_response USING btree (frontier_id) WHERE upper_inf(sys_period);


--
-- Name: json_response_frontier_fetched_desc_idx; Type: INDEX; Schema: crawler; Owner: -
--

CREATE INDEX json_response_frontier_fetched_desc_idx ON crawler.json_response USING btree (frontier_id, fetched_at DESC, id DESC);


--
-- Name: category_age_group_code_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX category_age_group_code_idx ON federated.category USING btree (age_group_code);


--
-- Name: competition_category_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_category_id_idx ON federated.competition USING btree (category_id);


--
-- Name: competition_entry_competition_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_entry_competition_id_idx ON federated.competition_entry USING btree (competition_id);


--
-- Name: competition_entry_competitor_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_entry_competitor_id_idx ON federated.competition_entry USING btree (competitor_id);


--
-- Name: competition_event_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_event_id_idx ON federated.competition USING btree (event_id);


--
-- Name: competition_official_competition_id_external_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_official_competition_id_external_id_idx ON federated.competition_official USING btree (competition_id, external_id) WHERE (external_id IS NOT NULL);


--
-- Name: competition_official_competition_id_role_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_official_competition_id_role_idx ON federated.competition_official USING btree (competition_id, role);


--
-- Name: competition_official_person_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_official_person_id_idx ON federated.competition_official USING btree (person_id);


--
-- Name: competition_result_competition_id_ranking_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_result_competition_id_ranking_idx ON federated.competition_result USING btree (competition_id, ranking);


--
-- Name: competition_result_competitor_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_result_competitor_id_idx ON federated.competition_result USING btree (competitor_id);


--
-- Name: competition_round_competition_id_round_key_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_round_competition_id_round_key_idx ON federated.competition_round USING btree (competition_id, round_key);


--
-- Name: competition_round_dance_program_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_round_dance_program_id_idx ON federated.competition_round USING btree (dance_program_id);


--
-- Name: competition_round_judge_judge; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_round_judge_judge ON federated.competition_round_judge USING btree (person_judge_id);


--
-- Name: competition_round_result_competitor_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_round_result_competitor_id_idx ON federated.competition_round_result USING btree (competitor_id);


--
-- Name: competition_round_result_round_id_overall_ranking_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_round_result_round_id_overall_ranking_idx ON federated.competition_round_result USING btree (round_id, overall_ranking);


--
-- Name: competition_start_date_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competition_start_date_idx ON federated.competition USING btree (start_date) INCLUDE (id);


--
-- Name: competitor_age_group_code_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competitor_age_group_code_idx ON federated.competitor USING btree (age_group_code);


--
-- Name: competitor_category_progress_competitor_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competitor_category_progress_competitor_id_idx ON federated.competitor_category_progress USING btree (competitor_id);


--
-- Name: competitor_club_affiliation_club_id_valid_from_valid_to_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competitor_club_affiliation_club_id_valid_from_valid_to_idx ON federated.competitor_club_affiliation USING btree (club_id, valid_from, valid_to);


--
-- Name: competitor_component_person_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competitor_component_person_id_idx ON federated.competitor_component USING btree (person_id);


--
-- Name: competitor_external_id_text_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competitor_external_id_text_idx ON federated.competitor USING btree (external_id);


--
-- Name: competitor_search_name_trgm_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX competitor_search_name_trgm_idx ON federated.competitor USING gin (search_name public.gin_trgm_ops);


--
-- Name: dance_program_dance_dance_code_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX dance_program_dance_dance_code_idx ON federated.dance_program_dance USING btree (dance_code);


--
-- Name: dance_program_dance_program_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX dance_program_dance_program_id_idx ON federated.dance_program_dance USING btree (program_id);


--
-- Name: event_federation_location_country_range_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX event_federation_location_country_range_idx ON federated.event USING gist (federation, location, country, range);


--
-- Name: event_federation_start_date_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX event_federation_start_date_idx ON federated.event USING btree (federation, start_date);


--
-- Name: event_official_event_id_role_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX event_official_event_id_role_idx ON federated.event_official USING btree (event_id, role);


--
-- Name: event_official_person_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX event_official_person_id_idx ON federated.event_official USING btree (person_id);


--
-- Name: event_range_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX event_range_idx ON federated.event USING gist (range);


--
-- Name: federated_person_csts_search_name_trgm_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX federated_person_csts_search_name_trgm_idx ON federated.person USING gin (search_name public.gin_trgm_ops) WHERE (federation = 'csts'::text);


--
-- Name: federated_person_wdsf_national_reference_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX federated_person_wdsf_national_reference_idx ON federated.person USING btree (national_reference) WHERE (federation = 'wdsf'::text);


--
-- Name: idx_person_search_name_trgm; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX idx_person_search_name_trgm ON federated.person USING gin (search_name public.gin_trgm_ops);


--
-- Name: person_age_group_code_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX person_age_group_code_idx ON federated.person USING btree (age_group_code);


--
-- Name: person_club_membership_club_id_valid_from_valid_to_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX person_club_membership_club_id_valid_from_valid_to_idx ON federated.person_club_membership USING btree (club_id, valid_from, valid_to);


--
-- Name: person_club_membership_person_id_valid_from_valid_to_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX person_club_membership_person_id_valid_from_valid_to_idx ON federated.person_club_membership USING btree (person_id, valid_from, valid_to);


--
-- Name: person_external_id_text_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX person_external_id_text_idx ON federated.person USING btree (((external_id)::text));


--
-- Name: person_license_federation_person_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX person_license_federation_person_id_idx ON federated.person_license USING btree (federation, person_id);


--
-- Name: ranklist_entry_competitor_id_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX ranklist_entry_competitor_id_idx ON federated.ranklist_entry USING btree (competitor_id);


--
-- Name: ranklist_entry_snapshot_id_ranking_idx; Type: INDEX; Schema: federated; Owner: -
--

CREATE INDEX ranklist_entry_snapshot_id_ranking_idx ON federated.ranklist_entry USING btree (snapshot_id, ranking);


--
-- Name: access_credential_person_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX access_credential_person_idx ON public.access_credential USING btree (tenant_id, person_id);


--
-- Name: access_event_credential_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX access_event_credential_idx ON public.access_event USING btree (tenant_id, kind, code, occurred_at DESC);


--
-- Name: access_event_location_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX access_event_location_idx ON public.access_event USING btree (tenant_id, location_id, occurred_at DESC);


--
-- Name: access_event_person_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX access_event_person_idx ON public.access_event USING btree (tenant_id, person_id, occurred_at DESC);


--
-- Name: account_balances_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX account_balances_id_idx ON public.account_balances USING btree (id);


--
-- Name: account_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX account_person_id_idx ON public.account USING btree (person_id);


--
-- Name: accounting_period_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX accounting_period_tenant_id_idx ON public.accounting_period USING btree (tenant_id);


--
-- Name: aktuality_tenant_id_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX aktuality_tenant_id_id_idx ON public.aktuality USING btree (tenant_id, id);


--
-- Name: announcement_attachment_file_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX announcement_attachment_file_idx ON public.announcement_attachment USING btree (tenant_id, file_id);


--
-- Name: announcement_audience_announcement_cohort_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX announcement_audience_announcement_cohort_idx ON public.announcement_audience USING btree (announcement_id, cohort_id);


--
-- Name: announcement_audience_announcement_role_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX announcement_audience_announcement_role_idx ON public.announcement_audience USING btree (announcement_id, audience_role);


--
-- Name: announcement_audience_cohort_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX announcement_audience_cohort_id_idx ON public.announcement_audience USING btree (cohort_id);


--
-- Name: announcement_audience_tenant_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX announcement_audience_tenant_idx ON public.announcement_audience USING btree (tenant_id);


--
-- Name: announcement_author_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX announcement_author_id_idx ON public.announcement USING btree (author_id);


--
-- Name: announcement_created_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX announcement_created_at_idx ON public.announcement USING btree (created_at);


--
-- Name: announcement_status_created_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX announcement_status_created_at_idx ON public.announcement USING btree (tenant_id, status, is_sticky, created_at DESC);


--
-- Name: announcement_tenant_id_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX announcement_tenant_id_id_idx ON public.announcement USING btree (tenant_id, id);


--
-- Name: announcement_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX announcement_tenant_id_idx ON public.announcement USING btree (tenant_id);


--
-- Name: article_attachment_file_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX article_attachment_file_idx ON public.article_attachment USING btree (tenant_id, file_id);


--
-- Name: cohort_cohort_group_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_cohort_group_id_idx ON public.cohort USING btree (cohort_group_id);


--
-- Name: cohort_group_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_group_tenant_id_idx ON public.cohort_group USING btree (tenant_id);


--
-- Name: cohort_image_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_image_idx ON public.cohort USING btree (tenant_id, image_id) WHERE (image_id IS NOT NULL);


--
-- Name: cohort_membership_active_by_person; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_membership_active_by_person ON public.cohort_membership USING btree (person_id) INCLUDE (cohort_id) WHERE (status = 'active'::public.relationship_status);


--
-- Name: cohort_membership_person_id_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_membership_person_id_id_idx ON public.cohort_membership USING btree (person_id, id) INCLUDE (cohort_id);


--
-- Name: cohort_membership_tenant_status_person_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_membership_tenant_status_person_idx ON public.cohort_membership USING btree (tenant_id, status, person_id, cohort_id);


--
-- Name: cohort_subscription_account_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_subscription_account_id_idx ON public.cohort_subscription USING btree (account_id);


--
-- Name: cohort_subscription_cohort_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_subscription_cohort_id_idx ON public.cohort_subscription USING btree (cohort_id);


--
-- Name: cohort_subscription_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_subscription_tenant_id_idx ON public.cohort_subscription USING btree (tenant_id);


--
-- Name: cohort_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_tenant_id_idx ON public.cohort USING btree (tenant_id);


--
-- Name: couple_image_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX couple_image_idx ON public.couple USING btree (image_id) WHERE (image_id IS NOT NULL);


--
-- Name: couple_man_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX couple_man_active_idx ON public.couple USING btree (man_id) INCLUDE (id) WHERE (status = 'active'::public.relationship_status);


--
-- Name: couple_man_active_lookup; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX couple_man_active_lookup ON public.couple USING btree (man_id, since, until, id) INCLUDE (woman_id, status) WHERE (status = 'active'::public.relationship_status);


--
-- Name: couple_man_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX couple_man_id_idx ON public.couple USING btree (man_id);


--
-- Name: couple_woman_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX couple_woman_active_idx ON public.couple USING btree (woman_id) INCLUDE (id) WHERE (status = 'active'::public.relationship_status);


--
-- Name: couple_woman_active_lookup; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX couple_woman_active_lookup ON public.couple USING btree (woman_id, since, until, id) INCLUDE (man_id, status) WHERE (status = 'active'::public.relationship_status);


--
-- Name: couple_woman_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX couple_woman_id_idx ON public.couple USING btree (woman_id);


--
-- Name: dokumenty_d_kdo_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX dokumenty_d_kdo_idx ON public.dokumenty USING btree (d_kdo);


--
-- Name: dokumenty_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX dokumenty_tenant_id_idx ON public.dokumenty USING btree (tenant_id);


--
-- Name: event_cover_image_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_cover_image_idx ON public.event USING btree (tenant_id, cover_image_id) WHERE (cover_image_id IS NOT NULL);


--
-- Name: event_lesson_request_registration_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_lesson_request_registration_id_idx ON public.event_lesson_request USING btree (registration_id);


--
-- Name: event_lesson_request_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_lesson_request_tenant_id_idx ON public.event_lesson_request USING btree (tenant_id);


--
-- Name: event_lesson_request_trainer_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_lesson_request_trainer_id_idx ON public.event_lesson_request USING btree (trainer_id);


--
-- Name: event_parent_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_parent_id_idx ON public.event USING btree (parent_id);


--
-- Name: event_range_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_range_idx ON public.event USING gist (range);


--
-- Name: event_registration_active_person_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_active_person_idx ON public.event_registration USING btree (tenant_id, person_id) INCLUDE (event_id, status) WHERE (registration_status = 'active'::public.event_registration_status);


--
-- Name: event_registration_couple_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_couple_id_idx ON public.event_registration USING btree (couple_id);


--
-- Name: event_registration_event_id_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_event_id_id_idx ON public.event_registration USING btree (event_id, id);


--
-- Name: event_registration_parent_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_parent_id_idx ON public.event_registration USING btree (parent_registration_id);


--
-- Name: event_registration_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_person_id_idx ON public.event_registration USING btree (person_id);


--
-- Name: event_registration_person_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX event_registration_person_key ON public.event_registration USING btree (event_id, person_id) WHERE ((person_id IS NOT NULL) AND (registration_status = 'active'::public.event_registration_status));


--
-- Name: event_registration_target_cohort_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_target_cohort_id_idx ON public.event_registration USING btree (target_cohort_id);


--
-- Name: event_registration_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_tenant_id_idx ON public.event_registration USING btree (tenant_id);


--
-- Name: event_registration_unit_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX event_registration_unit_key ON public.event_registration USING btree (event_id, couple_id, person_id) NULLS NOT DISTINCT WHERE (parent_registration_id IS NULL);


--
-- Name: event_share_token_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX event_share_token_key ON public.event USING btree (share_token) WHERE (share_token IS NOT NULL);


--
-- Name: event_since_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_since_idx ON public.event USING btree (since);


--
-- Name: event_target_cohort_cohort_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_target_cohort_cohort_id_idx ON public.event_target_cohort USING btree (cohort_id);


--
-- Name: event_target_cohort_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_target_cohort_tenant_id_idx ON public.event_target_cohort USING btree (tenant_id);


--
-- Name: event_tenant_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_tenant_idx ON public.event USING btree (tenant_id);


--
-- Name: event_tenant_range_gist; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_tenant_range_gist ON public.event USING gist (tenant_id, range);


--
-- Name: event_tenant_series_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_tenant_series_idx ON public.event USING btree (tenant_id, series_id);


--
-- Name: event_tenant_since_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_tenant_since_idx ON public.event USING btree (tenant_id, since);


--
-- Name: event_tenant_until_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_tenant_until_idx ON public.event USING btree (tenant_id, until);


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
-- Name: event_trainer_tenant_person_event_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_trainer_tenant_person_event_idx ON public.event_trainer USING btree (tenant_id, person_id, event_id);


--
-- Name: file_public_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX file_public_idx ON public.file USING btree (tenant_id, id) WHERE is_public;


--
-- Name: form_responses_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX form_responses_tenant_id_idx ON public.form_responses USING btree (tenant_id);


--
-- Name: idx_23753_aktuality_at_kdo_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23753_aktuality_at_kdo_fkey ON public.aktuality USING btree (at_kdo);


--
-- Name: idx_23753_at_timestamp_add; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23753_at_timestamp_add ON public.aktuality USING btree (created_at);


--
-- Name: idx_23771_d_path; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_23771_d_path ON public.dokumenty USING btree (d_path);


--
-- Name: idx_23771_dokumenty_d_kdo_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23771_dokumenty_d_kdo_fkey ON public.dokumenty USING btree (d_kdo);


--
-- Name: membership_application_created_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX membership_application_created_by_idx ON public.membership_application USING btree (created_by);


--
-- Name: membership_application_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX membership_application_tenant_id_idx ON public.membership_application USING btree (tenant_id);


--
-- Name: otp_token_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX otp_token_tenant_id_idx ON public.otp_token USING btree (tenant_id);


--
-- Name: otp_token_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX otp_token_user_id_idx ON public.otp_token USING btree (user_id);


--
-- Name: payment_accounting_period_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX payment_accounting_period_id_idx ON public.payment USING btree (accounting_period_id);


--
-- Name: payment_cohort_subscription_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX payment_cohort_subscription_id_idx ON public.payment USING btree (cohort_subscription_id);


--
-- Name: payment_debtor_payment_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX payment_debtor_payment_id_idx ON public.payment_debtor USING btree (payment_id);


--
-- Name: payment_debtor_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX payment_debtor_person_id_idx ON public.payment_debtor USING btree (person_id);


--
-- Name: payment_debtor_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX payment_debtor_tenant_id_idx ON public.payment_debtor USING btree (tenant_id);


--
-- Name: payment_event_instance_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX payment_event_instance_id_idx ON public.payment USING btree (event_id) WHERE (event_id IS NOT NULL);


--
-- Name: payment_recipient_account_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX payment_recipient_account_id_idx ON public.payment_recipient USING btree (account_id);


--
-- Name: payment_recipient_payment_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX payment_recipient_payment_id_idx ON public.payment_recipient USING btree (payment_id);


--
-- Name: payment_recipient_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX payment_recipient_tenant_id_idx ON public.payment_recipient USING btree (tenant_id);


--
-- Name: payment_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX payment_tenant_id_idx ON public.payment USING btree (tenant_id);


--
-- Name: person_csts_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX person_csts_id_idx ON public.person USING btree (csts_id) WHERE (csts_id IS NOT NULL);


--
-- Name: person_image_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX person_image_idx ON public.person USING btree (image_id) WHERE (image_id IS NOT NULL);


--
-- Name: person_invitation_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX person_invitation_person_id_idx ON public.person_invitation USING btree (person_id);


--
-- Name: person_invitation_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX person_invitation_tenant_id_idx ON public.person_invitation USING btree (tenant_id);


--
-- Name: posting_account_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX posting_account_id_idx ON public.posting USING btree (account_id);


--
-- Name: posting_original_account_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX posting_original_account_id_idx ON public.posting USING btree (original_account_id);


--
-- Name: posting_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX posting_tenant_id_idx ON public.posting USING btree (tenant_id);


--
-- Name: posting_transaction_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX posting_transaction_id_idx ON public.posting USING btree (transaction_id);


--
-- Name: scoreboard_manual_adjustment_cohort_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX scoreboard_manual_adjustment_cohort_id_idx ON public.scoreboard_manual_adjustment USING btree (cohort_id);


--
-- Name: scoreboard_manual_adjustment_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX scoreboard_manual_adjustment_person_id_idx ON public.scoreboard_manual_adjustment USING btree (person_id);


--
-- Name: scoreboard_manual_adjustment_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX scoreboard_manual_adjustment_tenant_id_idx ON public.scoreboard_manual_adjustment USING btree (tenant_id);


--
-- Name: security_event_actor_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX security_event_actor_user_id_idx ON public.security_event USING btree (actor_user_id);


--
-- Name: security_event_person_occurred_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX security_event_person_occurred_at_idx ON public.security_event USING btree (person_id, occurred_at DESC);


--
-- Name: security_event_tenant_occurred_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX security_event_tenant_occurred_at_idx ON public.security_event USING btree (tenant_id, occurred_at DESC);


--
-- Name: security_event_user_occurred_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX security_event_user_occurred_at_idx ON public.security_event USING btree (user_id, occurred_at DESC);


--
-- Name: tenant_administrator_active_by_person; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_administrator_active_by_person ON public.tenant_administrator USING btree (person_id) INCLUDE (tenant_id) WHERE (status = 'active'::public.relationship_status);


--
-- Name: tenant_administrator_active_by_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_administrator_active_by_tenant ON public.tenant_administrator USING btree (tenant_id) INCLUDE (person_id) WHERE (status = 'active'::public.relationship_status);


--
-- Name: tenant_administrator_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_administrator_person_id_idx ON public.tenant_administrator USING btree (person_id);


--
-- Name: tenant_administrator_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_administrator_tenant_id_idx ON public.tenant_administrator USING btree (tenant_id);


--
-- Name: tenant_administrator_tenant_person_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_administrator_tenant_person_active_idx ON public.tenant_administrator USING btree (tenant_id, person_id) WHERE (status = 'active'::public.relationship_status);


--
-- Name: tenant_administrator_tenant_status_person_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_administrator_tenant_status_person_idx ON public.tenant_administrator USING btree (tenant_id, status, person_id);


--
-- Name: tenant_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_id ON public.aktuality USING btree (tenant_id);


--
-- Name: tenant_location_cover_image_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_location_cover_image_idx ON public.location USING btree (tenant_id, cover_image_id) WHERE (cover_image_id IS NOT NULL);


--
-- Name: tenant_location_image_file_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_location_image_file_idx ON public.location_image USING btree (tenant_id, file_id);


--
-- Name: tenant_location_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_location_tenant_id_idx ON public.location USING btree (tenant_id);


--
-- Name: tenant_membership_active_by_person; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_membership_active_by_person ON public.tenant_membership USING btree (person_id) INCLUDE (tenant_id) WHERE (status = 'active'::public.relationship_status);


--
-- Name: tenant_membership_active_by_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_membership_active_by_tenant ON public.tenant_membership USING btree (tenant_id) INCLUDE (person_id) WHERE (status = 'active'::public.relationship_status);


--
-- Name: tenant_membership_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_membership_person_id_idx ON public.tenant_membership USING btree (person_id);


--
-- Name: tenant_membership_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_membership_tenant_id_idx ON public.tenant_membership USING btree (tenant_id);


--
-- Name: tenant_membership_tenant_person_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_membership_tenant_person_active_idx ON public.tenant_membership USING btree (tenant_id, person_id) WHERE (status = 'active'::public.relationship_status);


--
-- Name: tenant_membership_tenant_status_person_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_membership_tenant_status_person_idx ON public.tenant_membership USING btree (tenant_id, status, person_id);


--
-- Name: tenant_trainer_active_by_person; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_trainer_active_by_person ON public.tenant_trainer USING btree (person_id) INCLUDE (tenant_id) WHERE (status = 'active'::public.relationship_status);


--
-- Name: tenant_trainer_active_by_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_trainer_active_by_tenant ON public.tenant_trainer USING btree (tenant_id) INCLUDE (person_id) WHERE (status = 'active'::public.relationship_status);


--
-- Name: tenant_trainer_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_trainer_person_id_idx ON public.tenant_trainer USING btree (person_id);


--
-- Name: tenant_trainer_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_trainer_tenant_id_idx ON public.tenant_trainer USING btree (tenant_id);


--
-- Name: tenant_trainer_tenant_person_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_trainer_tenant_person_active_idx ON public.tenant_trainer USING btree (tenant_id, person_id) WHERE (status = 'active'::public.relationship_status);


--
-- Name: tenant_trainer_tenant_status_person_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_trainer_tenant_status_person_idx ON public.tenant_trainer USING btree (tenant_id, status, person_id);


--
-- Name: transaction_accounting_period_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX transaction_accounting_period_id_idx ON public.transaction USING btree (accounting_period_id);


--
-- Name: transaction_payment_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX transaction_payment_id_idx ON public.transaction USING btree (payment_id);


--
-- Name: transaction_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX transaction_tenant_id_idx ON public.transaction USING btree (tenant_id);


--
-- Name: user_proxy_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_proxy_person_id_idx ON public.user_proxy USING btree (person_id);


--
-- Name: user_proxy_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_proxy_user_id_idx ON public.user_proxy USING btree (user_id);


--
-- Name: users_email_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX users_email_key ON public.users USING btree (u_email) WHERE (id <> ALL (ARRAY[(916)::bigint, (914)::bigint, (915)::bigint, (587)::bigint, (765)::bigint, (696)::bigint, (786)::bigint, (259)::bigint, (306)::bigint, (540)::bigint, (443)::bigint, (1042)::bigint, (585)::bigint, (825)::bigint, (413)::bigint, (428)::bigint, (985)::bigint, (935)::bigint, (218)::bigint, (581)::bigint, (827)::bigint, (826)::bigint, (165)::bigint, (207)::bigint, (232)::bigint, (945)::bigint, (990)::bigint, (1039)::bigint, (1040)::bigint, (606)::bigint, (607)::bigint, (896)::bigint, (975)::bigint, (496)::bigint, (511)::bigint, (898)::bigint, (920)::bigint, (970)::bigint, (724)::bigint, (725)::bigint, (958)::bigint, (542)::bigint, (543)::bigint, (886)::bigint, (223)::bigint, (348)::bigint, (23)::bigint, (973)::bigint, (128)::bigint, (988)::bigint, (517)::bigint, (978)::bigint, (928)::bigint, (930)::bigint, (968)::bigint, (939)::bigint, (951)::bigint, (950)::bigint, (808)::bigint, (723)::bigint, (557)::bigint, (1013)::bigint, (1014)::bigint, (1015)::bigint, (820)::bigint, (841)::bigint, (599)::bigint, (681)::bigint, (31)::bigint, (40)::bigint, (120)::bigint, (360)::bigint, (417)::bigint, (419)::bigint, (545)::bigint, (39)::bigint, (643)::bigint, (670)::bigint, (782)::bigint, (790)::bigint, (668)::bigint, (894)::bigint, (922)::bigint, (925)::bigint, (803)::bigint, (812)::bigint, (153)::bigint, (602)::bigint, (198)::bigint, (239)::bigint, (397)::bigint, (686)::bigint, (846)::bigint, (537)::bigint, (893)::bigint, (974)::bigint, (993)::bigint, (755)::bigint, (805)::bigint, (337)::bigint, (155)::bigint, (629)::bigint, (630)::bigint, (554)::bigint, (994)::bigint, (661)::bigint, (891)::bigint, (434)::bigint, (436)::bigint, (640)::bigint, (829)::bigint, (683)::bigint, (505)::bigint, (1)::bigint, (648)::bigint, (649)::bigint, (677)::bigint, (4)::bigint, (162)::bigint, (17)::bigint, (565)::bigint, (700)::bigint, (701)::bigint, (952)::bigint, (999)::bigint, (1003)::bigint, (1006)::bigint, (346)::bigint, (576)::bigint, (986)::bigint, (582)::bigint, (315)::bigint, (753)::bigint, (76)::bigint, (93)::bigint, (316)::bigint, (359)::bigint, (370)::bigint, (508)::bigint, (506)::bigint, (509)::bigint, (510)::bigint, (754)::bigint, (811)::bigint, (430)::bigint, (654)::bigint, (598)::bigint, (612)::bigint, (698)::bigint, (923)::bigint, (943)::bigint, (971)::bigint, (679)::bigint, (798)::bigint, (799)::bigint]));


--
-- Name: users_login_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX users_login_key ON public.users USING btree (u_login) WHERE (id <> ALL (ARRAY[(1050)::bigint, (533)::bigint, (882)::bigint, (1075)::bigint, (489)::bigint, (82)::bigint, (1138)::bigint, (689)::bigint, (45)::bigint, (433)::bigint, (13)::bigint, (142)::bigint, (223)::bigint, (1046)::bigint, (498)::bigint, (1106)::bigint, (105)::bigint, (130)::bigint]));


--
-- Name: users_tenant_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX users_tenant_id_idx ON public.users USING btree (tenant_id);


--
-- Name: competition _900_refresh_competitor_activity_dates; Type: TRIGGER; Schema: federated; Owner: -
--

CREATE TRIGGER _900_refresh_competitor_activity_dates AFTER UPDATE ON federated.competition REFERENCING OLD TABLE AS oldtable NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competition_dates__refresh_competitor_activity();


--
-- Name: competition_entry _900_refresh_competitor_activity_delete; Type: TRIGGER; Schema: federated; Owner: -
--

CREATE TRIGGER _900_refresh_competitor_activity_delete AFTER DELETE ON federated.competition_entry REFERENCING OLD TABLE AS oldtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competitor_activity__refresh();


--
-- Name: competition_result _900_refresh_competitor_activity_delete; Type: TRIGGER; Schema: federated; Owner: -
--

CREATE TRIGGER _900_refresh_competitor_activity_delete AFTER DELETE ON federated.competition_result REFERENCING OLD TABLE AS oldtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competitor_activity__refresh();


--
-- Name: competition_entry _900_refresh_competitor_activity_insert; Type: TRIGGER; Schema: federated; Owner: -
--

CREATE TRIGGER _900_refresh_competitor_activity_insert AFTER INSERT ON federated.competition_entry REFERENCING NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competitor_activity__refresh();


--
-- Name: competition_result _900_refresh_competitor_activity_insert; Type: TRIGGER; Schema: federated; Owner: -
--

CREATE TRIGGER _900_refresh_competitor_activity_insert AFTER INSERT ON federated.competition_result REFERENCING NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competitor_activity__refresh();


--
-- Name: competition_entry _900_refresh_competitor_activity_update; Type: TRIGGER; Schema: federated; Owner: -
--

CREATE TRIGGER _900_refresh_competitor_activity_update AFTER UPDATE ON federated.competition_entry REFERENCING OLD TABLE AS oldtable NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competitor_activity__refresh();


--
-- Name: competition_result _900_refresh_competitor_activity_update; Type: TRIGGER; Schema: federated; Owner: -
--

CREATE TRIGGER _900_refresh_competitor_activity_update AFTER UPDATE ON federated.competition_result REFERENCING OLD TABLE AS oldtable NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competitor_activity__refresh();


--
-- Name: category _resolve_age_group; Type: TRIGGER; Schema: federated; Owner: -
--

CREATE TRIGGER _resolve_age_group BEFORE INSERT OR UPDATE OF age_group ON federated.category FOR EACH ROW EXECUTE FUNCTION app_private.tg_resolve_age_group();


--
-- Name: competitor _resolve_age_group; Type: TRIGGER; Schema: federated; Owner: -
--

CREATE TRIGGER _resolve_age_group BEFORE INSERT OR UPDATE OF age_group ON federated.competitor FOR EACH ROW EXECUTE FUNCTION app_private.tg_resolve_age_group();


--
-- Name: person _resolve_age_group; Type: TRIGGER; Schema: federated; Owner: -
--

CREATE TRIGGER _resolve_age_group BEFORE INSERT OR UPDATE OF age_group ON federated.person FOR EACH ROW EXECUTE FUNCTION app_private.tg_resolve_age_group();


--
-- Name: access_credential _050_relationship_status; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _050_relationship_status BEFORE INSERT OR UPDATE OF since, until ON public.access_credential FOR EACH ROW EXECUTE FUNCTION app_private.tg_relationship__status();


--
-- Name: tenant_administrator _050_relationship_status; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _050_relationship_status BEFORE INSERT OR UPDATE OF since, until ON public.tenant_administrator FOR EACH ROW EXECUTE FUNCTION app_private.tg_relationship__status();


--
-- Name: tenant_membership _050_relationship_status; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _050_relationship_status BEFORE INSERT OR UPDATE OF since, until ON public.tenant_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg_relationship__status();


--
-- Name: tenant_trainer _050_relationship_status; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _050_relationship_status BEFORE INSERT OR UPDATE OF since, until ON public.tenant_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg_relationship__status();


--
-- Name: user_proxy _050_relationship_status; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _050_relationship_status BEFORE INSERT OR UPDATE OF since, until ON public.user_proxy FOR EACH ROW EXECUTE FUNCTION app_private.tg_relationship__status();


--
-- Name: access_credential _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.access_credential FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: account _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.account FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: accounting_period _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.accounting_period FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: aktuality _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.aktuality FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: announcement _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.announcement FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: cohort_membership _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.cohort_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: cohort_subscription _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.cohort_subscription FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: couple _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.couple FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: dokumenty _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.dokumenty FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: event _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps('manager_person_ids', 'stats', 'series_info');


--
-- Name: event_lesson_request _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_lesson_request FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: event_registration _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps('attendance_created_at', 'attendance_updated_at');


--
-- Name: event_series _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_series FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


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
-- Name: location _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.location FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: membership_application _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.membership_application FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: otp_token _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.otp_token FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: payment _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.payment FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: person _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: person_invitation _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person_invitation FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: posting _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.posting FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: scoreboard_manual_adjustment _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.scoreboard_manual_adjustment FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


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
-- Name: transaction _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.transaction FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: user_proxy _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.user_proxy FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: users _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps('last_login', 'last_active_at', 'last_version');


--
-- Name: event_registration _200_attendance_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_attendance_timestamps BEFORE INSERT OR UPDATE OF person_id, status, attendance_note ON public.event_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_registration__attendance_timestamps();


--
-- Name: aktuality _200_author; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_author BEFORE INSERT OR UPDATE ON public.aktuality FOR EACH ROW EXECUTE FUNCTION app_private.tg_aktuality__author();


--
-- Name: announcement _200_author; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_author BEFORE INSERT OR UPDATE ON public.announcement FOR EACH ROW EXECUTE FUNCTION app_private.tg_announcement__author();


--
-- Name: users _200_encrypt_password; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_encrypt_password BEFORE INSERT OR UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg_users__encrypt_password();


--
-- Name: payment _200_fill_accounting_period; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_fill_accounting_period BEFORE INSERT ON public.payment FOR EACH ROW EXECUTE FUNCTION app_private.tg_payment__fill_accounting_period();


--
-- Name: transaction _200_fill_accounting_period; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_fill_accounting_period BEFORE INSERT ON public.transaction FOR EACH ROW EXECUTE FUNCTION app_private.tg_payment__fill_accounting_period();


--
-- Name: event _200_validate_parent; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_validate_parent BEFORE INSERT OR UPDATE OF tenant_id, parent_id ON public.event FOR EACH ROW EXECUTE FUNCTION app_private.tg_event__validate_parent();


--
-- Name: transaction _300_effective_date; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _300_effective_date BEFORE INSERT OR UPDATE ON public.transaction FOR EACH ROW EXECUTE FUNCTION app_private.tg_transaction__effective_date();


--
-- Name: announcement _300_status; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _300_status BEFORE INSERT OR UPDATE OF status, scheduled_since, scheduled_until ON public.announcement FOR EACH ROW EXECUTE FUNCTION app_private.tg_announcement__status();


--
-- Name: users _300_trim_login; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _300_trim_login BEFORE INSERT OR UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg_users__trim_login();


--
-- Name: event _500_delete_on_cancellation; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_delete_on_cancellation AFTER UPDATE OF is_cancelled ON public.event FOR EACH ROW WHEN ((old.is_cancelled IS DISTINCT FROM new.is_cancelled)) EXECUTE FUNCTION app_private.tg_event__delete_payment_on_cancellation();


--
-- Name: cohort_membership _500_on_status; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_on_status AFTER INSERT OR DELETE OR UPDATE OF status, cohort_id, person_id, since, until ON public.cohort_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg_cohort_membership__on_status();


--
-- Name: tenant_membership _500_on_status; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_on_status AFTER UPDATE ON public.tenant_membership FOR EACH ROW WHEN ((old.status IS DISTINCT FROM new.status)) EXECUTE FUNCTION app_private.tg_tenant_membership__on_status();


--
-- Name: event_trainer _500_refresh_manager_person_ids; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_refresh_manager_person_ids AFTER INSERT OR DELETE OR UPDATE OF event_id, person_id ON public.event_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_trainer__refresh_manager_person_ids();


--
-- Name: event _500_refresh_manager_person_ids_from_parent; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_refresh_manager_person_ids_from_parent AFTER INSERT OR UPDATE OF parent_id ON public.event FOR EACH ROW EXECUTE FUNCTION app_private.tg_event__refresh_manager_person_ids();


--
-- Name: event_registration _500_refresh_stats_del; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_refresh_stats_del AFTER DELETE ON public.event_registration REFERENCING OLD TABLE AS deleted_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event_registration__refresh_stats_stmt();


--
-- Name: event_registration _500_refresh_stats_ins; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_refresh_stats_ins AFTER INSERT ON public.event_registration REFERENCING NEW TABLE AS changed_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event_registration__refresh_stats_stmt();


--
-- Name: event_registration _500_refresh_stats_upd; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_refresh_stats_upd AFTER UPDATE ON public.event_registration REFERENCING OLD TABLE AS deleted_rows NEW TABLE AS changed_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event_registration__refresh_stats_stmt();


--
-- Name: person_invitation _500_send; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_send AFTER INSERT ON public.person_invitation FOR EACH ROW EXECUTE FUNCTION app_private.tg_person_invitation__send();


--
-- Name: aktuality _500_sync_attachments_insert; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_sync_attachments_insert AFTER INSERT ON public.aktuality FOR EACH ROW EXECUTE FUNCTION app_private.tg_aktuality__sync_attachments();


--
-- Name: announcement _500_sync_attachments_insert; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_sync_attachments_insert AFTER INSERT ON public.announcement FOR EACH ROW EXECUTE FUNCTION app_private.tg_announcement__sync_attachments();


--
-- Name: aktuality _500_sync_attachments_update; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_sync_attachments_update BEFORE UPDATE OF at_preview, at_text, title_photo_url ON public.aktuality FOR EACH ROW EXECUTE FUNCTION app_private.tg_aktuality__sync_attachments();


--
-- Name: announcement _500_sync_attachments_update; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_sync_attachments_update BEFORE UPDATE OF body ON public.announcement FOR EACH ROW EXECUTE FUNCTION app_private.tg_announcement__sync_attachments();


--
-- Name: announcement_audience _600_notify_announcement_audience_delete; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _600_notify_announcement_audience_delete AFTER DELETE ON public.announcement_audience REFERENCING OLD TABLE AS oldtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_announcement_audience__after_write();


--
-- Name: announcement_audience _600_notify_announcement_audience_insert; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _600_notify_announcement_audience_insert AFTER INSERT ON public.announcement_audience REFERENCING NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_announcement_audience__after_write();


--
-- Name: announcement_audience _600_notify_announcement_audience_update; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _600_notify_announcement_audience_update AFTER UPDATE ON public.announcement_audience REFERENCING OLD TABLE AS oldtable NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_announcement_audience__after_write();


--
-- Name: announcement _600_notify_announcement_insert; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _600_notify_announcement_insert AFTER INSERT ON public.announcement REFERENCING NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_announcement__after_write();


--
-- Name: announcement _600_notify_announcement_update; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _600_notify_announcement_update AFTER UPDATE ON public.announcement REFERENCING OLD TABLE AS oldtable NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_announcement__after_write();


--
-- Name: event _600_refresh_series_info_del; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _600_refresh_series_info_del AFTER DELETE ON public.event REFERENCING OLD TABLE AS deleted_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event__refresh_series_info();


--
-- Name: event _600_refresh_series_info_ins; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _600_refresh_series_info_ins AFTER INSERT ON public.event REFERENCING NEW TABLE AS changed_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event__refresh_series_info();


--
-- Name: event_series _600_refresh_series_info_name; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _600_refresh_series_info_name AFTER UPDATE ON public.event_series REFERENCING OLD TABLE AS deleted_rows NEW TABLE AS changed_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event_series__refresh_info();


--
-- Name: event _600_refresh_series_info_upd; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _600_refresh_series_info_upd AFTER UPDATE ON public.event REFERENCING OLD TABLE AS deleted_rows NEW TABLE AS changed_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event__refresh_series_info();


--
-- Name: event_registration _800_sync_lesson_payment; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER _800_sync_lesson_payment AFTER INSERT OR DELETE OR UPDATE OF event_id, person_id, registration_status ON public.event_registration DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_registration__sync_lesson_payment();


--
-- Name: file _900_delete_object; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_delete_object AFTER DELETE ON public.file FOR EACH ROW EXECUTE FUNCTION app_private.tg_file__delete();


--
-- Name: account _900_fix_balance_accounts; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_fix_balance_accounts AFTER INSERT OR DELETE OR UPDATE OF opening_balance OR TRUNCATE ON public.account FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_account_balances__update();


--
-- Name: posting _900_fix_balance_entries; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_fix_balance_entries AFTER INSERT OR DELETE OR UPDATE OF amount OR TRUNCATE ON public.posting FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_account_balances__update();


--
-- Name: event _900_refresh_event_people; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_refresh_event_people AFTER INSERT OR DELETE OR UPDATE OF tenant_id, parent_id ON public.event FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event_people__refresh();


--
-- Name: event_registration _900_refresh_event_people; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_refresh_event_people AFTER INSERT OR DELETE OR UPDATE OF tenant_id, event_id, person_id, registration_status ON public.event_registration FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event_people__refresh();


--
-- Name: event_trainer _900_refresh_event_people; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_refresh_event_people AFTER INSERT OR DELETE OR UPDATE OF tenant_id, event_id, person_id ON public.event_trainer FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event_people__refresh();


--
-- Name: access_credential _900_security_event; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_security_event AFTER INSERT OR DELETE OR UPDATE OF status ON public.access_credential FOR EACH ROW EXECUTE FUNCTION app_private.tg_security_event__credential();


--
-- Name: tenant_administrator _900_security_event; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_security_event AFTER INSERT OR DELETE OR UPDATE OF status ON public.tenant_administrator FOR EACH ROW EXECUTE FUNCTION app_private.tg_security_event__range('administrator_granted', 'administrator_revoked');


--
-- Name: tenant_membership _900_security_event; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_security_event AFTER INSERT OR DELETE OR UPDATE OF status ON public.tenant_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg_security_event__range('membership_granted', 'membership_revoked');


--
-- Name: tenant_trainer _900_security_event; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_security_event AFTER INSERT OR DELETE OR UPDATE OF status ON public.tenant_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg_security_event__range('trainer_granted', 'trainer_revoked');


--
-- Name: user_proxy _900_security_event; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_security_event AFTER INSERT OR DELETE OR UPDATE OF status ON public.user_proxy FOR EACH ROW EXECUTE FUNCTION app_private.tg_security_event__user_proxy();


--
-- Name: users _900_security_event; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_security_event AFTER UPDATE OF u_pass ON public.users FOR EACH ROW WHEN ((old.u_pass IS DISTINCT FROM new.u_pass)) EXECUTE FUNCTION app_private.tg_security_event__password();


--
-- Name: platby_category_group platby_category_group_pcg_id_category_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_category_group
    ADD CONSTRAINT platby_category_group_pcg_id_category_fkey FOREIGN KEY (pcg_id_category) REFERENCES app_private.platby_category(pc_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_category_group platby_category_group_pcg_id_group_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_category_group
    ADD CONSTRAINT platby_category_group_pcg_id_group_fkey FOREIGN KEY (pcg_id_group) REFERENCES app_private.platby_group(pg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_category_group platby_category_group_tenant_id_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_category_group
    ADD CONSTRAINT platby_category_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: platby_category platby_category_tenant_id_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_category
    ADD CONSTRAINT platby_category_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: platby_group_skupina platby_group_skupina_pgs_id_group_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_pgs_id_group_fkey FOREIGN KEY (pgs_id_group) REFERENCES app_private.platby_group(pg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_group_skupina platby_group_skupina_pgs_id_skupina_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_pgs_id_skupina_fkey FOREIGN KEY (pgs_id_skupina) REFERENCES public.cohort(id);


--
-- Name: platby_group_skupina platby_group_skupina_tenant_id_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: platby_group platby_group_tenant_id_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_group
    ADD CONSTRAINT platby_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: platby_item platby_item_pi_id_category_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_item
    ADD CONSTRAINT platby_item_pi_id_category_fkey FOREIGN KEY (pi_id_category) REFERENCES app_private.platby_category(pc_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_item platby_item_pi_id_raw_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_item
    ADD CONSTRAINT platby_item_pi_id_raw_fkey FOREIGN KEY (pi_id_raw) REFERENCES app_private.platby_raw(pr_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_item platby_item_pi_id_user_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_item
    ADD CONSTRAINT platby_item_pi_id_user_fkey FOREIGN KEY (pi_id_user) REFERENCES public.users(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_item platby_item_tenant_id_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_item
    ADD CONSTRAINT platby_item_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: platby_raw platby_raw_tenant_id_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.platby_raw
    ADD CONSTRAINT platby_raw_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: system_admin_user system_admin_user_user_id_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.system_admin_user
    ADD CONSTRAINT system_admin_user_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: frontier frontier_last_response_id_fkey; Type: FK CONSTRAINT; Schema: crawler; Owner: -
--

ALTER TABLE ONLY crawler.frontier
    ADD CONSTRAINT frontier_last_response_id_fkey FOREIGN KEY (last_response_id) REFERENCES crawler.json_response(id) ON DELETE SET NULL;


--
-- Name: CONSTRAINT frontier_last_response_id_fkey ON frontier; Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON CONSTRAINT frontier_last_response_id_fkey ON crawler.frontier IS '@foreignFieldName targetsWithLatestResponse';


--
-- Name: frontier frontier_last_successful_response_id_fkey; Type: FK CONSTRAINT; Schema: crawler; Owner: -
--

ALTER TABLE ONLY crawler.frontier
    ADD CONSTRAINT frontier_last_successful_response_id_fkey FOREIGN KEY (last_successful_response_id) REFERENCES crawler.json_response(id) ON DELETE SET NULL;


--
-- Name: CONSTRAINT frontier_last_successful_response_id_fkey ON frontier; Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON CONSTRAINT frontier_last_successful_response_id_fkey ON crawler.frontier IS '@foreignFieldName targetsWithLastSuccessfulResponse';


--
-- Name: json_response json_response_content_hash_fkey; Type: FK CONSTRAINT; Schema: crawler; Owner: -
--

ALTER TABLE ONLY crawler.json_response
    ADD CONSTRAINT json_response_content_hash_fkey FOREIGN KEY (content_hash) REFERENCES crawler.json_response_cache(content_hash);


--
-- Name: CONSTRAINT json_response_content_hash_fkey ON json_response; Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON CONSTRAINT json_response_content_hash_fkey ON crawler.json_response IS '@foreignFieldName responses';


--
-- Name: json_response json_response_frontier_id_fkey; Type: FK CONSTRAINT; Schema: crawler; Owner: -
--

ALTER TABLE ONLY crawler.json_response
    ADD CONSTRAINT json_response_frontier_id_fkey FOREIGN KEY (frontier_id) REFERENCES crawler.frontier(id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT json_response_frontier_id_fkey ON json_response; Type: COMMENT; Schema: crawler; Owner: -
--

COMMENT ON CONSTRAINT json_response_frontier_id_fkey ON crawler.json_response IS '@foreignFieldName responses';


--
-- Name: category category_age_group_code_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.category
    ADD CONSTRAINT category_age_group_code_fkey FOREIGN KEY (age_group_code) REFERENCES federated.age_group(code);


--
-- Name: CONSTRAINT category_age_group_code_fkey ON category; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT category_age_group_code_fkey ON federated.category IS '@fieldName ageGroupDetails
@foreignFieldName categories
@foreignSimpleFieldName categories
@backwardBehavior +list -connection';


--
-- Name: category category_base_dance_program_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.category
    ADD CONSTRAINT category_base_dance_program_id_fkey FOREIGN KEY (base_dance_program_id) REFERENCES federated.dance_program(id);


--
-- Name: CONSTRAINT category_base_dance_program_id_fkey ON category; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT category_base_dance_program_id_fkey ON federated.category IS '@foreignFieldName categories';


--
-- Name: competition competition_category_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_category_id_fkey FOREIGN KEY (category_id) REFERENCES federated.category(id);


--
-- Name: competition_entry competition_entry_competition_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_entry
    ADD CONSTRAINT competition_entry_competition_id_fkey FOREIGN KEY (competition_id) REFERENCES federated.competition(id);


--
-- Name: CONSTRAINT competition_entry_competition_id_fkey ON competition_entry; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_entry_competition_id_fkey ON federated.competition_entry IS '@foreignFieldName entries
@backwardBehavior +list';


--
-- Name: competition_entry competition_entry_competitor_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_entry
    ADD CONSTRAINT competition_entry_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);


--
-- Name: CONSTRAINT competition_entry_competitor_id_fkey ON competition_entry; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_entry_competitor_id_fkey ON federated.competition_entry IS '@foreignFieldName competitionEntries';


--
-- Name: competition competition_event_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_event_id_fkey FOREIGN KEY (event_id) REFERENCES federated.event(id);


--
-- Name: CONSTRAINT competition_event_id_fkey ON competition; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_event_id_fkey ON federated.competition IS '@foreignFieldName competitions
@backwardBehavior +list';


--
-- Name: competition competition_federation_event_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_federation_event_id_fkey FOREIGN KEY (federation, event_id) REFERENCES federated.event(federation, id);


--
-- Name: CONSTRAINT competition_federation_event_id_fkey ON competition; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_federation_event_id_fkey ON federated.competition IS '@behavior -single -connection -list';


--
-- Name: competition competition_federation_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);


--
-- Name: CONSTRAINT competition_federation_fkey ON competition; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_federation_fkey ON federated.competition IS '@foreignFieldName competitions';


--
-- Name: competition_official competition_official_competition_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_official
    ADD CONSTRAINT competition_official_competition_id_fkey FOREIGN KEY (competition_id) REFERENCES federated.competition(id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT competition_official_competition_id_fkey ON competition_official; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_official_competition_id_fkey ON federated.competition_official IS '@foreignFieldName officials';


--
-- Name: competition_official competition_official_person_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_official
    ADD CONSTRAINT competition_official_person_id_fkey FOREIGN KEY (person_id) REFERENCES federated.person(id);


--
-- Name: CONSTRAINT competition_official_person_id_fkey ON competition_official; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_official_person_id_fkey ON federated.competition_official IS '@foreignFieldName competitionOfficialAssignments';


--
-- Name: competition_result competition_result_competition_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_result
    ADD CONSTRAINT competition_result_competition_id_fkey FOREIGN KEY (competition_id) REFERENCES federated.competition(id);


--
-- Name: CONSTRAINT competition_result_competition_id_fkey ON competition_result; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_result_competition_id_fkey ON federated.competition_result IS '@foreignFieldName results
@backwardBehavior +list';


--
-- Name: competition_result competition_result_competitor_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_result
    ADD CONSTRAINT competition_result_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);


--
-- Name: CONSTRAINT competition_result_competitor_id_fkey ON competition_result; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_result_competitor_id_fkey ON federated.competition_result IS '@foreignFieldName competitionResults';


--
-- Name: competition_round competition_round_competition_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_competition_id_fkey FOREIGN KEY (competition_id) REFERENCES federated.competition(id);


--
-- Name: CONSTRAINT competition_round_competition_id_fkey ON competition_round; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_round_competition_id_fkey ON federated.competition_round IS '@foreignFieldName rounds
@backwardBehavior +list -connection';


--
-- Name: competition_round competition_round_dance_program_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_dance_program_id_fkey FOREIGN KEY (dance_program_id) REFERENCES federated.dance_program(id);


--
-- Name: competition_round_judge competition_round_judge_person_judge_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_round_judge
    ADD CONSTRAINT competition_round_judge_person_judge_id_fkey FOREIGN KEY (person_judge_id) REFERENCES federated.person(id);


--
-- Name: CONSTRAINT competition_round_judge_person_judge_id_fkey ON competition_round_judge; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_round_judge_person_judge_id_fkey ON federated.competition_round_judge IS '@foreignFieldName roundJudgeAssignments';


--
-- Name: competition_round_judge competition_round_judge_round_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_round_judge
    ADD CONSTRAINT competition_round_judge_round_id_fkey FOREIGN KEY (round_id) REFERENCES federated.competition_round(id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT competition_round_judge_round_id_fkey ON competition_round_judge; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_round_judge_round_id_fkey ON federated.competition_round_judge IS '@foreignFieldName judges
@backwardBehavior +list -connection';


--
-- Name: competition_round_result competition_round_result_competitor_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_round_result
    ADD CONSTRAINT competition_round_result_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);


--
-- Name: CONSTRAINT competition_round_result_competitor_id_fkey ON competition_round_result; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_round_result_competitor_id_fkey ON federated.competition_round_result IS '@foreignFieldName roundResults';


--
-- Name: competition_round_result competition_round_result_round_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competition_round_result
    ADD CONSTRAINT competition_round_result_round_id_fkey FOREIGN KEY (round_id) REFERENCES federated.competition_round(id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT competition_round_result_round_id_fkey ON competition_round_result; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competition_round_result_round_id_fkey ON federated.competition_round_result IS '@foreignFieldName results
@backwardBehavior +list -connection';


--
-- Name: competitor competitor_age_group_code_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_age_group_code_fkey FOREIGN KEY (age_group_code) REFERENCES federated.age_group(code);


--
-- Name: CONSTRAINT competitor_age_group_code_fkey ON competitor; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competitor_age_group_code_fkey ON federated.competitor IS '@fieldName ageGroupDetails
@backwardBehavior -single -list -connection';


--
-- Name: competitor_category_progress competitor_category_progress_category_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor_category_progress
    ADD CONSTRAINT competitor_category_progress_category_id_fkey FOREIGN KEY (category_id) REFERENCES federated.category(id);


--
-- Name: CONSTRAINT competitor_category_progress_category_id_fkey ON competitor_category_progress; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competitor_category_progress_category_id_fkey ON federated.competitor_category_progress IS '@foreignFieldName competitorProgresses';


--
-- Name: competitor_category_progress competitor_category_progress_competitor_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor_category_progress
    ADD CONSTRAINT competitor_category_progress_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);


--
-- Name: CONSTRAINT competitor_category_progress_competitor_id_fkey ON competitor_category_progress; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competitor_category_progress_competitor_id_fkey ON federated.competitor_category_progress IS '@foreignFieldName categoryProgresses
@backwardBehavior +list -connection';


--
-- Name: competitor_club_affiliation competitor_club_affiliation_club_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor_club_affiliation
    ADD CONSTRAINT competitor_club_affiliation_club_id_fkey FOREIGN KEY (club_id) REFERENCES federated.federation_club(id);


--
-- Name: CONSTRAINT competitor_club_affiliation_club_id_fkey ON competitor_club_affiliation; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competitor_club_affiliation_club_id_fkey ON federated.competitor_club_affiliation IS '@foreignFieldName competitorAffiliations';


--
-- Name: competitor_club_affiliation competitor_club_affiliation_competitor_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor_club_affiliation
    ADD CONSTRAINT competitor_club_affiliation_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);


--
-- Name: CONSTRAINT competitor_club_affiliation_competitor_id_fkey ON competitor_club_affiliation; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competitor_club_affiliation_competitor_id_fkey ON federated.competitor_club_affiliation IS '@foreignFieldName clubAffiliations';


--
-- Name: competitor_component competitor_component_competitor_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor_component
    ADD CONSTRAINT competitor_component_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT competitor_component_competitor_id_fkey ON competitor_component; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competitor_component_competitor_id_fkey ON federated.competitor_component IS '@foreignFieldName components';


--
-- Name: competitor_component competitor_component_person_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor_component
    ADD CONSTRAINT competitor_component_person_id_fkey FOREIGN KEY (person_id) REFERENCES federated.person(id);


--
-- Name: CONSTRAINT competitor_component_person_id_fkey ON competitor_component; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competitor_component_person_id_fkey ON federated.competitor_component IS '@foreignFieldName competitorComponents';


--
-- Name: competitor competitor_federation_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);


--
-- Name: CONSTRAINT competitor_federation_fkey ON competitor; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT competitor_federation_fkey ON federated.competitor IS '@foreignFieldName competitors';


--
-- Name: couple_age_rule couple_age_rule_age_group_code_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.couple_age_rule
    ADD CONSTRAINT couple_age_rule_age_group_code_fkey FOREIGN KEY (age_group_code) REFERENCES federated.age_group(code);


--
-- Name: CONSTRAINT couple_age_rule_age_group_code_fkey ON couple_age_rule; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT couple_age_rule_age_group_code_fkey ON federated.couple_age_rule IS '@fieldName ageGroupDetails
@foreignFieldName coupleRules
@foreignSimpleFieldName coupleRules
@backwardBehavior +list -connection';


--
-- Name: dance_program_dance dance_program_dance_dance_code_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.dance_program_dance
    ADD CONSTRAINT dance_program_dance_dance_code_fkey FOREIGN KEY (dance_code) REFERENCES federated.dance(code);


--
-- Name: CONSTRAINT dance_program_dance_dance_code_fkey ON dance_program_dance; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT dance_program_dance_dance_code_fkey ON federated.dance_program_dance IS '@foreignFieldName dancePrograms';


--
-- Name: dance_program_dance dance_program_dance_program_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.dance_program_dance
    ADD CONSTRAINT dance_program_dance_program_id_fkey FOREIGN KEY (program_id) REFERENCES federated.dance_program(id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT dance_program_dance_program_id_fkey ON dance_program_dance; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT dance_program_dance_program_id_fkey ON federated.dance_program_dance IS '@foreignFieldName dances';


--
-- Name: event event_federation_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);


--
-- Name: CONSTRAINT event_federation_fkey ON event; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT event_federation_fkey ON federated.event IS '@foreignFieldName events';


--
-- Name: event event_federation_organizing_club_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_federation_organizing_club_id_fkey FOREIGN KEY (federation, organizing_club_id) REFERENCES federated.federation_club(federation, id);


--
-- Name: CONSTRAINT event_federation_organizing_club_id_fkey ON event; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT event_federation_organizing_club_id_fkey ON federated.event IS '@behavior -single -connection -list';


--
-- Name: event_official event_official_event_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.event_official
    ADD CONSTRAINT event_official_event_id_fkey FOREIGN KEY (event_id) REFERENCES federated.event(id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT event_official_event_id_fkey ON event_official; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT event_official_event_id_fkey ON federated.event_official IS '@foreignFieldName officials';


--
-- Name: event_official event_official_person_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.event_official
    ADD CONSTRAINT event_official_person_id_fkey FOREIGN KEY (person_id) REFERENCES federated.person(id);


--
-- Name: CONSTRAINT event_official_person_id_fkey ON event_official; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT event_official_person_id_fkey ON federated.event_official IS '@foreignFieldName eventOfficialAssignments';


--
-- Name: event event_organizing_club_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_organizing_club_id_fkey FOREIGN KEY (organizing_club_id) REFERENCES federated.federation_club(id);


--
-- Name: CONSTRAINT event_organizing_club_id_fkey ON event; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT event_organizing_club_id_fkey ON federated.event IS '@foreignFieldName events';


--
-- Name: federation_club federation_club_federation_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.federation_club
    ADD CONSTRAINT federation_club_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);


--
-- Name: CONSTRAINT federation_club_federation_fkey ON federation_club; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT federation_club_federation_fkey ON federated.federation_club IS '@foreignFieldName clubs';


--
-- Name: judge_score judge_score_round_id_dance_order_dance_code_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_round_id_dance_order_dance_code_fkey FOREIGN KEY (round_id, dance_order, dance_code) REFERENCES federated.round_dance(round_id, dance_order, dance_code) ON DELETE CASCADE;


--
-- Name: CONSTRAINT judge_score_round_id_dance_order_dance_code_fkey ON judge_score; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT judge_score_round_id_dance_order_dance_code_fkey ON federated.judge_score IS '@foreignFieldName judgeScores';


--
-- Name: judge_score judge_score_round_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_round_id_fkey FOREIGN KEY (round_id) REFERENCES federated.competition_round(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT judge_score_round_id_fkey ON judge_score; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT judge_score_round_id_fkey ON federated.judge_score IS '@foreignFieldName judgeScores
@backwardBehavior +list -connection';


--
-- Name: person person_age_group_code_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.person
    ADD CONSTRAINT person_age_group_code_fkey FOREIGN KEY (age_group_code) REFERENCES federated.age_group(code);


--
-- Name: CONSTRAINT person_age_group_code_fkey ON person; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT person_age_group_code_fkey ON federated.person IS '@fieldName ageGroupDetails
@backwardBehavior -single -list -connection';


--
-- Name: person_club_membership person_club_membership_club_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.person_club_membership
    ADD CONSTRAINT person_club_membership_club_id_fkey FOREIGN KEY (club_id) REFERENCES federated.federation_club(id);


--
-- Name: CONSTRAINT person_club_membership_club_id_fkey ON person_club_membership; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT person_club_membership_club_id_fkey ON federated.person_club_membership IS '@foreignFieldName personMemberships';


--
-- Name: person_club_membership person_club_membership_person_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.person_club_membership
    ADD CONSTRAINT person_club_membership_person_id_fkey FOREIGN KEY (person_id) REFERENCES federated.person(id);


--
-- Name: CONSTRAINT person_club_membership_person_id_fkey ON person_club_membership; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT person_club_membership_person_id_fkey ON federated.person_club_membership IS '@foreignFieldName clubMemberships';


--
-- Name: person person_federation_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.person
    ADD CONSTRAINT person_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);


--
-- Name: CONSTRAINT person_federation_fkey ON person; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT person_federation_fkey ON federated.person IS '@foreignFieldName people';


--
-- Name: person_license person_license_federation_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.person_license
    ADD CONSTRAINT person_license_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);


--
-- Name: CONSTRAINT person_license_federation_fkey ON person_license; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT person_license_federation_fkey ON federated.person_license IS '@foreignFieldName licenses';


--
-- Name: person_license person_license_person_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.person_license
    ADD CONSTRAINT person_license_person_id_fkey FOREIGN KEY (person_id) REFERENCES federated.person(id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT person_license_person_id_fkey ON person_license; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT person_license_person_id_fkey ON federated.person_license IS '@foreignFieldName licenses';


--
-- Name: ranklist ranklist_category_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.ranklist
    ADD CONSTRAINT ranklist_category_id_fkey FOREIGN KEY (category_id) REFERENCES federated.category(id);


--
-- Name: ranklist_entry ranklist_entry_competitor_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.ranklist_entry
    ADD CONSTRAINT ranklist_entry_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);


--
-- Name: CONSTRAINT ranklist_entry_competitor_id_fkey ON ranklist_entry; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT ranklist_entry_competitor_id_fkey ON federated.ranklist_entry IS '@foreignFieldName ranklistEntries';


--
-- Name: ranklist_entry ranklist_entry_snapshot_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.ranklist_entry
    ADD CONSTRAINT ranklist_entry_snapshot_id_fkey FOREIGN KEY (snapshot_id) REFERENCES federated.ranklist_snapshot(id);


--
-- Name: ranklist ranklist_federation_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.ranklist
    ADD CONSTRAINT ranklist_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);


--
-- Name: CONSTRAINT ranklist_federation_fkey ON ranklist; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT ranklist_federation_fkey ON federated.ranklist IS '@foreignFieldName ranklists';


--
-- Name: ranklist_snapshot ranklist_snapshot_ranklist_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.ranklist_snapshot
    ADD CONSTRAINT ranklist_snapshot_ranklist_id_fkey FOREIGN KEY (ranklist_id) REFERENCES federated.ranklist(id);


--
-- Name: round_dance round_dance_dance_program_id_dance_code_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.round_dance
    ADD CONSTRAINT round_dance_dance_program_id_dance_code_fkey FOREIGN KEY (dance_program_id, dance_code) REFERENCES federated.dance_program_dance(program_id, dance_code);


--
-- Name: CONSTRAINT round_dance_dance_program_id_dance_code_fkey ON round_dance; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT round_dance_dance_program_id_dance_code_fkey ON federated.round_dance IS '@foreignFieldName roundDances';


--
-- Name: round_dance round_dance_round_id_dance_program_id_fkey; Type: FK CONSTRAINT; Schema: federated; Owner: -
--

ALTER TABLE ONLY federated.round_dance
    ADD CONSTRAINT round_dance_round_id_dance_program_id_fkey FOREIGN KEY (round_id, dance_program_id) REFERENCES federated.competition_round(id, dance_program_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT round_dance_round_id_dance_program_id_fkey ON round_dance; Type: COMMENT; Schema: federated; Owner: -
--

COMMENT ON CONSTRAINT round_dance_round_id_dance_program_id_fkey ON federated.round_dance IS '@foreignFieldName dances
@backwardBehavior +list -connection';


--
-- Name: access_credential access_credential_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.access_credential
    ADD CONSTRAINT access_credential_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: access_credential access_credential_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.access_credential
    ADD CONSTRAINT access_credential_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id);


--
-- Name: access_credential access_credential_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.access_credential
    ADD CONSTRAINT access_credential_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: access_event access_event_location_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.access_event
    ADD CONSTRAINT access_event_location_fkey FOREIGN KEY (tenant_id, location_id) REFERENCES public.location(tenant_id, id);


--
-- Name: CONSTRAINT access_event_location_fkey ON access_event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT access_event_location_fkey ON public.access_event IS '@fieldName location
@foreignFieldName accessEvents';


--
-- Name: access_event access_event_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.access_event
    ADD CONSTRAINT access_event_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id);


--
-- Name: access_event access_event_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.access_event
    ADD CONSTRAINT access_event_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: account account_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: account account_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: accounting_period accounting_period_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.accounting_period
    ADD CONSTRAINT accounting_period_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: aktuality aktuality_at_kdo_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_at_kdo_fkey FOREIGN KEY (at_kdo) REFERENCES public.users(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: aktuality aktuality_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: announcement_attachment announcement_attachment_announcement_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.announcement_attachment
    ADD CONSTRAINT announcement_attachment_announcement_fk FOREIGN KEY (tenant_id, announcement_id) REFERENCES public.announcement(tenant_id, id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT announcement_attachment_announcement_fk ON announcement_attachment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT announcement_attachment_announcement_fk ON public.announcement_attachment IS '@fieldName announcement
@foreignFieldName attachments';


--
-- Name: announcement_attachment announcement_attachment_file_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.announcement_attachment
    ADD CONSTRAINT announcement_attachment_file_fk FOREIGN KEY (tenant_id, file_id) REFERENCES public.file(tenant_id, id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT announcement_attachment_file_fk ON announcement_attachment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT announcement_attachment_file_fk ON public.announcement_attachment IS '@fieldName file
@foreignFieldName announcementAttachments';


--
-- Name: announcement_audience announcement_audience_announcement_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.announcement_audience
    ADD CONSTRAINT announcement_audience_announcement_id_fkey FOREIGN KEY (announcement_id) REFERENCES public.announcement(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: announcement_audience announcement_audience_cohort_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.announcement_audience
    ADD CONSTRAINT announcement_audience_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.cohort(id);


--
-- Name: announcement_audience announcement_audience_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.announcement_audience
    ADD CONSTRAINT announcement_audience_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: announcement announcement_author_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.announcement
    ADD CONSTRAINT announcement_author_id_fkey FOREIGN KEY (author_id) REFERENCES public.users(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: announcement announcement_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.announcement
    ADD CONSTRAINT announcement_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: article_attachment article_attachment_article_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.article_attachment
    ADD CONSTRAINT article_attachment_article_fk FOREIGN KEY (tenant_id, aktuality_id) REFERENCES public.aktuality(tenant_id, id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT article_attachment_article_fk ON article_attachment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT article_attachment_article_fk ON public.article_attachment IS '
@fieldName article
@foreignFieldName attachments';


--
-- Name: article_attachment article_attachment_file_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.article_attachment
    ADD CONSTRAINT article_attachment_file_fk FOREIGN KEY (tenant_id, file_id) REFERENCES public.file(tenant_id, id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT article_attachment_file_fk ON article_attachment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT article_attachment_file_fk ON public.article_attachment IS '
@fieldName file
@foreignFieldName articleAttachments';


--
-- Name: cohort cohort_cohort_group_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_cohort_group_fkey FOREIGN KEY (tenant_id, cohort_group_id) REFERENCES public.cohort_group(tenant_id, id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: CONSTRAINT cohort_cohort_group_fkey ON cohort; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT cohort_cohort_group_fkey ON public.cohort IS '@fieldName cohortGroup
@foreignFieldName cohorts';


--
-- Name: cohort_group cohort_group_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_group
    ADD CONSTRAINT cohort_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: cohort cohort_image_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_image_fk FOREIGN KEY (tenant_id, image_id) REFERENCES public.file(tenant_id, id) ON DELETE SET NULL (image_id);


--
-- Name: CONSTRAINT cohort_image_fk ON cohort; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT cohort_image_fk ON public.cohort IS '@fieldName image';


--
-- Name: cohort_membership cohort_membership_cohort_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_cohort_fkey FOREIGN KEY (tenant_id, cohort_id) REFERENCES public.cohort(tenant_id, id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT cohort_membership_cohort_fkey ON cohort_membership; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT cohort_membership_cohort_fkey ON public.cohort_membership IS '@fieldName cohort
@foreignFieldName cohortMemberships';


--
-- Name: cohort_membership cohort_membership_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: cohort_membership cohort_membership_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: cohort_subscription cohort_subscription_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_subscription
    ADD CONSTRAINT cohort_subscription_account_fkey FOREIGN KEY (tenant_id, account_id) REFERENCES public.account(tenant_id, id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT cohort_subscription_account_fkey ON cohort_subscription; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT cohort_subscription_account_fkey ON public.cohort_subscription IS '@fieldName account
@foreignFieldName cohortSubscriptions';


--
-- Name: cohort_subscription cohort_subscription_cohort_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_subscription
    ADD CONSTRAINT cohort_subscription_cohort_fkey FOREIGN KEY (tenant_id, cohort_id) REFERENCES public.cohort(tenant_id, id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT cohort_subscription_cohort_fkey ON cohort_subscription; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT cohort_subscription_cohort_fkey ON public.cohort_subscription IS '@fieldName cohort
@foreignFieldName cohortSubscriptions';


--
-- Name: cohort_subscription cohort_subscription_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_subscription
    ADD CONSTRAINT cohort_subscription_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: cohort cohort_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: couple couple_image_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_image_fk FOREIGN KEY (image_id) REFERENCES public.file(id) ON DELETE SET NULL;


--
-- Name: CONSTRAINT couple_image_fk ON couple; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT couple_image_fk ON public.couple IS '@fieldName image';


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
    ADD CONSTRAINT dokumenty_d_kdo_fkey FOREIGN KEY (d_kdo) REFERENCES public.users(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: dokumenty dokumenty_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dokumenty
    ADD CONSTRAINT dokumenty_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: event event_cover_image_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_cover_image_fk FOREIGN KEY (tenant_id, cover_image_id) REFERENCES public.file(tenant_id, id) ON DELETE SET NULL (cover_image_id);


--
-- Name: CONSTRAINT event_cover_image_fk ON event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_cover_image_fk ON public.event IS '@fieldName coverImage';


--
-- Name: event_lesson_request event_lesson_request_registration_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_lesson_request
    ADD CONSTRAINT event_lesson_request_registration_id_fkey FOREIGN KEY (registration_id) REFERENCES public.event_registration(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT event_lesson_request_registration_id_fkey ON event_lesson_request; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_lesson_request_registration_id_fkey ON public.event_lesson_request IS '@foreignFieldName lessonRequests';


--
-- Name: event_lesson_request event_lesson_request_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_lesson_request
    ADD CONSTRAINT event_lesson_request_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT event_lesson_request_tenant_id_fkey ON event_lesson_request; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_lesson_request_tenant_id_fkey ON public.event_lesson_request IS '@foreignFieldName lessonRequests';


--
-- Name: event_lesson_request event_lesson_request_trainer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_lesson_request
    ADD CONSTRAINT event_lesson_request_trainer_id_fkey FOREIGN KEY (trainer_id) REFERENCES public.event_trainer(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT event_lesson_request_trainer_id_fkey ON event_lesson_request; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_lesson_request_trainer_id_fkey ON public.event_lesson_request IS '@foreignFieldName lessonRequests';


--
-- Name: event event_location_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_location_fkey FOREIGN KEY (tenant_id, location_id) REFERENCES public.location(tenant_id, id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: CONSTRAINT event_location_fkey ON event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_location_fkey ON public.event IS '@fieldName location
@foreignFieldName events';


--
-- Name: event event_parent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_parent_id_fkey FOREIGN KEY (tenant_id, parent_id) REFERENCES public.event(tenant_id, id) ON UPDATE CASCADE;


--
-- Name: CONSTRAINT event_parent_id_fkey ON event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_parent_id_fkey ON public.event IS '@fieldName parent
@foreignFieldName childEvents';


--
-- Name: event_registration event_registration_couple_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_couple_id_fkey FOREIGN KEY (couple_id) REFERENCES public.couple(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_registration event_registration_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: CONSTRAINT event_registration_created_by_fkey ON event_registration; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_registration_created_by_fkey ON public.event_registration IS '@behavior -*';


--
-- Name: event_registration event_registration_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT event_registration_event_id_fkey ON event_registration; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_registration_event_id_fkey ON public.event_registration IS '@fieldName event
@foreignFieldName registrations';


--
-- Name: event_registration event_registration_parent_registration_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_parent_registration_id_fkey FOREIGN KEY (parent_registration_id) REFERENCES public.event_registration(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT event_registration_parent_registration_id_fkey ON event_registration; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_registration_parent_registration_id_fkey ON public.event_registration IS '@fieldName parentRegistration
@foreignFieldName childRegistrations';


--
-- Name: event_registration event_registration_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_registration event_registration_target_cohort_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_target_cohort_id_fkey FOREIGN KEY (target_cohort_id) REFERENCES public.cohort(id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: event_registration event_registration_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event event_series_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_series_fkey FOREIGN KEY (tenant_id, series_id) REFERENCES public.event_series(tenant_id, id) ON UPDATE CASCADE ON DELETE SET NULL (series_id);


--
-- Name: CONSTRAINT event_series_fkey ON event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_series_fkey ON public.event IS '@fieldName series
@foreignFieldName events';


--
-- Name: event_series event_series_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_series
    ADD CONSTRAINT event_series_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT event_series_tenant_id_fkey ON event_series; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_series_tenant_id_fkey ON public.event_series IS '@fieldName tenant
@foreignFieldName eventSeries';


--
-- Name: event_target_cohort event_target_cohort_cohort_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.cohort(id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: event_target_cohort event_target_cohort_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT event_target_cohort_event_id_fkey ON event_target_cohort; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_target_cohort_event_id_fkey ON public.event_target_cohort IS '@fieldName event
@foreignFieldName targetCohorts';


--
-- Name: event_target_cohort event_target_cohort_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event event_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_trainer event_trainer_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_trainer event_trainer_tenant_id_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_tenant_id_event_id_fkey FOREIGN KEY (tenant_id, event_id) REFERENCES public.event(tenant_id, id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT event_trainer_tenant_id_event_id_fkey ON event_trainer; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT event_trainer_tenant_id_event_id_fkey ON public.event_trainer IS '@fieldName event
@foreignFieldName eventTrainers';


--
-- Name: event_trainer event_trainer_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: file file_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.file
    ADD CONSTRAINT file_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: file file_uploaded_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.file
    ADD CONSTRAINT file_uploaded_by_fkey FOREIGN KEY (uploaded_by) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: form_responses form_responses_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.form_responses
    ADD CONSTRAINT form_responses_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: membership_application membership_application_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.membership_application
    ADD CONSTRAINT membership_application_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: membership_application membership_application_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.membership_application
    ADD CONSTRAINT membership_application_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: otp_token otp_token_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.otp_token
    ADD CONSTRAINT otp_token_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON DELETE CASCADE;


--
-- Name: otp_token otp_token_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.otp_token
    ADD CONSTRAINT otp_token_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: otp_token otp_token_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.otp_token
    ADD CONSTRAINT otp_token_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: payment payment_accounting_period_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_accounting_period_id_fkey FOREIGN KEY (accounting_period_id) REFERENCES public.accounting_period(id);


--
-- Name: payment payment_cohort_subscription_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_cohort_subscription_id_fkey FOREIGN KEY (cohort_subscription_id) REFERENCES public.cohort_subscription(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: payment_debtor payment_debtor_payment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_debtor
    ADD CONSTRAINT payment_debtor_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES public.payment(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: payment_debtor payment_debtor_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_debtor
    ADD CONSTRAINT payment_debtor_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: payment_debtor payment_debtor_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_debtor
    ADD CONSTRAINT payment_debtor_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: payment payment_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT payment_event_id_fkey ON payment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT payment_event_id_fkey ON public.payment IS '@fieldName event
@foreignFieldName payments';


--
-- Name: payment_recipient payment_recipient_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_recipient
    ADD CONSTRAINT payment_recipient_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: payment_recipient payment_recipient_payment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_recipient
    ADD CONSTRAINT payment_recipient_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES public.payment(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: payment_recipient payment_recipient_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_recipient
    ADD CONSTRAINT payment_recipient_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: payment payment_tenant_id_accounting_period_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_tenant_id_accounting_period_fkey FOREIGN KEY (tenant_id, accounting_period_id) REFERENCES public.accounting_period(tenant_id, id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: payment payment_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: person person_image_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person
    ADD CONSTRAINT person_image_fk FOREIGN KEY (image_id) REFERENCES public.file(id) ON DELETE SET NULL;


--
-- Name: CONSTRAINT person_image_fk ON person; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT person_image_fk ON public.person IS '@fieldName image';


--
-- Name: person_invitation person_invitation_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person_invitation
    ADD CONSTRAINT person_invitation_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON DELETE SET NULL;


--
-- Name: CONSTRAINT person_invitation_event_id_fkey ON person_invitation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT person_invitation_event_id_fkey ON public.person_invitation IS '@behavior -*';


--
-- Name: person_invitation person_invitation_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person_invitation
    ADD CONSTRAINT person_invitation_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON DELETE CASCADE;


--
-- Name: person_invitation person_invitation_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.person_invitation
    ADD CONSTRAINT person_invitation_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: posting posting_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posting
    ADD CONSTRAINT posting_account_fkey FOREIGN KEY (tenant_id, account_id) REFERENCES public.account(tenant_id, id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: CONSTRAINT posting_account_fkey ON posting; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT posting_account_fkey ON public.posting IS '@fieldName account
@foreignFieldName postings';


--
-- Name: posting posting_original_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posting
    ADD CONSTRAINT posting_original_account_id_fkey FOREIGN KEY (original_account_id) REFERENCES public.account(id);


--
-- Name: posting posting_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posting
    ADD CONSTRAINT posting_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: posting posting_transaction_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posting
    ADD CONSTRAINT posting_transaction_fkey FOREIGN KEY (tenant_id, transaction_id) REFERENCES public.transaction(tenant_id, id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT posting_transaction_fkey ON posting; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT posting_transaction_fkey ON public.posting IS '@fieldName transaction
@foreignFieldName postings';


--
-- Name: scoreboard_manual_adjustment scoreboard_manual_adjustment_cohort_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scoreboard_manual_adjustment
    ADD CONSTRAINT scoreboard_manual_adjustment_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.cohort(id);


--
-- Name: scoreboard_manual_adjustment scoreboard_manual_adjustment_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scoreboard_manual_adjustment
    ADD CONSTRAINT scoreboard_manual_adjustment_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: scoreboard_manual_adjustment scoreboard_manual_adjustment_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scoreboard_manual_adjustment
    ADD CONSTRAINT scoreboard_manual_adjustment_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);


--
-- Name: security_event security_event_actor_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.security_event
    ADD CONSTRAINT security_event_actor_user_id_fkey FOREIGN KEY (actor_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: CONSTRAINT security_event_actor_user_id_fkey ON security_event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT security_event_actor_user_id_fkey ON public.security_event IS '@behavior -manyRelation:resource:list -manyRelation:resource:connection';


--
-- Name: security_event security_event_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.security_event
    ADD CONSTRAINT security_event_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON DELETE SET NULL;


--
-- Name: security_event security_event_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.security_event
    ADD CONSTRAINT security_event_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT security_event_tenant_id_fkey ON security_event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT security_event_tenant_id_fkey ON public.security_event IS '@behavior -manyRelation:resource:list -manyRelation:resource:connection';


--
-- Name: security_event security_event_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.security_event
    ADD CONSTRAINT security_event_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE SET NULL;


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
-- Name: location tenant_location_cover_image_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location
    ADD CONSTRAINT tenant_location_cover_image_fk FOREIGN KEY (tenant_id, cover_image_id) REFERENCES public.file(tenant_id, id) ON DELETE SET NULL (cover_image_id);


--
-- Name: CONSTRAINT tenant_location_cover_image_fk ON location; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT tenant_location_cover_image_fk ON public.location IS '@fieldName coverImage
@behavior -manyRelation:resource:list -manyRelation:resource:connection';


--
-- Name: location_image tenant_location_image_file_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location_image
    ADD CONSTRAINT tenant_location_image_file_fk FOREIGN KEY (tenant_id, file_id) REFERENCES public.file(tenant_id, id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT tenant_location_image_file_fk ON location_image; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT tenant_location_image_file_fk ON public.location_image IS '@fieldName file
@foreignFieldName locationImageRecords';


--
-- Name: location_image tenant_location_image_location_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location_image
    ADD CONSTRAINT tenant_location_image_location_fk FOREIGN KEY (tenant_id, location_id) REFERENCES public.location(tenant_id, id) ON DELETE CASCADE;


--
-- Name: CONSTRAINT tenant_location_image_location_fk ON location_image; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT tenant_location_image_location_fk ON public.location_image IS '@fieldName location
@foreignFieldName images';


--
-- Name: location tenant_location_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location
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
-- Name: tenant_settings tenant_settings_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_settings
    ADD CONSTRAINT tenant_settings_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


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
-- Name: transaction transaction_accounting_period_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_accounting_period_fkey FOREIGN KEY (tenant_id, accounting_period_id) REFERENCES public.accounting_period(tenant_id, id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: CONSTRAINT transaction_accounting_period_fkey ON transaction; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT transaction_accounting_period_fkey ON public.transaction IS '@fieldName accountingPeriod
@foreignFieldName transactions';


--
-- Name: transaction transaction_payment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_payment_fkey FOREIGN KEY (tenant_id, payment_id) REFERENCES public.payment(tenant_id, id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: CONSTRAINT transaction_payment_fkey ON transaction; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT transaction_payment_fkey ON public.transaction IS '@fieldName payment
@foreignFieldName transactions';


--
-- Name: transaction transaction_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: user_proxy user_proxy_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_proxy
    ADD CONSTRAINT user_proxy_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: user_proxy user_proxy_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_proxy
    ADD CONSTRAINT user_proxy_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: users users_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: platby_category admin_all; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY admin_all ON app_private.platby_category TO administrator USING (true);


--
-- Name: platby_category_group admin_all; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY admin_all ON app_private.platby_category_group TO administrator USING (true);


--
-- Name: platby_group admin_all; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY admin_all ON app_private.platby_group TO administrator USING (true);


--
-- Name: platby_group_skupina admin_all; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY admin_all ON app_private.platby_group_skupina TO administrator USING (true);


--
-- Name: platby_item admin_all; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY admin_all ON app_private.platby_item TO administrator USING (true);


--
-- Name: platby_raw admin_all; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY admin_all ON app_private.platby_raw TO administrator USING (true);


--
-- Name: platby_category current_tenant; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY current_tenant ON app_private.platby_category AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: platby_category_group current_tenant; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY current_tenant ON app_private.platby_category_group AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: platby_group current_tenant; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY current_tenant ON app_private.platby_group AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: platby_group_skupina current_tenant; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY current_tenant ON app_private.platby_group_skupina AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: platby_item current_tenant; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY current_tenant ON app_private.platby_item AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: platby_raw current_tenant; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY current_tenant ON app_private.platby_raw AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: platby_category member_view; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY member_view ON app_private.platby_category FOR SELECT TO member USING (true);


--
-- Name: platby_category_group member_view; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY member_view ON app_private.platby_category_group FOR SELECT TO member USING (true);


--
-- Name: platby_group member_view; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY member_view ON app_private.platby_group FOR SELECT TO member USING (true);


--
-- Name: platby_group_skupina member_view; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY member_view ON app_private.platby_group_skupina FOR SELECT TO member USING (true);


--
-- Name: platby_raw member_view; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY member_view ON app_private.platby_raw FOR SELECT TO member USING ((EXISTS ( SELECT
   FROM app_private.platby_item
  WHERE ((platby_item.pi_id_raw = platby_raw.pr_id) AND (platby_item.pi_id_user = public.current_user_id())))));


--
-- Name: platby_category; Type: ROW SECURITY; Schema: app_private; Owner: -
--

ALTER TABLE app_private.platby_category ENABLE ROW LEVEL SECURITY;

--
-- Name: platby_category_group; Type: ROW SECURITY; Schema: app_private; Owner: -
--

ALTER TABLE app_private.platby_category_group ENABLE ROW LEVEL SECURITY;

--
-- Name: platby_group; Type: ROW SECURITY; Schema: app_private; Owner: -
--

ALTER TABLE app_private.platby_group ENABLE ROW LEVEL SECURITY;

--
-- Name: platby_group_skupina; Type: ROW SECURITY; Schema: app_private; Owner: -
--

ALTER TABLE app_private.platby_group_skupina ENABLE ROW LEVEL SECURITY;

--
-- Name: platby_item; Type: ROW SECURITY; Schema: app_private; Owner: -
--

ALTER TABLE app_private.platby_item ENABLE ROW LEVEL SECURITY;

--
-- Name: platby_raw; Type: ROW SECURITY; Schema: app_private; Owner: -
--

ALTER TABLE app_private.platby_raw ENABLE ROW LEVEL SECURITY;

--
-- Name: platby_item view_my; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY view_my ON app_private.platby_item FOR SELECT TO member USING ((pi_id_user = public.current_user_id()));


--
-- Name: frontier; Type: ROW SECURITY; Schema: crawler; Owner: -
--

ALTER TABLE crawler.frontier ENABLE ROW LEVEL SECURITY;

--
-- Name: incremental_ranges; Type: ROW SECURITY; Schema: crawler; Owner: -
--

ALTER TABLE crawler.incremental_ranges ENABLE ROW LEVEL SECURITY;

--
-- Name: json_response; Type: ROW SECURITY; Schema: crawler; Owner: -
--

ALTER TABLE crawler.json_response ENABLE ROW LEVEL SECURITY;

--
-- Name: json_response_cache; Type: ROW SECURITY; Schema: crawler; Owner: -
--

ALTER TABLE crawler.json_response_cache ENABLE ROW LEVEL SECURITY;

--
-- Name: rate_limit_rule; Type: ROW SECURITY; Schema: crawler; Owner: -
--

ALTER TABLE crawler.rate_limit_rule ENABLE ROW LEVEL SECURITY;

--
-- Name: frontier system_admin_read; Type: POLICY; Schema: crawler; Owner: -
--

CREATE POLICY system_admin_read ON crawler.frontier FOR SELECT TO system_admin USING (true);


--
-- Name: incremental_ranges system_admin_read; Type: POLICY; Schema: crawler; Owner: -
--

CREATE POLICY system_admin_read ON crawler.incremental_ranges FOR SELECT TO system_admin USING (true);


--
-- Name: json_response system_admin_read; Type: POLICY; Schema: crawler; Owner: -
--

CREATE POLICY system_admin_read ON crawler.json_response FOR SELECT TO system_admin USING (true);


--
-- Name: json_response_cache system_admin_read; Type: POLICY; Schema: crawler; Owner: -
--

CREATE POLICY system_admin_read ON crawler.json_response_cache FOR SELECT TO system_admin USING (true);


--
-- Name: rate_limit_rule system_admin_read; Type: POLICY; Schema: crawler; Owner: -
--

CREATE POLICY system_admin_read ON crawler.rate_limit_rule FOR SELECT TO system_admin USING (true);


--
-- Name: access_credential; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.access_credential ENABLE ROW LEVEL SECURITY;

--
-- Name: access_event; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.access_event ENABLE ROW LEVEL SECURITY;

--
-- Name: account; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.account ENABLE ROW LEVEL SECURITY;

--
-- Name: accounting_period; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.accounting_period ENABLE ROW LEVEL SECURITY;

--
-- Name: aktuality admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.aktuality TO administrator USING (true);


--
-- Name: announcement admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.announcement TO administrator USING (true);


--
-- Name: announcement_attachment admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.announcement_attachment TO administrator USING (true);


--
-- Name: announcement_audience admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.announcement_audience TO administrator USING (true);


--
-- Name: article_attachment admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.article_attachment TO administrator USING (true);


--
-- Name: cohort admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.cohort TO administrator USING (true);


--
-- Name: cohort_group admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.cohort_group TO administrator USING (true);


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

CREATE POLICY admin_all ON public.dokumenty TO administrator USING (true);


--
-- Name: event_lesson_request admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_lesson_request TO administrator USING (true);


--
-- Name: event_registration admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_registration TO administrator USING (true);


--
-- Name: event_series admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_series TO administrator USING (true);


--
-- Name: event_target_cohort admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_target_cohort TO administrator USING (true);


--
-- Name: event_trainer admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_trainer TO administrator USING (true);


--
-- Name: file admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.file TO administrator USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: form_responses admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.form_responses TO administrator USING (true);


--
-- Name: location admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.location TO administrator USING (true);


--
-- Name: location_image admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.location_image TO administrator USING (true);


--
-- Name: person admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.person TO administrator USING (true);


--
-- Name: person_invitation admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.person_invitation TO administrator USING (true);


--
-- Name: scoreboard_manual_adjustment admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.scoreboard_manual_adjustment TO administrator USING (true);


--
-- Name: tenant admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant TO administrator USING ((id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: tenant_administrator admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant_administrator TO administrator USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: tenant_membership admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant_membership TO administrator USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: tenant_trainer admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant_trainer TO administrator USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: user_proxy admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.user_proxy TO administrator USING (true);


--
-- Name: users admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.users TO administrator USING (true);


--
-- Name: access_credential admin_insert; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_insert ON public.access_credential FOR INSERT TO administrator WITH CHECK (((EXISTS ( SELECT 1
   FROM public.tenant_membership r
  WHERE ((r.tenant_id = access_credential.tenant_id) AND (r.person_id = access_credential.person_id)))) OR (EXISTS ( SELECT 1
   FROM public.tenant_trainer r
  WHERE ((r.tenant_id = access_credential.tenant_id) AND (r.person_id = access_credential.person_id)))) OR (EXISTS ( SELECT 1
   FROM public.tenant_administrator r
  WHERE ((r.tenant_id = access_credential.tenant_id) AND (r.person_id = access_credential.person_id))))));


--
-- Name: account admin_manage; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_manage ON public.account TO administrator USING (true);


--
-- Name: accounting_period admin_manage; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_manage ON public.accounting_period TO administrator USING (true);


--
-- Name: cohort_subscription admin_manage; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_manage ON public.cohort_subscription TO administrator USING (true);


--
-- Name: payment admin_manage; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_manage ON public.payment TO administrator USING (true);


--
-- Name: payment_debtor admin_manage; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_manage ON public.payment_debtor TO administrator USING (true);


--
-- Name: payment_recipient admin_manage; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_manage ON public.payment_recipient TO administrator USING (true);


--
-- Name: posting admin_manage; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_manage ON public.posting TO administrator USING (true);


--
-- Name: transaction admin_manage; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_manage ON public.transaction TO administrator USING (true);


--
-- Name: person admin_myself; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_myself ON public.person FOR UPDATE USING ((id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])));


--
-- Name: tenant_settings admin_own; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_own ON public.tenant_settings TO administrator USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: event admin_same_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_same_tenant ON public.event TO administrator USING (true);


--
-- Name: access_credential admin_update; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_update ON public.access_credential FOR UPDATE TO administrator USING (true) WITH CHECK (((EXISTS ( SELECT 1
   FROM public.tenant_membership r
  WHERE ((r.tenant_id = access_credential.tenant_id) AND (r.person_id = access_credential.person_id)))) OR (EXISTS ( SELECT 1
   FROM public.tenant_trainer r
  WHERE ((r.tenant_id = access_credential.tenant_id) AND (r.person_id = access_credential.person_id)))) OR (EXISTS ( SELECT 1
   FROM public.tenant_administrator r
  WHERE ((r.tenant_id = access_credential.tenant_id) AND (r.person_id = access_credential.person_id))))));


--
-- Name: access_credential admin_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_view ON public.access_credential FOR SELECT TO administrator USING (true);


--
-- Name: access_event admin_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_view ON public.access_event FOR SELECT TO administrator USING (true);


--
-- Name: security_event admin_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_view ON public.security_event FOR SELECT TO administrator USING (true);


--
-- Name: aktuality; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.aktuality ENABLE ROW LEVEL SECURITY;

--
-- Name: announcement; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.announcement ENABLE ROW LEVEL SECURITY;

--
-- Name: announcement_attachment; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.announcement_attachment ENABLE ROW LEVEL SECURITY;

--
-- Name: announcement_audience; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.announcement_audience ENABLE ROW LEVEL SECURITY;

--
-- Name: article_attachment; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.article_attachment ENABLE ROW LEVEL SECURITY;

--
-- Name: cohort; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.cohort ENABLE ROW LEVEL SECURITY;

--
-- Name: cohort_group; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.cohort_group ENABLE ROW LEVEL SECURITY;

--
-- Name: cohort_membership; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.cohort_membership ENABLE ROW LEVEL SECURITY;

--
-- Name: cohort_subscription; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.cohort_subscription ENABLE ROW LEVEL SECURITY;

--
-- Name: couple; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.couple ENABLE ROW LEVEL SECURITY;

--
-- Name: access_credential current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.access_credential AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: access_event current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.access_event AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));


--
-- Name: account current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.account AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: accounting_period current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.accounting_period AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: aktuality current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.aktuality AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: announcement current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.announcement AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: announcement_attachment current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.announcement_attachment AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: announcement_audience current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.announcement_audience AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: article_attachment current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.article_attachment AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: cohort current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.cohort AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: cohort_group current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.cohort_group AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: cohort_subscription current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.cohort_subscription AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: dokumenty current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.dokumenty AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: event current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.event AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: event_lesson_request current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.event_lesson_request AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: event_registration current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.event_registration AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: event_series current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.event_series AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: event_target_cohort current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.event_target_cohort AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: event_trainer current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.event_trainer AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: form_responses current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.form_responses AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: location current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.location AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: location_image current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.location_image AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: membership_application current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.membership_application AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: payment current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.payment AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: payment_debtor current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.payment_debtor AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: payment_recipient current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.payment_recipient AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: person_invitation current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.person_invitation AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: posting current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.posting AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: scoreboard_manual_adjustment current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.scoreboard_manual_adjustment AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: security_event current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.security_event AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: transaction current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.transaction AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: event_registration delete_my; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY delete_my ON public.event_registration FOR DELETE USING (((person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])) OR (couple_id = ANY (( SELECT public.current_couple_ids() AS current_couple_ids)::bigint[]))));


--
-- Name: membership_application delete_my; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY delete_my ON public.membership_application FOR DELETE USING (((created_by = ( SELECT public.current_user_id() AS current_user_id)) AND (status = ANY (ARRAY['new'::public.application_form_status, 'sent'::public.application_form_status]))));


--
-- Name: dokumenty; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.dokumenty ENABLE ROW LEVEL SECURITY;

--
-- Name: event; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event ENABLE ROW LEVEL SECURITY;

--
-- Name: event event_access_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY event_access_view ON public.event FOR SELECT TO anonymous USING ((id IN ( SELECT app_private.accessible_event_ids() AS accessible_event_ids)));


--
-- Name: event_target_cohort event_access_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY event_access_view ON public.event_target_cohort FOR SELECT TO anonymous USING ((event_id IN ( SELECT app_private.accessible_event_ids() AS accessible_event_ids)));


--
-- Name: event_trainer event_access_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY event_access_view ON public.event_trainer FOR SELECT TO anonymous USING ((event_id IN ( SELECT app_private.accessible_event_ids() AS accessible_event_ids)));


--
-- Name: event_lesson_request; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event_lesson_request ENABLE ROW LEVEL SECURITY;

--
-- Name: event_registration; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event_registration ENABLE ROW LEVEL SECURITY;

--
-- Name: event_series; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event_series ENABLE ROW LEVEL SECURITY;

--
-- Name: couple event_share_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY event_share_view ON public.couple FOR SELECT TO anonymous USING ((id IN ( SELECT event_registration.couple_id
   FROM public.event_registration
  WHERE ((event_registration.registration_status = 'active'::public.event_registration_status) AND (event_registration.parent_registration_id IS NULL) AND (event_registration.couple_id IS NOT NULL) AND (event_registration.event_id = ANY ((( SELECT current_setting('jwt.claims.shared.event_ids'::text, true) AS current_setting))::bigint[]))))));


--
-- Name: person event_share_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY event_share_view ON public.person FOR SELECT TO anonymous USING ((id IN ( SELECT event_registration.person_id
   FROM public.event_registration
  WHERE ((event_registration.registration_status = 'active'::public.event_registration_status) AND (event_registration.person_id IS NOT NULL) AND (event_registration.event_id = ANY ((( SELECT current_setting('jwt.claims.shared.event_ids'::text, true) AS current_setting))::bigint[])))
UNION ALL
 SELECT event_trainer.person_id
   FROM public.event_trainer
  WHERE (event_trainer.event_id = ANY ((( SELECT current_setting('jwt.claims.shared.event_ids'::text, true) AS current_setting))::bigint[])))));


--
-- Name: event_target_cohort; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event_target_cohort ENABLE ROW LEVEL SECURITY;

--
-- Name: event_trainer; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event_trainer ENABLE ROW LEVEL SECURITY;

--
-- Name: file; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.file ENABLE ROW LEVEL SECURITY;

--
-- Name: form_responses; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.form_responses ENABLE ROW LEVEL SECURITY;

--
-- Name: event_registration insert_my; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY insert_my ON public.event_registration FOR INSERT WITH CHECK (((person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])) OR (couple_id = ANY (( SELECT public.current_couple_ids() AS current_couple_ids)::bigint[]))));


--
-- Name: membership_application insert_my; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY insert_my ON public.membership_application FOR INSERT WITH CHECK (((created_by = ( SELECT public.current_user_id() AS current_user_id)) AND (status = ANY (ARRAY['new'::public.application_form_status, 'sent'::public.application_form_status]))));


--
-- Name: location; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.location ENABLE ROW LEVEL SECURITY;

--
-- Name: location_image; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.location_image ENABLE ROW LEVEL SECURITY;

--
-- Name: membership_application manage_admin; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_admin ON public.membership_application TO administrator USING (true);


--
-- Name: users manage_own; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_own ON public.users USING ((id = ( SELECT public.current_user_id() AS current_user_id)));


--
-- Name: scoreboard_manual_adjustment member_read; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_read ON public.scoreboard_manual_adjustment FOR SELECT USING (true);


--
-- Name: account member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.account FOR SELECT TO member USING ((person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])));


--
-- Name: announcement member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.announcement FOR SELECT TO member USING ((id IN ( SELECT app_private.visible_announcement_ids() AS visible_announcement_ids)));


--
-- Name: announcement_attachment member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.announcement_attachment FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.announcement a
  WHERE (a.id = announcement_attachment.announcement_id))));


--
-- Name: announcement_audience member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.announcement_audience FOR SELECT TO member USING (true);


--
-- Name: cohort_subscription member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.cohort_subscription FOR SELECT TO member USING (true);


--
-- Name: dokumenty member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.dokumenty FOR SELECT TO member USING (true);


--
-- Name: event member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.event FOR SELECT TO member USING (is_visible);


--
-- Name: event_series member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.event_series FOR SELECT TO member USING ((EXISTS ( SELECT 1
   FROM public.event i
  WHERE (i.series_id = event_series.id))));


--
-- Name: event_target_cohort member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.event_target_cohort FOR SELECT TO member USING ((EXISTS ( SELECT 1
   FROM public.event e
  WHERE (e.id = event_target_cohort.event_id))));


--
-- Name: event_trainer member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.event_trainer FOR SELECT TO member USING (true);


--
-- Name: payment member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.payment FOR SELECT TO member USING ((id = ANY (ARRAY( SELECT app_private.visible_payment_ids() AS visible_payment_ids))));


--
-- Name: payment_debtor member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.payment_debtor FOR SELECT TO member USING ((person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])));


--
-- Name: payment_recipient member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.payment_recipient FOR SELECT TO member USING ((account_id = ANY (ARRAY( SELECT public.current_account_ids() AS current_account_ids))));


--
-- Name: posting member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.posting FOR SELECT TO member USING ((account_id = ANY (ARRAY( SELECT public.current_account_ids() AS current_account_ids))));


--
-- Name: transaction member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.transaction FOR SELECT TO member USING (((id = ANY (ARRAY( SELECT posting.transaction_id
   FROM public.posting))) OR (payment_id = ANY (ARRAY( SELECT app_private.visible_payment_ids() AS visible_payment_ids)))));


--
-- Name: membership_application; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.membership_application ENABLE ROW LEVEL SECURITY;

--
-- Name: cohort_membership my_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_tenant ON public.cohort_membership AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: access_event my_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY my_view ON public.access_event FOR SELECT USING ((person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])));


--
-- Name: otp_token; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.otp_token ENABLE ROW LEVEL SECURITY;

--
-- Name: payment; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.payment ENABLE ROW LEVEL SECURITY;

--
-- Name: payment_debtor; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.payment_debtor ENABLE ROW LEVEL SECURITY;

--
-- Name: payment_recipient; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.payment_recipient ENABLE ROW LEVEL SECURITY;

--
-- Name: person; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.person ENABLE ROW LEVEL SECURITY;

--
-- Name: person_invitation; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.person_invitation ENABLE ROW LEVEL SECURITY;

--
-- Name: posting; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.posting ENABLE ROW LEVEL SECURITY;

--
-- Name: accounting_period public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.accounting_period FOR SELECT USING (true);


--
-- Name: aktuality public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.aktuality FOR SELECT USING (is_visible);


--
-- Name: article_attachment public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.article_attachment FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.aktuality a
  WHERE (a.id = article_attachment.aktuality_id))));


--
-- Name: cohort public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.cohort FOR SELECT TO anonymous USING ((is_visible OR app_private.cohort_has_accessible_event(id)));


--
-- Name: cohort_group public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.cohort_group FOR SELECT USING (true);


--
-- Name: event public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.event FOR SELECT TO anonymous USING (is_public);


--
-- Name: event_series public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.event_series FOR SELECT TO anonymous USING ((EXISTS ( SELECT 1
   FROM public.event i
  WHERE (i.series_id = event_series.id))));


--
-- Name: event_trainer public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.event_trainer FOR SELECT TO anonymous USING ((event_id IN ( SELECT event.id
   FROM public.event
  WHERE event.is_public)));


--
-- Name: location public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.location FOR SELECT USING (true);


--
-- Name: location_image public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.location_image FOR SELECT USING (true);


--
-- Name: tenant public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant FOR SELECT TO anonymous USING ((id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: tenant_administrator public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant_administrator FOR SELECT USING (true);


--
-- Name: tenant_trainer public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant_trainer FOR SELECT USING (true);


--
-- Name: scoreboard_manual_adjustment; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.scoreboard_manual_adjustment ENABLE ROW LEVEL SECURITY;

--
-- Name: security_event; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.security_event ENABLE ROW LEVEL SECURITY;

--
-- Name: security_event self_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY self_view ON public.security_event FOR SELECT TO member USING (((user_id = ( SELECT public.current_user_id() AS current_user_id)) OR (person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[]))));


--
-- Name: tenant system_admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY system_admin_all ON public.tenant TO system_admin USING (true);


--
-- Name: tenant_administrator system_admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY system_admin_all ON public.tenant_administrator TO system_admin USING (true);


--
-- Name: tenant_membership system_admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY system_admin_all ON public.tenant_membership TO system_admin USING (true);


--
-- Name: tenant_settings system_admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY system_admin_all ON public.tenant_settings TO system_admin USING (true);


--
-- Name: tenant_trainer system_admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY system_admin_all ON public.tenant_trainer TO system_admin USING (true);


--
-- Name: tenant; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant ENABLE ROW LEVEL SECURITY;

--
-- Name: tenant_administrator; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant_administrator ENABLE ROW LEVEL SECURITY;

--
-- Name: tenant_membership; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant_membership ENABLE ROW LEVEL SECURITY;

--
-- Name: tenant_settings; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant_settings ENABLE ROW LEVEL SECURITY;

--
-- Name: tenant_trainer; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.tenant_trainer ENABLE ROW LEVEL SECURITY;

--
-- Name: event_series trainer_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_all ON public.event_series TO trainer USING (true);


--
-- Name: event trainer_delete; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_delete ON public.event FOR DELETE TO trainer USING (app_private.can_trainer_edit_event(id));


--
-- Name: event trainer_insert; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_insert ON public.event FOR INSERT TO trainer WITH CHECK (((parent_id IS NULL) OR app_private.can_trainer_edit_event(parent_id)));


--
-- Name: event_registration trainer_insert; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_insert ON public.event_registration FOR INSERT TO trainer WITH CHECK (app_private.can_trainer_edit_event(event_id));


--
-- Name: announcement_attachment trainer_manage; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_manage ON public.announcement_attachment TO trainer USING ((EXISTS ( SELECT 1
   FROM public.announcement a
  WHERE ((a.id = announcement_attachment.announcement_id) AND (a.author_id = ( SELECT public.current_user_id() AS current_user_id))))));


--
-- Name: announcement_audience trainer_manage; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_manage ON public.announcement_audience TO trainer USING ((announcement_id IN ( SELECT announcement.id
   FROM public.announcement
  WHERE (announcement.author_id = ( SELECT public.current_user_id() AS current_user_id)))));


--
-- Name: announcement trainer_manage_own; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_manage_own ON public.announcement TO trainer USING ((author_id = ( SELECT public.current_user_id() AS current_user_id)));


--
-- Name: event_trainer trainer_same_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_same_tenant ON public.event_trainer TO trainer USING (app_private.can_trainer_edit_event(event_id));


--
-- Name: event trainer_select; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_select ON public.event FOR SELECT TO trainer USING (((cardinality(manager_person_ids) = 0) OR (manager_person_ids && ( SELECT public.current_person_ids() AS current_person_ids))));


--
-- Name: event_target_cohort trainer_select; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_select ON public.event_target_cohort TO trainer USING (app_private.can_trainer_edit_event(event_id));


--
-- Name: event trainer_update; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_update ON public.event FOR UPDATE TO trainer USING (app_private.can_trainer_edit_event(id));


--
-- Name: event_registration trainer_update; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_update ON public.event_registration FOR UPDATE TO trainer USING (app_private.can_trainer_edit_event(event_id));


--
-- Name: transaction; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.transaction ENABLE ROW LEVEL SECURITY;

--
-- Name: membership_application update_my; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY update_my ON public.membership_application FOR UPDATE USING (((created_by = ( SELECT public.current_user_id() AS current_user_id)) AND (status = ANY (ARRAY['new'::public.application_form_status, 'sent'::public.application_form_status])))) WITH CHECK (((created_by = ( SELECT public.current_user_id() AS current_user_id)) AND (status = ANY (ARRAY['new'::public.application_form_status, 'sent'::public.application_form_status]))));


--
-- Name: file uploader_manage; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY uploader_manage ON public.file USING (((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (uploaded_by = ( SELECT public.current_user_id() AS current_user_id))));


--
-- Name: user_proxy; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.user_proxy ENABLE ROW LEVEL SECURITY;

--
-- Name: users; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.users ENABLE ROW LEVEL SECURITY;

--
-- Name: person view_active_couple_partner; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_active_couple_partner ON public.person FOR SELECT USING ((id IN ( SELECT unnest(ARRAY[c.man_id, c.woman_id]) AS unnest
   FROM public.couple c
  WHERE (c.status = 'active'::public.relationship_status))));


--
-- Name: cohort_membership view_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_all ON public.cohort_membership FOR SELECT USING (true);


--
-- Name: membership_application view_my; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_my ON public.membership_application FOR SELECT USING ((created_by = ( SELECT public.current_user_id() AS current_user_id)));


--
-- Name: user_proxy view_personal; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_personal ON public.user_proxy FOR SELECT USING (((user_id = ( SELECT public.current_user_id() AS current_user_id)) OR (id = ANY (ARRAY( SELECT app_private.visible_user_proxy_ids() AS visible_user_proxy_ids)))));


--
-- Name: users view_shared_person; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_shared_person ON public.users FOR SELECT USING ((id = ANY (ARRAY( SELECT user_proxy.user_id
   FROM public.user_proxy
  WHERE (user_proxy.status = 'active'::public.relationship_status)))));


--
-- Name: person view_tenant_or_trainer; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_tenant_or_trainer ON public.person FOR SELECT USING ((id IN ( SELECT app_private.visible_person_ids() AS visible_person_ids)));


--
-- Name: event_lesson_request view_visible_instance; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_instance ON public.event_lesson_request FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event_registration r
  WHERE (r.id = event_lesson_request.registration_id))));


--
-- Name: event_registration view_visible_instance; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_instance ON public.event_registration FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event e
  WHERE (e.id = event_registration.event_id))));


--
-- Name: couple view_visible_person; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_person ON public.couple FOR SELECT USING (((man_id IN ( SELECT app_private.visible_person_ids() AS visible_person_ids)) OR (woman_id IN ( SELECT app_private.visible_person_ids() AS visible_person_ids))));


--
-- Name: tenant_membership view_visible_person; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_person ON public.tenant_membership FOR SELECT USING (true);


--
-- Name: file visible; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY visible ON public.file FOR SELECT USING ((((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)) AND (id IN ( SELECT app_private.visible_file_ids() AS visible_file_ids))) OR (id IN ( SELECT person.image_id
   FROM public.person
  WHERE (person.image_id IS NOT NULL)
UNION ALL
 SELECT couple.image_id
   FROM public.couple
  WHERE (couple.image_id IS NOT NULL)
UNION ALL
 SELECT cohort.image_id
   FROM public.cohort
  WHERE (cohort.image_id IS NOT NULL)))));


--
-- Name: SCHEMA app_private; Type: ACL; Schema: -; Owner: -
--

GRANT ALL ON SCHEMA app_private TO postgres;
GRANT USAGE ON SCHEMA app_private TO anonymous;


--
-- Name: SCHEMA crawler; Type: ACL; Schema: -; Owner: -
--

GRANT USAGE ON SCHEMA crawler TO system_admin;


--
-- Name: SCHEMA federated; Type: ACL; Schema: -; Owner: -
--

GRANT USAGE ON SCHEMA federated TO anonymous;


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: -
--

REVOKE USAGE ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO anonymous;


--
-- Name: SCHEMA wdsf; Type: ACL; Schema: -; Owner: -
--

GRANT ALL ON SCHEMA wdsf TO postgres;


--
-- Name: TYPE event_status; Type: ACL; Schema: federated; Owner: -
--

GRANT ALL ON TYPE federated.event_status TO anonymous;


--
-- Name: TABLE category; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.category TO anonymous;


--
-- Name: FUNCTION current_tenant_id(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_tenant_id() TO anonymous;


--
-- Name: TABLE users; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.users TO anonymous;


--
-- Name: COLUMN users.id; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(id) ON TABLE public.users TO anonymous;


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
-- Name: FUNCTION accessible_event_ids(); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.accessible_event_ids() TO anonymous;


--
-- Name: FUNCTION announcement_status_next(ts timestamp with time zone, scheduled_since timestamp with time zone, scheduled_until timestamp with time zone, current_status public.announcement_status); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.announcement_status_next(ts timestamp with time zone, scheduled_since timestamp with time zone, scheduled_until timestamp with time zone, current_status public.announcement_status) TO anonymous;


--
-- Name: FUNCTION can_trainer_edit_event(p_event_id bigint); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.can_trainer_edit_event(p_event_id bigint) TO anonymous;


--
-- Name: FUNCTION cohort_has_accessible_event(p_cohort_id bigint); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.cohort_has_accessible_event(p_cohort_id bigint) TO anonymous;


--
-- Name: FUNCTION competitor_category_candidates(age_groups text[], classes text[], disciplines text[]); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.competitor_category_candidates(age_groups text[], classes text[], disciplines text[]) TO anonymous;


--
-- Name: TABLE payment; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.payment TO anonymous;


--
-- Name: FUNCTION ensure_couple_person_registrations(p_registration_id bigint); Type: ACL; Schema: app_private; Owner: -
--

REVOKE ALL ON FUNCTION app_private.ensure_couple_person_registrations(p_registration_id bigint) FROM PUBLIC;
GRANT ALL ON FUNCTION app_private.ensure_couple_person_registrations(p_registration_id bigint) TO trainer;
GRANT ALL ON FUNCTION app_private.ensure_couple_person_registrations(p_registration_id bigint) TO administrator;


--
-- Name: FUNCTION event_share_claims(p_tenant_id bigint, p_share text); Type: ACL; Schema: app_private; Owner: -
--

REVOKE ALL ON FUNCTION app_private.event_share_claims(p_tenant_id bigint, p_share text) FROM PUBLIC;


--
-- Name: TABLE event; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event TO anonymous;


--
-- Name: TABLE tenant_trainer; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant_trainer TO anonymous;


--
-- Name: FUNCTION event_trainers_at(v_instance public.event, v_at timestamp with time zone); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.event_trainers_at(v_instance public.event, v_at timestamp with time zone) TO anonymous;


--
-- Name: FUNCTION normalize_name(text); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.normalize_name(text) TO anonymous;


--
-- Name: FUNCTION person_membership_candidates(membership_state text); Type: ACL; Schema: app_private; Owner: -
--

REVOKE ALL ON FUNCTION app_private.person_membership_candidates(membership_state text) FROM PUBLIC;
GRANT ALL ON FUNCTION app_private.person_membership_candidates(membership_state text) TO anonymous;


--
-- Name: FUNCTION queue_announcement_notifications(in_announcement_id bigint); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.queue_announcement_notifications(in_announcement_id bigint) TO anonymous;


--
-- Name: FUNCTION reconcile_event_cohort_registrations(p_event_ids bigint[], p_person_ids bigint[]); Type: ACL; Schema: app_private; Owner: -
--

REVOKE ALL ON FUNCTION app_private.reconcile_event_cohort_registrations(p_event_ids bigint[], p_person_ids bigint[]) FROM PUBLIC;
GRANT ALL ON FUNCTION app_private.reconcile_event_cohort_registrations(p_event_ids bigint[], p_person_ids bigint[]) TO trainer;
GRANT ALL ON FUNCTION app_private.reconcile_event_cohort_registrations(p_event_ids bigint[], p_person_ids bigint[]) TO administrator;


--
-- Name: FUNCTION refresh_event_manager_person_ids(p_event_id bigint); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.refresh_event_manager_person_ids(p_event_id bigint) TO anonymous;


--
-- Name: FUNCTION relationship_status_next(ts timestamp with time zone, range tstzrange, current public.relationship_status); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.relationship_status_next(ts timestamp with time zone, range tstzrange, current public.relationship_status) TO administrator;


--
-- Name: FUNCTION current_user_id(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_user_id() TO anonymous;


--
-- Name: TABLE event_lesson_request; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_lesson_request TO anonymous;


--
-- Name: TABLE event_registration; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_registration TO anonymous;


--
-- Name: FUNCTION tg_cohort_membership__on_status(); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.tg_cohort_membership__on_status() TO trainer;


--
-- Name: FUNCTION tg_event_registration__attendance_timestamps(); Type: ACL; Schema: app_private; Owner: -
--

REVOKE ALL ON FUNCTION app_private.tg_event_registration__attendance_timestamps() FROM PUBLIC;


--
-- Name: FUNCTION tg_event_registration__refresh_stats_stmt(); Type: ACL; Schema: app_private; Owner: -
--

REVOKE ALL ON FUNCTION app_private.tg_event_registration__refresh_stats_stmt() FROM PUBLIC;


--
-- Name: FUNCTION tg_resolve_age_group(); Type: ACL; Schema: app_private; Owner: -
--

REVOKE ALL ON FUNCTION app_private.tg_resolve_age_group() FROM PUBLIC;


--
-- Name: FUNCTION tstzrange_overlaps_internal(tstzrange, anyrange); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.tstzrange_overlaps_internal(tstzrange, anyrange) TO anonymous;


--
-- Name: FUNCTION tstzrange_overlaps(tstzrange, tstzrange); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.tstzrange_overlaps(tstzrange, tstzrange) TO anonymous;


--
-- Name: FUNCTION visible_announcement_ids(); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.visible_announcement_ids() TO anonymous;


--
-- Name: FUNCTION visible_file_ids(); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.visible_file_ids() TO anonymous;


--
-- Name: FUNCTION visible_payment_ids(); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.visible_payment_ids() TO anonymous;


--
-- Name: FUNCTION visible_person_ids(); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.visible_person_ids() TO anonymous;


--
-- Name: FUNCTION visible_user_proxy_ids(); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.visible_user_proxy_ids() TO anonymous;


--
-- Name: FUNCTION reserve_request(in_host text, OUT granted boolean, OUT allowed_at timestamp with time zone); Type: ACL; Schema: crawler; Owner: -
--

REVOKE ALL ON FUNCTION crawler.reserve_request(in_host text, OUT granted boolean, OUT allowed_at timestamp with time zone) FROM PUBLIC;


--
-- Name: FUNCTION store_response(p_frontier_id bigint, p_url text, p_http_status integer, p_error text, p_content jsonb, p_fetch_status crawler.fetch_status); Type: ACL; Schema: crawler; Owner: -
--

REVOKE ALL ON FUNCTION crawler.store_response(p_frontier_id bigint, p_url text, p_http_status integer, p_error text, p_content jsonb, p_fetch_status crawler.fetch_status) FROM PUBLIC;


--
-- Name: FUNCTION class_rank(class text); Type: ACL; Schema: federated; Owner: -
--

GRANT ALL ON FUNCTION federated.class_rank(class text) TO anonymous;


--
-- Name: TABLE federation; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.federation TO anonymous;


--
-- Name: FUNCTION federation_competitor_facets(federation federated.federation, search text, competitor_types federated.competitor_type[], age_groups text[], classes text[], disciplines text[], competed_since date); Type: ACL; Schema: federated; Owner: -
--

REVOKE ALL ON FUNCTION federated.federation_competitor_facets(federation federated.federation, search text, competitor_types federated.competitor_type[], age_groups text[], classes text[], disciplines text[], competed_since date) FROM PUBLIC;
GRANT ALL ON FUNCTION federated.federation_competitor_facets(federation federated.federation, search text, competitor_types federated.competitor_type[], age_groups text[], classes text[], disciplines text[], competed_since date) TO anonymous;


--
-- Name: FUNCTION federation_event_facets(federation federated.federation, include_cancelled boolean); Type: ACL; Schema: federated; Owner: -
--

REVOKE ALL ON FUNCTION federated.federation_event_facets(federation federated.federation, include_cancelled boolean) FROM PUBLIC;
GRANT ALL ON FUNCTION federated.federation_event_facets(federation federated.federation, include_cancelled boolean) TO anonymous;


--
-- Name: FUNCTION federation_people_facets(federation federated.federation, search text, age_groups text[], genders text[], nationalities text[]); Type: ACL; Schema: federated; Owner: -
--

REVOKE ALL ON FUNCTION federated.federation_people_facets(federation federated.federation, search text, age_groups text[], genders text[], nationalities text[]) FROM PUBLIC;
GRANT ALL ON FUNCTION federated.federation_people_facets(federation federated.federation, search text, age_groups text[], genders text[], nationalities text[]) TO anonymous;


--
-- Name: FUNCTION resolve_age_group(source_name text); Type: ACL; Schema: federated; Owner: -
--

REVOKE ALL ON FUNCTION federated.resolve_age_group(source_name text) FROM PUBLIC;
GRANT ALL ON FUNCTION federated.resolve_age_group(source_name text) TO anonymous;


--
-- Name: TABLE access_credential; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.access_credential TO anonymous;


--
-- Name: FUNCTION access_credential_is_allowed(c public.access_credential); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.access_credential_is_allowed(c public.access_credential) TO anonymous;


--
-- Name: FUNCTION access_credential_last_used(c public.access_credential); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.access_credential_last_used(c public.access_credential) TO anonymous;


--
-- Name: TABLE account; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.account TO anonymous;


--
-- Name: TABLE posting; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.posting TO anonymous;


--
-- Name: TABLE transaction; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.transaction TO anonymous;


--
-- Name: FUNCTION account_assets(a public.account, since timestamp with time zone, until timestamp with time zone); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.account_assets(a public.account, since timestamp with time zone, until timestamp with time zone) TO anonymous;


--
-- Name: FUNCTION account_balance(a public.account); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.account_balance(a public.account) TO anonymous;


--
-- Name: FUNCTION account_liabilities(a public.account, since timestamp with time zone, until timestamp with time zone); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.account_liabilities(a public.account, since timestamp with time zone, until timestamp with time zone) TO anonymous;


--
-- Name: TABLE activity_timeline_item; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT ON TABLE public.activity_timeline_item TO anonymous;


--
-- Name: FUNCTION activity_timeline(p_since timestamp with time zone, p_until timestamp with time zone, p_person_ids bigint[], p_cohort_id bigint, p_kinds public.activity_timeline_kind[], p_event_types public.event_type[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.activity_timeline(p_since timestamp with time zone, p_until timestamp with time zone, p_person_ids bigint[], p_cohort_id bigint, p_kinds public.activity_timeline_kind[], p_event_types public.event_type[]) TO anonymous;


--
-- Name: TABLE announcement; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.announcement TO anonymous;


--
-- Name: FUNCTION announcement_author_name(a public.announcement); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.announcement_author_name(a public.announcement) TO anonymous;


--
-- Name: TABLE cohort; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.cohort TO anonymous;


--
-- Name: FUNCTION archive_cohort(cohort_id bigint); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.archive_cohort(cohort_id bigint) TO administrator;


--
-- Name: FUNCTION change_password(new_pass text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.change_password(new_pass text) TO anonymous;


--
-- Name: FUNCTION competition_brief(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.competition_brief(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) TO anonymous;


--
-- Name: FUNCTION competition_report(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.competition_report(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) TO anonymous;


--
-- Name: FUNCTION immutable_concat_ws(text, VARIADIC text[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.immutable_concat_ws(text, VARIADIC text[]) TO anonymous;


--
-- Name: TABLE competition; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.competition TO anonymous;


--
-- Name: TABLE couple; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.couple TO anonymous;


--
-- Name: TABLE person; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.person TO anonymous;


--
-- Name: FUNCTION competition_suggestions(event_ids bigint[], since date, until date, person_ids bigint[], couple_id bigint, cohort_id bigint, registered_only boolean); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.competition_suggestions(event_ids bigint[], since date, until date, person_ids bigint[], couple_id bigint, cohort_id bigint, registered_only boolean) TO anonymous;


--
-- Name: FUNCTION confirm_membership_application(application_id bigint, is_member boolean, is_trainer boolean, is_admin boolean, join_date timestamp with time zone, cohort_ids bigint[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.confirm_membership_application(application_id bigint, is_member boolean, is_trainer boolean, is_admin boolean, join_date timestamp with time zone, cohort_ids bigint[]) TO administrator;


--
-- Name: FUNCTION couple_age_group(c public.couple); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.couple_age_group(c public.couple) TO anonymous;


--
-- Name: FUNCTION create_credit_transaction_for_person(v_person_id bigint, v_description text, v_amount numeric, v_currency text, v_date timestamp with time zone); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.create_credit_transaction_for_person(v_person_id bigint, v_description text, v_amount numeric, v_currency text, v_date timestamp with time zone) TO anonymous;


--
-- Name: FUNCTION create_event_payment(target_event public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.create_event_payment(target_event public.event) TO anonymous;


--
-- Name: TABLE cohort_subscription; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.cohort_subscription TO anonymous;


--
-- Name: FUNCTION create_missing_cohort_subscription_payments(c public.cohort_subscription); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.create_missing_cohort_subscription_payments(c public.cohort_subscription) TO anonymous;


--
-- Name: FUNCTION create_next_cohort_subscription_payment(c public.cohort_subscription); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.create_next_cohort_subscription_payment(c public.cohort_subscription) TO anonymous;


--
-- Name: FUNCTION create_person(person_id bigint, INOUT p public.person, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamp with time zone); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.create_person(person_id bigint, INOUT p public.person, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamp with time zone) TO anonymous;


--
-- Name: FUNCTION current_account_ids(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_account_ids() TO anonymous;


--
-- Name: FUNCTION current_claims(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_claims() TO anonymous;


--
-- Name: FUNCTION current_couple_ids(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_couple_ids() TO anonymous;


--
-- Name: FUNCTION current_person_ids(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.current_person_ids() TO anonymous;


--
-- Name: FUNCTION digest(bytea, text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.digest(bytea, text) TO anonymous;


--
-- Name: FUNCTION digest(text, text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.digest(text, text) TO anonymous;


--
-- Name: FUNCTION event_approx_price(v_instance public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_approx_price(v_instance public.event) TO anonymous;


--
-- Name: FUNCTION event_facets(start_range timestamp with time zone, end_range timestamp with time zone, only_types public.event_type[], trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[]); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.event_facets(start_range timestamp with time zone, end_range timestamp with time zone, only_types public.event_type[], trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[]) FROM PUBLIC;
GRANT ALL ON FUNCTION public.event_facets(start_range timestamp with time zone, end_range timestamp with time zone, only_types public.event_type[], trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[]) TO anonymous;


--
-- Name: FUNCTION event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[], only_types public.event_type[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[], only_types public.event_type[]) TO anonymous;


--
-- Name: FUNCTION event_overlaps_attendee_report(p_since timestamp with time zone, p_until timestamp with time zone); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_overlaps_attendee_report(p_since timestamp with time zone, p_until timestamp with time zone) TO anonymous;


--
-- Name: FUNCTION event_overlaps_trainer_report(p_since timestamp with time zone, p_until timestamp with time zone); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_overlaps_trainer_report(p_since timestamp with time zone, p_until timestamp with time zone) TO anonymous;


--
-- Name: FUNCTION event_registration_info(inst public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_registration_info(inst public.event) TO anonymous;


--
-- Name: FUNCTION event_registration_last_attended(reg public.event_registration); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_registration_last_attended(reg public.event_registration) TO anonymous;


--
-- Name: FUNCTION event_remaining_person_spots(inst public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_remaining_person_spots(inst public.event) TO anonymous;


--
-- Name: FUNCTION event_share_token(i public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_share_token(i public.event) TO anonymous;


--
-- Name: TABLE event_trainer; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_trainer TO anonymous;


--
-- Name: FUNCTION event_trainer_lessons_remaining(e public.event_trainer); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_trainer_lessons_remaining(e public.event_trainer) TO anonymous;


--
-- Name: FUNCTION event_trainer_name(trainer public.event_trainer); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_trainer_name(trainer public.event_trainer) TO anonymous;


--
-- Name: FUNCTION event_trainer_tenant_trainer(trainer public.event_trainer); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_trainer_tenant_trainer(trainer public.event_trainer) TO anonymous;


--
-- Name: FUNCTION event_trainers(v_instance public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_trainers(v_instance public.event) TO anonymous;


--
-- Name: FUNCTION federation_person_candidates(first_name text, last_name text, federation text, gender public.gender_type, birth_date date, "limit" integer, threshold real); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.federation_person_candidates(first_name text, last_name text, federation text, gender public.gender_type, birth_date date, "limit" integer, threshold real) TO anonymous;


--
-- Name: TABLE tenant; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT ON TABLE public.tenant TO anonymous;
GRANT ALL ON TABLE public.tenant TO system_admin;


--
-- Name: COLUMN tenant.name; Type: ACL; Schema: public; Owner: -
--

GRANT UPDATE(name) ON TABLE public.tenant TO administrator;


--
-- Name: COLUMN tenant.cz_ico; Type: ACL; Schema: public; Owner: -
--

GRANT UPDATE(cz_ico) ON TABLE public.tenant TO administrator;


--
-- Name: COLUMN tenant.cz_dic; Type: ACL; Schema: public; Owner: -
--

GRANT UPDATE(cz_dic) ON TABLE public.tenant TO administrator;


--
-- Name: COLUMN tenant.address; Type: ACL; Schema: public; Owner: -
--

GRANT UPDATE(address) ON TABLE public.tenant TO administrator;


--
-- Name: COLUMN tenant.description; Type: ACL; Schema: public; Owner: -
--

GRANT UPDATE(description) ON TABLE public.tenant TO administrator;


--
-- Name: COLUMN tenant.bank_account; Type: ACL; Schema: public; Owner: -
--

GRANT UPDATE(bank_account) ON TABLE public.tenant TO administrator;


--
-- Name: FUNCTION get_current_tenant(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.get_current_tenant() TO anonymous;


--
-- Name: FUNCTION get_current_user(version_id text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.get_current_user(version_id text) TO anonymous;


--
-- Name: FUNCTION invitation_info(token uuid); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.invitation_info(token uuid) TO anonymous;


--
-- Name: FUNCTION invitation_name(token uuid); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.invitation_name(token uuid) TO anonymous;


--
-- Name: FUNCTION log_in_as(id bigint); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.log_in_as(id bigint) FROM PUBLIC;
GRANT ALL ON FUNCTION public.log_in_as(id bigint) TO administrator;


--
-- Name: FUNCTION login(login text, passwd text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.login(login text, passwd text) TO anonymous;


--
-- Name: FUNCTION move_event(id bigint, since timestamp with time zone, until timestamp with time zone, trainer_person_id bigint, location_id bigint, location_text text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.move_event(id bigint, since timestamp with time zone, until timestamp with time zone, trainer_person_id bigint, location_id bigint, location_text text) TO anonymous;


--
-- Name: FUNCTION my_tenants_array(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.my_tenants_array() TO anonymous;


--
-- Name: FUNCTION otp_login(token uuid); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.otp_login(token uuid) TO anonymous;


--
-- Name: TABLE payment_debtor; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.payment_debtor TO anonymous;


--
-- Name: FUNCTION payment_debtor_is_unpaid(p public.payment_debtor); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.payment_debtor_is_unpaid(p public.payment_debtor) TO anonymous;


--
-- Name: FUNCTION payment_debtor_price(p public.payment_debtor, OUT amount numeric, OUT currency text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.payment_debtor_price(p public.payment_debtor, OUT amount numeric, OUT currency text) TO anonymous;


--
-- Name: FUNCTION people_without_access_or_invitation(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.people_without_access_or_invitation() TO anonymous;


--
-- Name: FUNCTION people_without_access_with_existing_account(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.people_without_access_with_existing_account() TO anonymous;


--
-- Name: FUNCTION people_without_access_with_invitation(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.people_without_access_with_invitation() TO anonymous;


--
-- Name: FUNCTION person_account(p_id bigint, in_currency text, OUT acc public.account); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_account(p_id bigint, in_currency text, OUT acc public.account) TO anonymous;


--
-- Name: FUNCTION person_active_couples(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_active_couples(p public.person) TO anonymous;


--
-- Name: FUNCTION person_age_group(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_age_group(p public.person) TO anonymous;


--
-- Name: FUNCTION person_all_couples(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_all_couples(p public.person) TO anonymous;


--
-- Name: FUNCTION person_cohort_ids(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_cohort_ids(p public.person) TO anonymous;


--
-- Name: FUNCTION person_facets(membership_states text[], in_cohorts bigint[], is_trainer boolean, is_admin boolean); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.person_facets(membership_states text[], in_cohorts bigint[], is_trainer boolean, is_admin boolean) FROM PUBLIC;
GRANT ALL ON FUNCTION public.person_facets(membership_states text[], in_cohorts bigint[], is_trainer boolean, is_admin boolean) TO anonymous;


--
-- Name: FUNCTION person_federation_candidates(in_person public.person, federation text, "limit" integer, threshold real); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_federation_candidates(in_person public.person, federation text, "limit" integer, threshold real) TO anonymous;


--
-- Name: FUNCTION person_has_access(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_has_access(p public.person) TO anonymous;


--
-- Name: FUNCTION person_is_admin(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_is_admin(p public.person) TO anonymous;


--
-- Name: FUNCTION person_is_member(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_is_member(p public.person) TO anonymous;


--
-- Name: FUNCTION person_is_trainer(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_is_trainer(p public.person) TO anonymous;


--
-- Name: FUNCTION refresh_jwt(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.refresh_jwt() TO anonymous;


--
-- Name: FUNCTION register_using_invitation(email text, passwd text, token uuid, login text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.register_using_invitation(email text, passwd text, token uuid, login text) TO anonymous;


--
-- Name: FUNCTION register_without_invitation(email text, passwd text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.register_without_invitation(email text, passwd text) TO anonymous;


--
-- Name: TABLE membership_application; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.membership_application TO anonymous;
GRANT ALL ON TABLE public.membership_application TO administrator;


--
-- Name: COLUMN membership_application.first_name; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(first_name),UPDATE(first_name) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.middle_name; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(middle_name),UPDATE(middle_name) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.last_name; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(last_name),UPDATE(last_name) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.gender; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(gender),UPDATE(gender) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.birth_date; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(birth_date),UPDATE(birth_date) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.nationality; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(nationality),UPDATE(nationality) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.tax_identification_number; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(tax_identification_number),UPDATE(tax_identification_number) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.national_id_number; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(national_id_number),UPDATE(national_id_number) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.csts_id; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(csts_id),UPDATE(csts_id) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.wdsf_id; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(wdsf_id),UPDATE(wdsf_id) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.prefix_title; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(prefix_title),UPDATE(prefix_title) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.suffix_title; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(suffix_title),UPDATE(suffix_title) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.bio; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(bio),UPDATE(bio) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.email; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(email),UPDATE(email) ON TABLE public.membership_application TO anonymous;


--
-- Name: COLUMN membership_application.phone; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(phone),UPDATE(phone) ON TABLE public.membership_application TO anonymous;


--
-- Name: FUNCTION reset_password(email character varying); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.reset_password(email character varying) TO anonymous;


--
-- Name: FUNCTION resolve_payment_with_credit(p public.payment); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.resolve_payment_with_credit(p public.payment) TO anonymous;


--
-- Name: FUNCTION save_events(details public.event_details_input, events public.event_input[], trainers public.event_trainer_input[], cohort_ids bigint[], series public.event_series_input); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.save_events(details public.event_details_input, events public.event_input[], trainers public.event_trainer_input[], cohort_ids bigint[], series public.event_series_input) TO anonymous;


--
-- Name: FUNCTION scoreboard_entries(since date, until date, cohort_id bigint); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.scoreboard_entries(since date, until date, cohort_id bigint) TO anonymous;


--
-- Name: FUNCTION set_event_instance_registration(p_instance_id bigint, p_person_id bigint, p_couple_id bigint, p_is_registered boolean, p_note text, p_lesson_trainer_ids bigint[], p_lesson_counts integer[], p_person public.event_registration_person_input); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.set_event_instance_registration(p_instance_id bigint, p_person_id bigint, p_couple_id bigint, p_is_registered boolean, p_note text, p_lesson_trainer_ids bigint[], p_lesson_counts integer[], p_person public.event_registration_person_input) TO anonymous;


--
-- Name: FUNCTION set_event_registration(event_id bigint, person_id bigint, couple_id bigint, is_registered boolean, note text, lesson_trainer_ids bigint[], lesson_counts integer[], person public.event_registration_person_input); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.set_event_registration(event_id bigint, person_id bigint, couple_id bigint, is_registered boolean, note text, lesson_trainer_ids bigint[], lesson_counts integer[], person public.event_registration_person_input) TO anonymous;


--
-- Name: FUNCTION set_event_sharing(id bigint, p_enabled boolean); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.set_event_sharing(id bigint, p_enabled boolean) TO anonymous;


--
-- Name: FUNCTION set_lesson_demand(instance_registration_id bigint, instance_trainer_id bigint, lesson_count integer); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.set_lesson_demand(instance_registration_id bigint, instance_trainer_id bigint, lesson_count integer) TO anonymous;


--
-- Name: FUNCTION set_lesson_request(registration_id bigint, trainer_id bigint, lesson_count integer); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.set_lesson_request(registration_id bigint, trainer_id bigint, lesson_count integer) TO anonymous;


--
-- Name: FUNCTION submit_form(type text, data jsonb, url text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.submit_form(type text, data jsonb, url text) TO anonymous;


--
-- Name: FUNCTION suggested_competition_matches(since date, until date, person_ids bigint[], couple_id bigint, cohort_id bigint, event_ids bigint[], registered_only boolean); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.suggested_competition_matches(since date, until date, person_ids bigint[], couple_id bigint, cohort_id bigint, event_ids bigint[], registered_only boolean) TO anonymous;


--
-- Name: FUNCTION sync_cohort_memberships(person_id bigint, cohort_ids bigint[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.sync_cohort_memberships(person_id bigint, cohort_ids bigint[]) TO administrator;


--
-- Name: FUNCTION system_admin_tenants(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.system_admin_tenants() TO anonymous;


--
-- Name: FUNCTION system_admin_update_tenant(tenant_id bigint, name text, description text, bank_account text, origins text[], address public.address_domain, cz_ico text, cz_dic text, settings jsonb); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.system_admin_update_tenant(tenant_id bigint, name text, description text, bank_account text, origins text[], address public.address_domain, cz_ico text, cz_dic text, settings jsonb) TO anonymous;


--
-- Name: FUNCTION tenant_account(in_currency text, OUT acc public.account); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.tenant_account(in_currency text, OUT acc public.account) TO anonymous;


--
-- Name: TABLE tenant_administrator; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant_administrator TO anonymous;


--
-- Name: FUNCTION tenant_couples(t public.tenant); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.tenant_couples(t public.tenant) TO anonymous;


--
-- Name: TABLE tenant_membership; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant_membership TO anonymous;


--
-- Name: FUNCTION trainer_group_attendance_completion(since timestamp with time zone, until timestamp with time zone); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.trainer_group_attendance_completion(since timestamp with time zone, until timestamp with time zone) TO anonymous;


--
-- Name: TABLE tenant_settings; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant_settings TO anonymous;


--
-- Name: FUNCTION update_tenant_settings_key(path text[], new_value jsonb); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.update_tenant_settings_key(path text[], new_value jsonb) TO administrator;


--
-- Name: FUNCTION upsert_announcement(info public.announcement_type_input, audiences public.announcement_audience_type_input[], attachments bigint[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.upsert_announcement(info public.announcement_type_input, audiences public.announcement_audience_type_input[], attachments bigint[]) TO anonymous;


--
-- Name: TABLE aktuality; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.aktuality TO anonymous;


--
-- Name: FUNCTION upsert_article(info public.article_type_input, attachments bigint[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.upsert_article(info public.article_type_input, attachments bigint[]) TO anonymous;


--
-- Name: TABLE location; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.location TO anonymous;


--
-- Name: FUNCTION upsert_location(details public.location_details_input, image_ids bigint[], cover_image_id bigint); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.upsert_location(details public.location_details_input, image_ids bigint[], cover_image_id bigint) TO administrator;


--
-- Name: FUNCTION verify_function(f regproc, relid regclass); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION public.verify_function(f regproc, relid regclass) FROM PUBLIC;


--
-- Name: TABLE platby_category; Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON TABLE app_private.platby_category TO anonymous;


--
-- Name: TABLE platby_category_group; Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON TABLE app_private.platby_category_group TO anonymous;


--
-- Name: SEQUENCE platby_category_group_pcg_id_seq; Type: ACL; Schema: app_private; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE app_private.platby_category_group_pcg_id_seq TO anonymous;


--
-- Name: SEQUENCE platby_category_pc_id_seq; Type: ACL; Schema: app_private; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE app_private.platby_category_pc_id_seq TO anonymous;


--
-- Name: TABLE platby_group; Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON TABLE app_private.platby_group TO anonymous;


--
-- Name: SEQUENCE platby_group_pg_id_seq; Type: ACL; Schema: app_private; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE app_private.platby_group_pg_id_seq TO anonymous;


--
-- Name: TABLE platby_group_skupina; Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON TABLE app_private.platby_group_skupina TO anonymous;


--
-- Name: SEQUENCE platby_group_skupina_pgs_id_seq; Type: ACL; Schema: app_private; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE app_private.platby_group_skupina_pgs_id_seq TO anonymous;


--
-- Name: TABLE platby_item; Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON TABLE app_private.platby_item TO anonymous;


--
-- Name: SEQUENCE platby_item_pi_id_seq; Type: ACL; Schema: app_private; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE app_private.platby_item_pi_id_seq TO anonymous;


--
-- Name: TABLE platby_raw; Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON TABLE app_private.platby_raw TO anonymous;


--
-- Name: SEQUENCE platby_raw_pr_id_seq; Type: ACL; Schema: app_private; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE app_private.platby_raw_pr_id_seq TO anonymous;


--
-- Name: TABLE frontier; Type: ACL; Schema: crawler; Owner: -
--

GRANT SELECT ON TABLE crawler.frontier TO system_admin;


--
-- Name: TABLE json_response; Type: ACL; Schema: crawler; Owner: -
--

GRANT SELECT ON TABLE crawler.json_response TO system_admin;


--
-- Name: TABLE frontier_failure; Type: ACL; Schema: crawler; Owner: -
--

GRANT SELECT ON TABLE crawler.frontier_failure TO system_admin;


--
-- Name: TABLE frontier_failure_group; Type: ACL; Schema: crawler; Owner: -
--

GRANT SELECT ON TABLE crawler.frontier_failure_group TO system_admin;


--
-- Name: TABLE frontier_fetch_job; Type: ACL; Schema: crawler; Owner: -
--

GRANT SELECT ON TABLE crawler.frontier_fetch_job TO system_admin;


--
-- Name: TABLE frontier_summary; Type: ACL; Schema: crawler; Owner: -
--

GRANT SELECT ON TABLE crawler.frontier_summary TO system_admin;


--
-- Name: TABLE incremental_ranges; Type: ACL; Schema: crawler; Owner: -
--

GRANT SELECT ON TABLE crawler.incremental_ranges TO system_admin;


--
-- Name: TABLE json_response_cache; Type: ACL; Schema: crawler; Owner: -
--

GRANT SELECT ON TABLE crawler.json_response_cache TO system_admin;


--
-- Name: TABLE rate_limit_rule; Type: ACL; Schema: crawler; Owner: -
--

GRANT SELECT ON TABLE crawler.rate_limit_rule TO system_admin;


--
-- Name: TABLE age_group; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.age_group TO anonymous;


--
-- Name: TABLE competition_entry; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.competition_entry TO anonymous;


--
-- Name: TABLE competition_official; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.competition_official TO anonymous;


--
-- Name: TABLE competition_result; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.competition_result TO anonymous;


--
-- Name: TABLE competition_participation; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.competition_participation TO anonymous;


--
-- Name: TABLE competition_round; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.competition_round TO anonymous;


--
-- Name: TABLE competition_round_judge; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.competition_round_judge TO anonymous;


--
-- Name: TABLE competition_round_result; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.competition_round_result TO anonymous;


--
-- Name: TABLE competitor; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.competitor TO anonymous;


--
-- Name: TABLE competitor_category_progress; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.competitor_category_progress TO anonymous;


--
-- Name: TABLE competitor_club_affiliation; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.competitor_club_affiliation TO anonymous;


--
-- Name: TABLE competitor_component; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.competitor_component TO anonymous;


--
-- Name: TABLE couple_age_rule; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.couple_age_rule TO anonymous;


--
-- Name: TABLE dance; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.dance TO anonymous;


--
-- Name: TABLE dance_program; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.dance_program TO anonymous;


--
-- Name: TABLE dance_program_dance; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.dance_program_dance TO anonymous;


--
-- Name: TABLE event; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.event TO anonymous;


--
-- Name: TABLE event_official; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.event_official TO anonymous;


--
-- Name: TABLE federation_club; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.federation_club TO anonymous;


--
-- Name: TABLE judge_score; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.judge_score TO anonymous;


--
-- Name: TABLE person; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.person TO anonymous;


--
-- Name: TABLE person_club_membership; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.person_club_membership TO anonymous;


--
-- Name: TABLE person_license; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.person_license TO anonymous;


--
-- Name: TABLE ranklist; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.ranklist TO anonymous;


--
-- Name: TABLE ranklist_entry; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.ranklist_entry TO anonymous;


--
-- Name: TABLE ranklist_snapshot; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.ranklist_snapshot TO anonymous;


--
-- Name: TABLE round_dance; Type: ACL; Schema: federated; Owner: -
--

GRANT SELECT ON TABLE federated.round_dance TO anonymous;


--
-- Name: SEQUENCE access_credential_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.access_credential_id_seq TO anonymous;


--
-- Name: TABLE access_event; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.access_event TO anonymous;


--
-- Name: SEQUENCE access_event_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.access_event_id_seq TO anonymous;


--
-- Name: SEQUENCE account_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.account_id_seq TO anonymous;


--
-- Name: TABLE accounting_period; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.accounting_period TO anonymous;


--
-- Name: SEQUENCE accounting_period_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.accounting_period_id_seq TO anonymous;


--
-- Name: SEQUENCE aktuality_at_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.aktuality_at_id_seq TO anonymous;


--
-- Name: TABLE announcement_attachment; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.announcement_attachment TO anonymous;


--
-- Name: TABLE announcement_audience; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.announcement_audience TO anonymous;


--
-- Name: SEQUENCE announcement_audience_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.announcement_audience_id_seq TO anonymous;


--
-- Name: SEQUENCE announcement_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.announcement_id_seq TO anonymous;


--
-- Name: TABLE article_attachment; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.article_attachment TO anonymous;


--
-- Name: TABLE cohort_group; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.cohort_group TO anonymous;


--
-- Name: SEQUENCE cohort_group_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.cohort_group_id_seq TO anonymous;


--
-- Name: SEQUENCE cohort_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.cohort_id_seq TO anonymous;


--
-- Name: TABLE cohort_membership; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.cohort_membership TO anonymous;


--
-- Name: SEQUENCE cohort_membership_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.cohort_membership_id_seq TO anonymous;


--
-- Name: SEQUENCE cohort_subscription_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.cohort_subscription_id_seq TO anonymous;


--
-- Name: TABLE person_federation_link; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT ON TABLE public.person_federation_link TO anonymous;


--
-- Name: TABLE couple_federation_link; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT ON TABLE public.couple_federation_link TO anonymous;


--
-- Name: SEQUENCE couple_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.couple_id_seq TO anonymous;


--
-- Name: TABLE current_cohort_membership; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT ON TABLE public.current_cohort_membership TO anonymous;


--
-- Name: TABLE current_tenant_administrator; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT ON TABLE public.current_tenant_administrator TO anonymous;


--
-- Name: TABLE current_tenant_membership; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT ON TABLE public.current_tenant_membership TO anonymous;


--
-- Name: TABLE current_tenant_trainer; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT ON TABLE public.current_tenant_trainer TO anonymous;


--
-- Name: TABLE dokumenty; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.dokumenty TO anonymous;


--
-- Name: SEQUENCE dokumenty_d_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.dokumenty_d_id_seq TO anonymous;


--
-- Name: SEQUENCE event_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.event_id_seq TO anonymous;


--
-- Name: SEQUENCE event_lesson_request_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.event_lesson_request_id_seq TO anonymous;


--
-- Name: SEQUENCE event_registration_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.event_registration_id_seq TO anonymous;


--
-- Name: TABLE event_series; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_series TO anonymous;


--
-- Name: SEQUENCE event_series_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.event_series_id_seq TO anonymous;


--
-- Name: TABLE event_target_cohort; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_target_cohort TO anonymous;


--
-- Name: SEQUENCE event_target_cohort_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.event_target_cohort_id_seq TO anonymous;


--
-- Name: SEQUENCE event_trainer_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.event_trainer_id_seq TO anonymous;


--
-- Name: TABLE file; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.file TO anonymous;


--
-- Name: SEQUENCE file_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.file_id_seq TO anonymous;


--
-- Name: TABLE form_responses; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.form_responses TO anonymous;


--
-- Name: SEQUENCE form_responses_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.form_responses_id_seq TO anonymous;


--
-- Name: TABLE location_image; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.location_image TO anonymous;


--
-- Name: SEQUENCE membership_application_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.membership_application_id_seq TO anonymous;


--
-- Name: SEQUENCE otp_token_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.otp_token_id_seq TO anonymous;


--
-- Name: SEQUENCE payment_debtor_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.payment_debtor_id_seq TO anonymous;


--
-- Name: SEQUENCE payment_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.payment_id_seq TO anonymous;


--
-- Name: TABLE payment_recipient; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.payment_recipient TO anonymous;


--
-- Name: SEQUENCE payment_recipient_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.payment_recipient_id_seq TO anonymous;


--
-- Name: SEQUENCE person_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.person_id_seq TO anonymous;


--
-- Name: TABLE person_invitation; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.person_invitation TO anonymous;


--
-- Name: SEQUENCE person_invitation_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.person_invitation_id_seq TO anonymous;


--
-- Name: TABLE user_proxy; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.user_proxy TO anonymous;


--
-- Name: TABLE person_last_activity; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT ON TABLE public.person_last_activity TO anonymous;


--
-- Name: SEQUENCE posting_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.posting_id_seq TO anonymous;


--
-- Name: TABLE scoreboard_manual_adjustment; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.scoreboard_manual_adjustment TO anonymous;


--
-- Name: SEQUENCE scoreboard_manual_adjustment_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.scoreboard_manual_adjustment_id_seq TO anonymous;
GRANT USAGE ON SEQUENCE public.scoreboard_manual_adjustment_id_seq TO administrator;


--
-- Name: TABLE security_event; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT ON TABLE public.security_event TO member;


--
-- Name: SEQUENCE security_event_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.security_event_id_seq TO anonymous;


--
-- Name: SEQUENCE tenant_administrator_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.tenant_administrator_id_seq TO anonymous;


--
-- Name: SEQUENCE tenant_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.tenant_id_seq TO anonymous;


--
-- Name: SEQUENCE tenant_location_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.tenant_location_id_seq TO anonymous;


--
-- Name: SEQUENCE tenant_membership_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.tenant_membership_id_seq TO anonymous;


--
-- Name: SEQUENCE tenant_trainer_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.tenant_trainer_id_seq TO anonymous;


--
-- Name: SEQUENCE transaction_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.transaction_id_seq TO anonymous;


--
-- Name: SEQUENCE user_proxy_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.user_proxy_id_seq TO anonymous;


--
-- Name: SEQUENCE users_u_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.users_u_id_seq TO anonymous;


--
-- Name: TABLE account_balances; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.account_balances TO anonymous;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: federated; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE olymp IN SCHEMA federated GRANT SELECT ON TABLES TO anonymous;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: public; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE olymp IN SCHEMA public GRANT SELECT,USAGE ON SEQUENCES TO anonymous;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: public; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT SELECT,USAGE ON SEQUENCES TO anonymous;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: -; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres REVOKE ALL ON FUNCTIONS FROM PUBLIC;


--
-- PostgreSQL database dump complete
--

\unrestrict Uj6s2rBFWEaDgfIWk3EuLVmOQ3zhEGFovqN5eMQolptGGIZkDNulGcVXW1mgdis

