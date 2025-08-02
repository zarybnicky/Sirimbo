--
-- PostgreSQL database dump
--

-- Dumped from database version 17.5
-- Dumped by pg_dump version 17.5

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
-- Name: http; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS http WITH SCHEMA public;


--
-- Name: EXTENSION http; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION http IS 'HTTP client for PostgreSQL, allows web page retrieval inside the database.';


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
-- Name: application_form_status; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.application_form_status AS ENUM (
    'new',
    'sent',
    'approved',
    'rejected'
);


--
-- Name: attendance_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.attendance_type AS ENUM (
    'unknown',
    'attended',
    'excused',
    'not-excused',
    'cancelled'
);


--
-- Name: event_payment_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.event_payment_type AS ENUM (
    'upfront',
    'after_instance',
    'none'
);


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
	my_person_ids json,
	my_tenant_ids json,
	my_cohort_ids json,
	my_couple_ids json,
	is_member boolean,
	is_trainer boolean,
	is_admin boolean
);


--
-- Name: TYPE jwt_token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TYPE public.jwt_token IS '@jwt';


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
	CONSTRAINT price_check CHECK (((VALUE IS NULL) OR (((VALUE).currency IS NOT NULL) AND ((VALUE).amount IS NOT NULL) AND (length((VALUE).currency) = 3) AND ((VALUE).currency = upper((VALUE).currency)))));


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

COMMENT ON FUNCTION public.current_tenant_id() IS '@omit';


SET default_tablespace = '';

SET default_table_access_method = heap;

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
-- Name: COLUMN event_lesson_demand.registration_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.event_lesson_demand.registration_id IS '@hasDefault';


--
-- Name: register_to_event_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.register_to_event_type AS (
	event_id bigint,
	person_id bigint,
	couple_id bigint,
	note text,
	lessons public.event_lesson_demand[]
);


--
-- Name: relationship_status; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.relationship_status AS ENUM (
    'pending',
    'active',
    'expired'
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
-- Name: event_instance; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_instance (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    since timestamp with time zone NOT NULL,
    until timestamp with time zone NOT NULL,
    range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL,
    location_id bigint,
    is_cancelled boolean DEFAULT false
);


--
-- Name: TABLE event_instance; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_instance IS '@omit create,delete
@simpleCollections only';


--
-- Name: event_registration; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_registration (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    target_cohort_id bigint,
    couple_id bigint,
    person_id bigint,
    note text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_registration_check CHECK ((((couple_id IS NOT NULL) AND (person_id IS NULL)) OR ((couple_id IS NULL) AND (person_id IS NOT NULL))))
);


--
-- Name: TABLE event_registration; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_registration IS '@omit update
@simpleCollections both';


--
-- Name: payment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.payment (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    accounting_period_id bigint NOT NULL,
    cohort_subscription_id bigint,
    event_registration_id bigint,
    event_instance_id bigint,
    status public.payment_status NOT NULL,
    variable_symbol text,
    specific_symbol text,
    is_auto_credit_allowed boolean DEFAULT true NOT NULL,
    tags text[] DEFAULT ARRAY[]::text[] NOT NULL,
    due_at timestamp with time zone,
    paid_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE payment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.payment IS '@omit create
@simpleCollections only';


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

COMMENT ON TABLE public.transaction IS '@omit create,update';


--
-- Name: calculate_transaction_effective_date(public.transaction); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.calculate_transaction_effective_date(t public.transaction) RETURNS timestamp with time zone
    LANGUAGE sql
    BEGIN ATOMIC
 SELECT COALESCE(( SELECT event_instance.since
            FROM (public.payment
              JOIN public.event_instance ON ((payment.event_instance_id = event_instance.id)))
           WHERE ((calculate_transaction_effective_date.t).payment_id = payment.id)), ( SELECT event_instance.since
            FROM ((public.payment
              JOIN public.event_registration ON ((payment.event_registration_id = event_registration.id)))
              JOIN public.event_instance ON ((event_instance.event_id = event_registration.event_id)))
           WHERE ((calculate_transaction_effective_date.t).payment_id = payment.id)
           ORDER BY event_instance.since
          LIMIT 1), ( SELECT payment.due_at
            FROM public.payment
           WHERE ((calculate_transaction_effective_date.t).payment_id = payment.id)), (t).created_at) AS "coalesce";
END;


--
-- Name: can_trainer_edit_event(bigint); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.can_trainer_edit_event(eid bigint) RETURNS boolean
    LANGUAGE sql STABLE SECURITY DEFINER LEAKPROOF PARALLEL SAFE
    AS $$
  select exists (
    select 1 from event_trainer where eid = event_id and person_id = any (current_person_ids())
  ) or not exists (
    select 1 from event_trainer where eid = event_id
  );
$$;


--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users (
    u_id bigint NOT NULL,
    u_login public.citext,
    u_pass character(40) NOT NULL,
    u_jmeno text,
    u_prijmeni text,
    u_email public.citext NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    u_ban boolean DEFAULT true NOT NULL,
    u_confirmed boolean DEFAULT false NOT NULL,
    u_created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    id bigint GENERATED ALWAYS AS (u_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    last_login timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    u_timestamp timestamp with time zone GENERATED ALWAYS AS (updated_at) STORED
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
-- Name: COLUMN users.u_ban; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.users.u_ban IS '@omit';


--
-- Name: COLUMN users.u_confirmed; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.users.u_confirmed IS '@omit';


--
-- Name: create_jwt_token(public.users); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.create_jwt_token(u public.users) RETURNS public.jwt_token
    LANGUAGE sql STABLE
    AS $$
  with details as (
    SELECT
      u_id,
      user_proxy.person_id as person_id,
      tenant_memberships || tenant_trainers || tenant_administrators as my_tenant_ids,
      cohort_memberships as my_cohort_ids,
      couple_ids as my_couple_ids,
      current_tenant_id() = ANY (tenant_memberships || tenant_trainers || tenant_administrators) as is_member,
      current_tenant_id() = ANY (tenant_trainers) as is_trainer,
      current_tenant_id() = ANY (tenant_administrators) as is_admin
    from users
    left join user_proxy on user_id=users.u_id
    left join auth_details on user_proxy.person_id=auth_details.person_id
    where users.u_id=u.u_id
  ) select
    extract(epoch from now() + interval '7 days')::integer,
    u.u_id,
    current_tenant_id(),
    u.u_login,
    u.u_email,
    array_to_json(array_agg(person_id)) as my_person_ids,
    array_to_json(app_private.array_accum(my_tenant_ids)) as my_tenant_ids,
    array_to_json(app_private.array_accum(my_cohort_ids)) as my_cohort_ids,
    array_to_json(app_private.array_accum(my_couple_ids)) as my_couple_ids,
    bool_or(is_member) as is_member,
    bool_or(is_trainer) as is_trainer,
    bool_or(is_admin) as is_admin
  from details group by u_id;
$$;


--
-- Name: create_latest_lesson_payments(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.create_latest_lesson_payments() RETURNS SETOF public.payment
    LANGUAGE plpgsql
    AS $$
declare
  v_id bigint;
  created_ids bigint[] := array[]::bigint[];
begin
  perform set_config('jwt.claims.tenant_id', '2', true);
  perform set_config('jwt.claims.my_tenant_ids', '[2]', true);
  if not (select row_security_active('event')) then
    set local role to administrator;
  end if;

  update event set payment_type='after_instance'
  where type='lesson' and payment_type <> 'after_instance';

  select array_agg((create_event_instance_payment(event_instance)).id) into created_ids
  from event_instance join event on event.id=event_id
  where type='lesson'
    and not event_instance.is_cancelled
    and event_instance.since < now()
    and payment_type = 'after_instance'
    and not exists (
      select * from payment where event_instance_id=event_instance.id
    );

  update payment set status ='unpaid' where id = any (created_ids);

  foreach v_id in array created_ids loop
    perform resolve_payment_with_credit(payment.*) from payment where id = v_id;
  end loop;

  return query select * from payment where id = any (created_ids);
end;
$$;


--
-- Name: cron_update_memberships(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.cron_update_memberships() RETURNS void
    LANGUAGE sql
    AS $$
  update user_proxy set status = 'active' where now() <@ active_range and status <> 'active';
  update user_proxy set status = 'pending' where now() < since and status <> 'pending';
  update user_proxy set status = 'expired' where now() > until and status <> 'expired';

  update couple set status = 'active' where now() <@ active_range and status <> 'active';
  update couple set status = 'pending' where now() < since and status <> 'pending';
  update couple set status = 'expired' where now() > until and status <> 'expired';

  update cohort_membership set status = 'active' where now() <@ active_range and status <> 'active';
  update cohort_membership set status = 'pending' where now() < since and status <> 'pending';
  update cohort_membership set status = 'expired' where now() > until and status <> 'expired';

  update tenant_membership set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_membership set status = 'pending' where now() < since and status <> 'pending';
  update tenant_membership set status = 'expired' where now() > until and status <> 'expired';

  update tenant_trainer set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_trainer set status = 'pending' where now() < since and status <> 'pending';
  update tenant_trainer set status = 'expired' where now() > until and status <> 'expired';

  update tenant_administrator set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_administrator set status = 'pending' where now() < since and status <> 'pending';
  update tenant_administrator set status = 'expired' where now() > until and status <> 'expired';
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
-- Name: index_advisor(text); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.index_advisor(query text) RETURNS TABLE(startup_cost_before jsonb, startup_cost_after jsonb, total_cost_before jsonb, total_cost_after jsonb, index_statements text[], errors text[])
    LANGUAGE plpgsql
    AS $_$
declare
    n_args int;
    prepared_statement_name text = 'index_advisor_working_statement';
    hypopg_schema_name text = (select extnamespace::regnamespace::text from pg_extension where extname = 'hypopg');
    explain_plan_statement text;
    error_message text;
    rec record;
    plan_initial jsonb;
    plan_final jsonb;
    statements text[] = '{}';
    v_context text;
begin

    -- Remove comment lines (its common that they contain semicolons)
    query := trim(
        regexp_replace(
            regexp_replace(
                regexp_replace(query,'\/\*.+\*\/', '', 'g'),
            '--[^\r\n]*', ' ', 'g'),
        '\s+', ' ', 'g')
    );

    -- Remove trailing semicolon
    query := regexp_replace(query, ';\s*$', '');

    begin
        -- Disallow multiple statements
        if query ilike '%;%' then
            raise exception 'Query must not contain a semicolon';
        end if;

        -- Hack to support PostgREST because the prepared statement for args incorrectly defaults to text
        query := replace(query, 'WITH pgrst_payload AS (SELECT $1 AS json_data)', 'WITH pgrst_payload AS (SELECT $1::json AS json_data)');

        -- Create a prepared statement for the given query
        deallocate all;
        execute format('prepare %I as %s', prepared_statement_name, query);

        -- Detect how many arguments are present in the prepared statement
        n_args = (
            select
                coalesce(array_length(parameter_types, 1), 0)
            from
                pg_prepared_statements
            where
                name = prepared_statement_name
            limit
                1
        );

        -- Create a SQL statement that can be executed to collect the explain plan
        explain_plan_statement = format(
            'set local plan_cache_mode = force_generic_plan; explain (format json) execute %I%s',
            --'explain (format json) execute %I%s',
            prepared_statement_name,
            case
                when n_args = 0 then ''
                else format(
                    '(%s)', array_to_string(array_fill('null'::text, array[n_args]), ',')
                )
            end
        );
        -- Store the query plan before any new indexes
        execute explain_plan_statement into plan_initial;

        -- Create possible indexes
        for rec in (
            with extension_regclass as (
                select
                    distinct objid as oid
                from
                    pg_catalog.pg_depend
                where
                    deptype = 'e'
            )
            select
                pc.relnamespace::regnamespace::text as schema_name,
                pc.relname as table_name,
                pa.attname as column_name,
                format(
                    'select %I.hypopg_create_index($i$create index on %I.%I(%I)$i$)',
                    hypopg_schema_name,
                    pc.relnamespace::regnamespace::text,
                    pc.relname,
                    pa.attname
                ) hypopg_statement
            from
                pg_catalog.pg_class pc
                join pg_catalog.pg_attribute pa
                    on pc.oid = pa.attrelid
                left join extension_regclass er
                    on pc.oid = er.oid
                left join pg_catalog.pg_index pi
                    on pc.oid = pi.indrelid
                    and (select array_agg(x) from unnest(pi.indkey) v(x)) = array[pa.attnum]
                    and pi.indexprs is null -- ignore expression indexes
                    and pi.indpred is null -- ignore partial indexes
            where
                pc.relnamespace::regnamespace::text not in ( -- ignore schema list
                    'pg_catalog', 'pg_toast', 'information_schema'
                )
                and er.oid is null -- ignore entities owned by extensions
                and pc.relkind in ('r', 'm') -- regular tables, and materialized views
                and pc.relpersistence = 'p' -- permanent tables (not unlogged or temporary)
                and pa.attnum > 0
                and not pa.attisdropped
                and pi.indrelid is null
                and pa.atttypid in (20,16,1082,1184,1114,701,23,21,700,1083,2950,1700,25,18,1042,1043)
            )
            loop
                -- Create the hypothetical index
                execute rec.hypopg_statement;
            end loop;

        /*
        for rec in select * from hypopg()
            loop
                raise notice '%', rec;
            end loop;
        */

        -- Create a prepared statement for the given query
        -- The original prepared statement MUST be dropped because its plan is cached
        execute format('deallocate %I', prepared_statement_name);
        execute format('prepare %I as %s', prepared_statement_name, query);

        -- Store the query plan after new indexes
        execute explain_plan_statement into plan_final;

        --raise notice '%', plan_final;

        -- Idenfity referenced indexes in new plan
        execute format(
            'select
                coalesce(array_agg(hypopg_get_indexdef(indexrelid) order by indrelid, indkey::text), $i${}$i$::text[])
            from
                %I.hypopg()
            where
                %s ilike ($i$%%$i$ || indexname || $i$%%$i$)
            ',
            hypopg_schema_name,
            quote_literal(plan_final)::text
        ) into statements;

        -- Reset all hypothetical indexes
        perform hypopg_reset();

        -- Reset prepared statements
        deallocate all;

        return query values (
            (plan_initial -> 0 -> 'Plan' -> 'Startup Cost'),
            (plan_final -> 0 -> 'Plan' -> 'Startup Cost'),
            (plan_initial -> 0 -> 'Plan' -> 'Total Cost'),
            (plan_final -> 0 -> 'Plan' -> 'Total Cost'),
            statements::text[],
            array[]::text[]
        );
        return;

    exception when others then
        get stacked diagnostics error_message = MESSAGE_TEXT,
		v_context = pg_exception_context;

        return query values (
            null::jsonb,
            null::jsonb,
            null::jsonb,
            null::jsonb,
            array[]::text[],
            array[error_message, v_context]::text[]
        );
        return;
    end;

end;
$_$;


--
-- Name: FUNCTION index_advisor(query text); Type: COMMENT; Schema: app_private; Owner: -
--

COMMENT ON FUNCTION app_private.index_advisor(query text) IS '@omit';


--
-- Name: log_in_as(public.users); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.log_in_as(u public.users) RETURNS TABLE(key text, value text)
    LANGUAGE sql
    AS $$
  select 'jwt.claims.' || kv.key, set_config('jwt.claims.' || kv.key, kv.value, false)
  from app_private.create_jwt_token(u) j join lateral jsonb_each_text(to_jsonb(j)) kv on true
  union
  select 'role', set_config('role', case when is_admin then 'administrator' when is_trainer then 'trainer' when is_member then 'member' else 'anonymous' end, false)
  from app_private.create_jwt_token(u) j
$$;


--
-- Name: merge_couples(bigint, bigint); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.merge_couples(one bigint, two bigint) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  registrations_before bigint;
  registrations_after bigint;
begin
  -- duplicate detection: select array_agg(couple.id), man_id, woman_id, array_agg(array[since, until] order by since) from couple group by man_id, woman_id having count(*) > 1;

  select count(*) into registrations_before from event_registration;

  assert (select min(man_id) is not distinct from max(man_id) from couple where id in (one, two));
  assert (select min(woman_id) is not distinct from max(woman_id) from couple where id in (one, two));
  assert (select extract(epoch from (select since from couple where id = two) - (select until from couple where id = one)) < 3600);

  update event_registration set couple_id = one where couple_id = two;
  update couple set until = (select until from couple where id = two) where id = one;
  delete from couple where id = two;

  select count(*) into registrations_after from event_registration;
  assert registrations_before = registrations_after;
end
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
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    active boolean GENERATED ALWAYS AS ((status = 'active'::public.relationship_status)) STORED NOT NULL
);


--
-- Name: TABLE cohort_membership; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.cohort_membership IS '@simpleCollections only';


--
-- Name: COLUMN cohort_membership.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.cohort_membership.active_range IS '@omit';


--
-- Name: register_new_cohort_member_to_events(public.cohort_membership); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.register_new_cohort_member_to_events(c public.cohort_membership) RETURNS SETOF public.event_registration
    LANGUAGE sql
    AS $$
  insert into event_registration (event_id, target_cohort_id, person_id, tenant_id)
  select event.id, event_target_cohort.id, cohort_membership.person_id, cohort_membership.tenant_id
  from event
  join event_target_cohort on event_id=event.id
  join cohort_membership on event_target_cohort.cohort_id=cohort_membership.cohort_id and active_range @> now()
  left join event_registration on target_cohort_id=event_target_cohort.id and event_registration.person_id=cohort_membership.person_id and event_registration.event_id=event.id
  where event_registration.id is null
    and exists (select 1 from event_instance where event_id=event.id and until > now())
    and cohort_membership.id = c.id
    and event.tenant_id = c.tenant_id
  on conflict on constraint event_registration_unique_event_person_couple_key do nothing
  returning event_registration.*;
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
-- Name: tg_account_balances__update(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_account_balances__update() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
BEGIN
	REFRESH MATERIALIZED VIEW account_balances;
  return null;
END
$$;


--
-- Name: tg_auth_details__refresh(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_auth_details__refresh() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
BEGIN
  refresh materialized view concurrently auth_details;
  return null;
END
$$;


--
-- Name: tg_cohort_membership__on_status(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_cohort_membership__on_status() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  if NEW.status = 'expired' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- with affected as (
    --   select cohort_membership.until, event_registration.person_id, event_registration.id as registration_id
    --   from event_target_cohort
    --   join event_registration on event_target_cohort.event_id=event_registration.event_id
    --   join cohort_membership on cohort_membership.person_id=event_registration.person_id and cohort_membership.cohort_id=event_target_cohort.cohort_id
    --   where cohort_membership.until is not null
    --     and cohort_membership.id = OLD.id
    -- )
    -- update event_attendance set status = 'cancelled'
    -- where id in (
    --   select event_attendance.id
    --   from event_attendance
    --   join event_instance on event_instance.id = event_attendance.instance_id
    --   join affected on event_attendance.registration_id = affected.registration_id
    --    and affected.until < event_instance.since
    --    and status in ('unknown', 'not-excused', 'excused')
    -- );

    -- with affected as (
    --   select cohort_membership.until, event_registration.person_id, event_registration.id as registration_id
    --   from event_target_cohort
    --   join event_registration on event_target_cohort.event_id=event_registration.event_id
    --   join cohort_membership on cohort_membership.person_id=event_registration.person_id and cohort_membership.cohort_id=event_target_cohort.cohort_id
    --   where cohort_membership.until is not null
    --   and cohort_membership.id = OLD.id
    -- )
    -- delete from event_registration
    -- where exists (
    --   select event_attendance.id
    --   from event_attendance
    --   join event_instance on event_instance.id = event_attendance.instance_id
    --   join affected on event_attendance.registration_id = affected.registration_id
    --    and affected.until < event_instance.since
    --    and status = 'cancelled'
    -- ) and not exists (
    --   select event_attendance.id
    --   from event_attendance
    --   join event_instance on event_instance.id = event_attendance.instance_id
    --   join affected on event_attendance.registration_id = affected.registration_id
    --    and affected.until < event_instance.since
    --    and status <> 'cancelled'
    -- );

  elsif NEW.status = 'active' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- add payments
    perform app_private.register_new_cohort_member_to_events(NEW);
  end if;
  return NEW;
end;
$$;


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
-- Name: tg_event_instance__delete_payment_on_cancellation(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
declare
  payment_id bigint;
begin
  delete from payment where event_instance_id = OLD.id;

  if not new.is_cancelled then
    select (create_event_instance_payment(event_instance)).id into payment_id
    from event_instance join event on event.id=event_id
    where type='lesson'
      and event_instance.id = NEW.id
      and not event_instance.is_cancelled
      and event_instance.since < now()
      and payment_type = 'after_instance'
      and not exists (
        select * from payment where event_instance_id=event_instance.id
      );

    update payment set status ='unpaid' where id = payment_id;
    perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
  end if;

  return OLD;
end;
$$;


--
-- Name: tg_event_instance__update_parent_range(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_event_instance__update_parent_range() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
    UPDATE event
    SET since = (SELECT min(since) FROM event_instance WHERE event_id = NEW.event_id),
        until = (SELECT max(until) FROM event_instance WHERE event_id = NEW.event_id)
    WHERE id = NEW.event_id;
  END IF;

  IF TG_OP = 'UPDATE' OR TG_OP = 'DELETE' THEN
    UPDATE event
    SET since = (SELECT min(since) FROM event_instance WHERE event_id = OLD.event_id),
        until = (SELECT max(until) FROM event_instance where event_id = OLD.event_id)
    WHERE id = OLD.event_id;
  END IF;

  RETURN NULL;
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
-- Name: tg_payment__fill_accounting_period(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_payment__fill_accounting_period() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
declare
  period accounting_period;
  since timestamptz;
begin
	if NEW.accounting_period_id is null then
    select id into NEW.accounting_period_id from accounting_period where range @> now();
    if not found then
      since := case
        when extract(month from now()) > 8
        then date_trunc('year', now()) + '8 month'::interval
        else date_trunc('year', now()) + '8 month'::interval - '1 year'::interval
      end;
      insert into accounting_period (name, since, until)
      values ('Školní rok ' || extract(year from since), since, since + '12 month'::interval - '1 day'::interval)
      returning id into NEW.accounting_period_id;
    end if;
  end if;
  return NEW;
END
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
-- Name: tg_tenant_membership__on_status(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_tenant_membership__on_status() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if NEW.status = 'expired' then
    update cohort_membership set status = 'expired', until = NEW.until where cohort_membership.person_id = NEW.person_id;
  end if;
  return NEW;
end;
$$;


--
-- Name: tg_transaction__effective_date(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_transaction__effective_date() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  if NEW.effective_date is null then
    NEW.effective_date = app_private.calculate_transaction_effective_date(NEW);
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
  if length(NEW.u_pass) <> 40 then
      select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
      NEW.u_pass := encode(digest(v_salt || NEW.u_pass || v_salt, 'sha1'), 'hex');
  end if;
  return NEW;
end;
$$;


--
-- Name: tg_users__trim_login(); Type: FUNCTION; Schema: app_private; Owner: -
--

CREATE FUNCTION app_private.tg_users__trim_login() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare
  v_salt varchar;
begin
  NEW.u_login := trim(NEW.u_login);
  return NEW;
end;
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

COMMENT ON TABLE public.account IS '@omit create,update,delete
@simpleCollections only';


--
-- Name: posting; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.posting (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    transaction_id bigint NOT NULL,
    account_id bigint NOT NULL,
    original_account_id bigint,
    amount numeric(19,4),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE posting; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.posting IS '@omit create,update,delete
@simpleCollections only';


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
-- Name: upozorneni; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.upozorneni (
    up_id bigint NOT NULL,
    up_kdo bigint,
    up_nadpis text NOT NULL,
    up_text text NOT NULL,
    up_lock boolean DEFAULT false NOT NULL,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    scheduled_since timestamp with time zone,
    scheduled_until timestamp with time zone,
    is_visible boolean DEFAULT true,
    id bigint GENERATED ALWAYS AS (up_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    sticky boolean DEFAULT false NOT NULL,
    up_timestamp timestamp with time zone GENERATED ALWAYS AS (updated_at) STORED NOT NULL,
    up_timestamp_add timestamp with time zone GENERATED ALWAYS AS (created_at) STORED NOT NULL
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
  SELECT nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$;


--
-- Name: FUNCTION current_user_id(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.current_user_id() IS '@omit';


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
    LANGUAGE plpgsql
    AS $$
declare
  v_event event;
  v_reg event_registration;
begin
  select * into v_reg from event_registration er where er.id = registration_id;
  select * into v_event from event where id = v_reg.event_id;

  if v_event is null or v_reg is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if v_event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if v_reg.person_id <> all (current_person_ids()) and v_reg.couple_id <> all (current_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  delete from event_registration where id = v_reg.id;
end;
$$;


--
-- Name: change_password(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.change_password(new_pass text) RETURNS void
    LANGUAGE sql STRICT
    AS $$
  update users set u_pass = new_pass where u_id = current_user_id();
$$;


--
-- Name: immutable_concat_ws(text, text[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.immutable_concat_ws(text, VARIADIC text[]) RETURNS text
    LANGUAGE internal IMMUTABLE PARALLEL SAFE
    AS $$text_concat_ws$$;


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
    csts_id text,
    wdsf_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    legacy_user_id bigint,
    prefix_title text DEFAULT ''::text NOT NULL,
    suffix_title text DEFAULT ''::text NOT NULL,
    bio text DEFAULT ''::text NOT NULL,
    email public.citext,
    phone text,
    name text GENERATED ALWAYS AS (public.immutable_concat_ws(' '::text, VARIADIC ARRAY[NULLIF(TRIM(BOTH FROM prefix_title), ''::text), NULLIF(TRIM(BOTH FROM first_name), ''::text), NULLIF(TRIM(BOTH FROM last_name), ''::text),
CASE
    WHEN ((suffix_title IS NULL) OR (TRIM(BOTH FROM suffix_title) = ''::text)) THEN NULL::text
    ELSE public.immutable_concat_ws(' '::text, VARIADIC ARRAY[','::text, TRIM(BOTH FROM suffix_title)])
END])) STORED NOT NULL,
    address public.address_domain
);


--
-- Name: TABLE person; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.person IS '@omit create';


--
-- Name: COLUMN person.legacy_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.person.legacy_user_id IS '@omit';


--
-- Name: confirm_membership_application(bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.confirm_membership_application(application_id bigint) RETURNS public.person
    LANGUAGE sql
    AS $$
  with t_person as (
    insert into person (
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone
    ) select
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone
    from membership_application where id = application_id and status='sent'
    returning *
  ), appl as (
     update membership_application set status='approved' where id=application_id
  ), member as (
    insert into tenant_membership (tenant_id, person_id)
    values (current_tenant_id(), (select id from t_person))
    returning *
  ), proxy as (
    insert into user_proxy (person_id, user_id)
    values ((select id from t_person), (select created_by from membership_application where id = application_id))
  ) select * from t_person;
$$;


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
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    active boolean GENERATED ALWAYS AS ((status = 'active'::public.relationship_status)) STORED NOT NULL
);


--
-- Name: TABLE couple; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.couple IS '@simpleCollections only';


--
-- Name: COLUMN couple.legacy_pary_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.couple.legacy_pary_id IS '@omit';


--
-- Name: COLUMN couple.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.couple.active_range IS '@omit';


--
-- Name: couple_event_instances(public.couple); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.couple_event_instances(p public.couple) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $$
  select distinct event_instance.*
  from event_instance
  join event_registration on event_instance.event_id=event_registration.event_id
  where couple_id = p.id;
$$;


--
-- Name: FUNCTION couple_event_instances(p public.couple); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.couple_event_instances(p public.couple) IS '@simpleCollections only
@filterable
@sortable';


--
-- Name: create_credit_transaction_for_person(bigint, text, numeric, text, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_credit_transaction_for_person(v_person_id bigint, v_description text, v_amount numeric, v_currency text, v_date timestamp with time zone DEFAULT now()) RETURNS public.transaction
    LANGUAGE sql
    AS $$
  with txn as (
    insert into transaction (source, description, effective_date) values ('manual-credit', v_description, v_date) returning *
  ), posting as (
    insert into posting (transaction_id, account_id, amount) values ((select id from txn), (select id from person_account(V_person_id, v_currency)), v_amount)
  )
  select * from txn;
$$;


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
    capacity integer DEFAULT '0'::bigint NOT NULL,
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
    location_id bigint,
    payment_type public.event_payment_type DEFAULT 'none'::public.event_payment_type NOT NULL,
    is_paid_by_tenant boolean DEFAULT true NOT NULL,
    member_price public.price DEFAULT NULL::public.price_type,
    guest_price public.price DEFAULT NULL::public.price_type,
    payment_recipient_id bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: TABLE event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event IS '@omit create';


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
-- Name: create_event(public.event, public.event_instance[], public.event_trainer[], public.event_target_cohort[], public.event_registration[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]) RETURNS public.event
    LANGUAGE plpgsql
    AS $$
begin
  insert into event (name, summary, description, type, location_text, capacity, is_visible, is_public, is_locked, enable_notes)
  select info.name, info.summary, info.description, info.type, info.location_text, info.capacity, info.is_visible, info.is_public, info.is_locked, info.enable_notes
  returning * into info;

  insert into event_instance (event_id, since, until)
  select info.id, since, until from unnest(instances) i;

  insert into event_trainer (event_id, person_id, lessons_offered)
  select info.id, person_id, coalesce(lessons_offered, 0) from unnest(trainers) i;

  insert into event_target_cohort (event_id, cohort_id)
  select info.id, cohort_id from unnest(cohorts) i;

  insert into event_registration (event_id, person_id, couple_id)
  select info.id, person_id, couple_id from unnest(registrations) i;
end;
$$;


--
-- Name: FUNCTION create_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.create_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]) IS '@arg0variant input
@arg1variant patch
@arg2variant patch
@arg3variant patch
@arg4variant patch
';


--
-- Name: create_event_instance_payment(public.event_instance); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.create_event_instance_payment(i public.event_instance) RETURNS public.payment
    LANGUAGE plpgsql
    AS $$
declare
  e event;
  payment payment;
  duration numeric(19, 4);
  counter int;
begin
  select * into payment from payment where event_instance_id = i.id;
  if found then
    return payment;
  end if;
  select * into e from event where id = i.event_id;
  if e.payment_type <> 'after_instance' or not exists (select * from event_registration where event_id=e.id) then
    return null;
  end if;

  duration := extract(epoch from (i.until - i.since)) / 60;

  insert into payment (accounting_period_id, status, event_instance_id, due_at)
  values ((select id from accounting_period where range @> now()), 'tentative', i.id, now() + '2 week'::interval)
  returning * into payment;

  insert into payment_recipient (payment_id, account_id, amount)
  select payment.id, account.id, (price).amount
  from event_instance_trainer
  join tenant_trainer on event_instance_trainer.person_id = tenant_trainer.person_id and tenant_trainer.tenant_id=current_tenant_id() and tenant_trainer.active_range @> now()
  join lateral coalesce(event_instance_trainer.lesson_price, ((tenant_trainer.member_price_45min).amount / 45 * duration, (tenant_trainer.member_price_45min).currency)::price) price on true
  join lateral person_account(tenant_trainer.person_id, (price).currency) account on true
  where event_instance_trainer.instance_id=i.id and (lesson_price is not null or member_price_45min is not null);

  get diagnostics counter = row_count;
  if counter <= 0 then
    insert into payment_recipient (payment_id, account_id, amount)
    select payment.id, account.id, (price).amount
    from event_trainer
    join tenant_trainer on event_trainer.person_id = tenant_trainer.person_id and tenant_trainer.tenant_id=current_tenant_id() and tenant_trainer.active_range @> now()
    join lateral coalesce(event_trainer.lesson_price, ((tenant_trainer.member_price_45min).amount / 45 * duration, (tenant_trainer.member_price_45min).currency)::price) price on true
    join lateral person_account(tenant_trainer.person_id, (price).currency) account on true
    where event_trainer.event_id=i.event_id and (lesson_price is not null or member_price_45min is not null);
  end if;

  if e.type = 'group' and current_tenant_id() = 2 then
  else
    insert into payment_debtor (payment_id, person_id)
    select payment.id, registrant.id
    from event
    join lateral event_registrants(event) registrant on true
    where event.id=i.event_id;
  end if;

  return payment;
end
$$;


--
-- Name: FUNCTION create_event_instance_payment(i public.event_instance); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.create_event_instance_payment(i public.event_instance) IS '@omit';


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
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE cohort_subscription; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.cohort_subscription IS '@omit create,update,delete
@simpleCollections only';


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
      phone
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
      p.phone
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
-- Name: current_couple_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_couple_ids() RETURNS bigint[]
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_couple_ids', true), ''), '[]', '{}')::bigint[];
$$;


--
-- Name: FUNCTION current_couple_ids(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.current_couple_ids() IS '@omit';


--
-- Name: current_person_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.current_person_ids() RETURNS bigint[]
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_person_ids', true), '[]'), '[]', '{}')::bigint[] || array[]::bigint[];
$$;


--
-- Name: FUNCTION current_person_ids(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.current_person_ids() IS '@omit';


--
-- Name: delete_event_instance(bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.delete_event_instance(id bigint, OUT deleted public.event_instance) RETURNS public.event_instance
    LANGUAGE plpgsql STRICT
    AS $_$
declare
  inst event_instance;
begin
  select * into inst from event_instance where event_instance.id = $1;
  if inst is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  delete from event_instance where event_instance.id=inst.id returning * into deleted;
  if (select count(*) < 2 from event_instance where event_instance.event_id = inst.event_id) then
    delete from event where event.id=inst.event_id;
  end if;
end
$_$;


--
-- Name: edit_registration(bigint, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.edit_registration(registration_id bigint, note text) RETURNS public.event_registration
    LANGUAGE plpgsql STRICT
    AS $_$
declare
  v_event event;
  reg event_registration;
begin
  select * into reg from event_registration where id = registration_id;
  select * into v_event from event where id = reg.event_id;

  if v_event is null or reg is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if v_event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if reg.person_id <> all (current_person_ids()) and reg.couple_id <> all (current_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  update event_registration set note=$2 where id = reg.id returning * into reg;
  return reg;
end;
$_$;


--
-- Name: event_instance_approx_price(public.event_instance); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_instance_approx_price(v_instance public.event_instance) RETURNS TABLE(amount numeric, currency text)
    LANGUAGE plpgsql STABLE
    AS $$
declare
  num_participants bigint;
  duration numeric;
begin
  num_participants := (select count(*) from event join lateral event_registrants(event.*) on true where event.id=v_instance.event_id);
  duration = extract(epoch from (v_instance.until - v_instance.since)) / 60;

  if exists (select 1 from event_instance_trainer where instance_id = v_instance.id) then
    return query
    select
      sum((tenant_trainer.member_price_45min).amount * duration / 45 / num_participants)::numeric(19,4) as amount,
      (tenant_trainer.member_price_45min).currency as currency
    from event_instance_trainer join tenant_trainer on event_instance_trainer.person_id=tenant_trainer.person_id
    where active and event_instance_trainer.instance_id=v_instance.id and tenant_trainer.tenant_id = event_instance_trainer.tenant_id
    group by (tenant_trainer.member_price_45min).currency;
  else
    return query
    select
      sum((tenant_trainer.member_price_45min).amount * duration / 45 / num_participants)::numeric(19,4) as amount,
      (tenant_trainer.member_price_45min).currency as currency
    from event_trainer join tenant_trainer on event_trainer.person_id=tenant_trainer.person_id
    where active and event_trainer.event_id=v_instance.event_id and tenant_trainer.tenant_id = event_trainer.tenant_id
    group by (tenant_trainer.member_price_45min).currency;
  end if;
end;
$$;


--
-- Name: FUNCTION event_instance_approx_price(v_instance public.event_instance); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.event_instance_approx_price(v_instance public.event_instance) IS '@simpleCollections only';


--
-- Name: event_instance_attendance_summary(public.event_instance); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_instance_attendance_summary(e public.event_instance) RETURNS TABLE(status public.attendance_type, count integer)
    LANGUAGE sql STABLE
    AS $$
  select status, count(status) as count from event_attendance where instance_id=e.id group by status;
$$;


--
-- Name: FUNCTION event_instance_attendance_summary(e public.event_instance); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.event_instance_attendance_summary(e public.event_instance) IS '@simpleCollections only';


--
-- Name: event_instance_trainer; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_instance_trainer (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    instance_id bigint NOT NULL,
    person_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    lesson_price public.price DEFAULT NULL::public.price_type
);


--
-- Name: TABLE event_instance_trainer; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_instance_trainer IS '@omit create,update,delete
@simpleCollections only';


--
-- Name: event_instance_trainer_name(public.event_instance_trainer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_instance_trainer_name(t public.event_instance_trainer) RETURNS text
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT (concat_ws(' '::text, person.prefix_title, person.first_name, person.last_name) ||
         CASE person.suffix_title
             WHEN ''::text THEN ''::text
             ELSE (', '::text || person.suffix_title)
         END)
    FROM public.person
   WHERE ((event_instance_trainer_name.t).person_id = person.id);
END;


--
-- Name: event_instances_for_range(public.event_type, timestamp with time zone, timestamp with time zone, boolean, bigint[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean DEFAULT false, trainer_ids bigint[] DEFAULT NULL::bigint[]) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT event_instance.id,
     event_instance.tenant_id,
     event_instance.event_id,
     event_instance.created_at,
     event_instance.updated_at,
     event_instance.since,
     event_instance.until,
     event_instance.range,
     event_instance.location_id,
     event_instance.is_cancelled
    FROM (public.event_instance
      JOIN public.event ON ((event_instance.event_id = event.id)))
   WHERE (event.is_visible AND (event_instance.since <= event_instances_for_range.end_range) AND (event_instance.until >= event_instances_for_range.start_range) AND ((event_instances_for_range.only_type IS NULL) OR (event.type = event_instances_for_range.only_type)) AND ((event_instances_for_range.trainer_ids IS NULL) OR (EXISTS ( SELECT 1
            FROM public.event_trainer
           WHERE ((event_trainer.person_id = ANY (event_instances_for_range.trainer_ids)) AND (event_trainer.event_id = event.id)))) OR (EXISTS ( SELECT 1
            FROM public.event_instance_trainer
           WHERE ((event_instance_trainer.person_id = ANY (event_instances_for_range.trainer_ids)) AND (event_instance_trainer.instance_id = event_instance.id))))));
END;


--
-- Name: FUNCTION event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean, trainer_ids bigint[]); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean, trainer_ids bigint[]) IS '@simpleCollections only';


--
-- Name: event_is_registration_open(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_is_registration_open(e public.event) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select not e.is_locked;
$$;


--
-- Name: event_my_registrations(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_my_registrations(e public.event) RETURNS SETOF public.event_registration
    LANGUAGE sql STABLE
    AS $$
  select * from event_registration
  where event_id = e.id
  and (person_id = any (current_person_ids())
    or couple_id = any (current_couple_ids()));
$$;


--
-- Name: FUNCTION event_my_registrations(e public.event); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.event_my_registrations(e public.event) IS '@simpleCollections only';


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
  ) order by last_name asc, first_name asc;
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
  select (
    select coalesce(sum(lessons_offered), 0) from event_trainer where event_id = e.id
  ) - (
    select coalesce(sum(lesson_count), 0) from event_registration join event_lesson_demand on registration_id = event_registration.id where event_id = e.id
  );
$$;


--
-- Name: event_remaining_person_spots(public.event); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_remaining_person_spots(e public.event) RETURNS integer
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  select e.capacity - (
    select coalesce(sum(case when couple_id is not null then 2 else 1 end), 0)
    from event_registration where event_id = e.id
  ) - (
    select coalesce(count(id), 0)
    from event_external_registration where event_id = e.id
  );
$$;


--
-- Name: event_trainer_lessons_remaining(public.event_trainer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_trainer_lessons_remaining(e public.event_trainer) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select e.lessons_offered - (
    select coalesce(sum(lesson_count), 0)
    from event_lesson_demand where trainer_id = e.id
  );
$$;


--
-- Name: event_trainer_name(public.event_trainer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.event_trainer_name(t public.event_trainer) RETURNS text
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT (concat_ws(' '::text, person.prefix_title, person.first_name, person.last_name) ||
         CASE person.suffix_title
             WHEN ''::text THEN ''::text
             ELSE (', '::text || person.suffix_title)
         END)
    FROM public.person
   WHERE ((event_trainer_name.t).person_id = person.id);
END;


--
-- Name: response_cache; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.response_cache (
    id bigint NOT NULL,
    url text NOT NULL,
    status integer NOT NULL,
    content text NOT NULL,
    content_type text NOT NULL,
    cached_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: TABLE response_cache; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.response_cache IS '@omit';


--
-- Name: fetch_with_cache(text, public.http_header[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fetch_with_cache(input_url text, headers public.http_header[] DEFAULT NULL::public.http_header[]) RETURNS public.response_cache
    LANGUAGE plpgsql
    AS $$
DECLARE
  new_response record;
  cached_response response_cache;
BEGIN
  SELECT * INTO cached_response FROM response_cache WHERE url = input_url;

  IF NOT FOUND THEN
    SELECT * INTO new_response FROM http(('GET', input_url, headers, NULL, NULL));

    INSERT INTO response_cache (url, status, content, content_type)
    VALUES (input_url, new_response.status, new_response.content, new_response.content_type)
    ON CONFLICT (url) DO UPDATE
    SET status = EXCLUDED.status, content = EXCLUDED.content, content_type = EXCLUDED.content_type, cached_at = NOW()
    RETURNING * INTO cached_response;
  END IF;

  RETURN cached_response;
END;
$$;


--
-- Name: FUNCTION fetch_with_cache(input_url text, headers public.http_header[]); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fetch_with_cache(input_url text, headers public.http_header[]) IS '@omit';


--
-- Name: filtered_people(boolean, boolean, bigint[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.filtered_people(is_trainer boolean, is_admin boolean, in_cohorts bigint[] DEFAULT NULL::bigint[]) RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
  select person.* from person
  join auth_details on person_id=person.id
  where
    current_tenant_id() = any (auth_details.allowed_tenants)
    and case when in_cohorts is null then true else in_cohorts = auth_details.cohort_memberships OR in_cohorts && auth_details.cohort_memberships end
    and case when is_trainer is null then true else is_trainer = (current_tenant_id() = any (auth_details.tenant_trainers)) end
    and case when is_admin is null then true else is_admin = (current_tenant_id() = any (auth_details.tenant_administrators)) end
  order by last_name, first_name
$$;


--
-- Name: FUNCTION filtered_people(is_trainer boolean, is_admin boolean, in_cohorts bigint[]); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.filtered_people(is_trainer boolean, is_admin boolean, in_cohorts bigint[]) IS '@simpleCollections only';


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
  select email from person_invitation where access_token=token and used_at is null;
$$;


--
-- Name: log_in_as(bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.log_in_as(id bigint, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $_$
begin
  select users.* into usr from users where users.id=$1;
  jwt := app_private.create_jwt_token(usr);
end
$_$;


--
-- Name: login(character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.login(login character varying, passwd character varying, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_salt varchar;
begin
  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  login := trim(login);
  select users.* into usr from users where lower(u_login) = lower(login) and u_pass = encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') limit 1;
  if usr is null then
    select users.* into usr from users where lower(u_email) = lower(login) and u_pass = encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') limit 1;
  end if;

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
end;
$$;


--
-- Name: move_event_instance(bigint, timestamp with time zone, timestamp with time zone, bigint, bigint, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.move_event_instance(id bigint, since timestamp with time zone, until timestamp with time zone, trainer_person_id bigint, location_id bigint, location_text text) RETURNS public.event_instance
    LANGUAGE plpgsql
    AS $$
declare
  inst event_instance;
begin
  select * from event_instance into inst where event_instance.id = move_event_instance.id;

  if location_id is not null then
    update event set location_id = move_event_instance.location_id where event.id = inst.event_id;
  end if;
  if location_text is not null then
    update event set location_text = move_event_instance.location_text where event.id = inst.event_id;
  end if;

  if trainer_person_id is not null then
    if (select count(*) = 1 from event_instance_trainer where instance_id = inst.id) then
      update event_instance_trainer set person_id = trainer_person_id where instance_id = inst.id;
    elsif (select count(*) = 1 from event_trainer where event_id = inst.event_id) then
      update event_trainer set person_id = trainer_person_id where event_id = inst.event_id;
    end if;
  end if;

  update event_instance set since=move_event_instance.since, until=move_event_instance.until where event_instance.id=inst.id
  returning * into inst;
  return inst;
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
-- Name: my_event_instances_for_range(public.event_type, timestamp with time zone, timestamp with time zone, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean DEFAULT false) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT DISTINCT ON (instances.id) instances.id,
     instances.tenant_id,
     instances.event_id,
     instances.created_at,
     instances.updated_at,
     instances.since,
     instances.until,
     instances.range,
     instances.location_id,
     instances.is_cancelled
    FROM (((public.event_instances_for_range(my_event_instances_for_range.only_type, my_event_instances_for_range.start_range, my_event_instances_for_range.end_range) instances(id, tenant_id, event_id, created_at, updated_at, since, until, range, location_id, is_cancelled)
      LEFT JOIN public.event_registration ON (((event_registration.event_id = instances.event_id) AND ((event_registration.person_id = ANY (public.current_person_ids())) OR (event_registration.couple_id = ANY (public.current_couple_ids()))))))
      LEFT JOIN public.event_trainer ON (((event_trainer.event_id = instances.event_id) AND (event_trainer.person_id = ANY (public.current_person_ids())))))
      LEFT JOIN public.event_instance_trainer ON (((event_instance_trainer.instance_id = instances.id) AND (event_instance_trainer.person_id = ANY (public.current_person_ids())))))
   WHERE ((event_registration.id IS NOT NULL) OR (event_trainer.id IS NOT NULL) OR (event_instance_trainer.id IS NOT NULL));
END;


--
-- Name: FUNCTION my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean) IS '@simpleCollections only';


--
-- Name: my_tenant_ids(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.my_tenant_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE ROWS 5
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
-- Name: otp_login(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.otp_login(token uuid, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_token otp_token;
begin
  select * into v_token from otp_token where access_token = token and used_at is null and expires_at > now();
  if not found then
    raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
  end if;
  select * into usr from users where u_id = v_token.user_id;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);

  update users set last_login = now() where id = usr.id;
  update otp_token set used_at = now() where id = v_token.id;
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

COMMENT ON TABLE public.payment_debtor IS '@omit create,update,delete
@simpleCollections only';


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

COMMENT ON FUNCTION public.payment_debtor_is_unpaid(p public.payment_debtor) IS '@filterable';


--
-- Name: payment_debtor_price(public.payment_debtor); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.payment_debtor_price(p public.payment_debtor) RETURNS public.price_type
    LANGUAGE sql STABLE
    AS $$
  SELECT (
    sum(payment_recipient.amount) / (
      SELECT count(*) AS count
      FROM public.payment_debtor
      WHERE p.payment_id = payment_debtor.payment_id
    )::numeric(19,4),
    min(account.currency)::text
  )::price
  FROM payment_recipient
  JOIN account ON payment_recipient.account_id = account.id
  WHERE payment_recipient.payment_id = p.payment_id;
$$;


--
-- Name: FUNCTION payment_debtor_price(p public.payment_debtor); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.payment_debtor_price(p public.payment_debtor) IS '@simpleCollections only';


--
-- Name: person_account(bigint, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_account(p_id bigint, c text, OUT acc public.account) RETURNS public.account
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  select * into acc from account
  where person_id=p_id and currency=c and tenant_id=current_tenant_id();

  if not found then
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), p_id, coalesce(c, 'CZK'))
    on conflict on constraint account_tenant_id_person_id_currency_idx do nothing
    returning * into acc;
  end if;
end;
$$;


--
-- Name: person_active_couples(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_active_couples(p public.person) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT couple.id,
     couple.man_id,
     couple.woman_id,
     couple.since,
     couple.until,
     couple.created_at,
     couple.updated_at,
     couple.legacy_pary_id,
     couple.active_range,
     couple.status,
     couple.active
    FROM public.couple
   WHERE (((couple.man_id = (person_active_couples.p).id) OR (couple.woman_id = (person_active_couples.p).id)) AND couple.active)
   ORDER BY couple.active_range;
END;


--
-- Name: FUNCTION person_active_couples(p public.person); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.person_active_couples(p public.person) IS '@simpleCollections only';


--
-- Name: person_all_couples(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_all_couples(p public.person) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT couple.id,
     couple.man_id,
     couple.woman_id,
     couple.since,
     couple.until,
     couple.created_at,
     couple.updated_at,
     couple.legacy_pary_id,
     couple.active_range,
     couple.status,
     couple.active
    FROM public.couple
   WHERE ((couple.man_id = (person_all_couples.p).id) OR (couple.woman_id = (person_all_couples.p).id))
   ORDER BY couple.active_range;
END;


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
-- Name: person_is_admin(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_is_admin(p public.person) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select current_tenant_id() = any (tenant_administrators) from auth_details where person_id=p.id;
$$;


--
-- Name: person_is_member(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_is_member(p public.person) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select current_tenant_id() = any (tenant_memberships) from auth_details where person_id=p.id;
$$;


--
-- Name: person_is_trainer(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_is_trainer(p public.person) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select current_tenant_id() = any (tenant_trainers) from auth_details where person_id=p.id;
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
-- Name: FUNCTION person_name(p public.person); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.person_name(p public.person) IS '@omit';


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
-- Name: person_recent_attendance(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_recent_attendance(p public.person) RETURNS SETOF public.event_attendance
    LANGUAGE sql STABLE
    AS $$
  select event_attendance.*
  from event_attendance
  join event_instance on instance_id=event_instance.id
  where person_id = p.id and since <= (CURRENT_DATE + interval '1 day')
  order by since desc
  limit 20
$$;


--
-- Name: FUNCTION person_recent_attendance(p public.person); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.person_recent_attendance(p public.person) IS '@simpleCollections only';


--
-- Name: person_weekly_attendance(public.person); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.person_weekly_attendance(p public.person) RETURNS TABLE(week date, event_count integer)
    LANGUAGE sql STABLE
    AS $$
  select date_trunc('week', since) as week, count(*) as count
  from event_attendance
  join event_instance on instance_id=event_instance.id
  where person_id = p.id and since <= (CURRENT_DATE + interval '1 day')
  group by date_trunc('week', since)
$$;


--
-- Name: FUNCTION person_weekly_attendance(p public.person); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.person_weekly_attendance(p public.person) IS '@simpleCollections only';


--
-- Name: refresh_jwt(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.refresh_jwt() RETURNS public.jwt_token
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  SELECT app_private.create_jwt_token(users) FROM users WHERE u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$;


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
  if registration.person_id <> all (current_person_ids()) and registration.couple_id <> all (current_couple_ids()) then
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

COMMENT ON FUNCTION public.register_to_event(INOUT registration public.event_registration, lessons public.event_lesson_demand[]) IS '@arg0variant input
@arg1variant patch';


--
-- Name: register_to_event_many(public.register_to_event_type[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.register_to_event_many(registrations public.register_to_event_type[]) RETURNS SETOF public.event_registration
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
declare
  event event;
  created_ids bigint[] := array[]::bigint[];
  registration register_to_event_type;
  created event_registration;
  demand event_lesson_demand;
begin
  foreach registration in array registrations loop
    select * into event from event where id = registration.event_id;

    if event is null then
      raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
    end if;
    if event.is_locked = true then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;
    if registration.person_id <> all (current_person_ids()) and registration.couple_id <> all (current_couple_ids()) then
      raise exception 'ACCESS_DENIED' using errcode = '42501';
    end if;

    insert into event_registration (event_id, person_id, couple_id, note)
    values (registration.event_id, registration.person_id, registration.couple_id, registration.note)
    returning * into created;
    created_ids := created_ids || created.id;
    foreach demand in array registration.lessons loop
      perform set_lesson_demand(created.id, demand.trainer_id, demand.lesson_count);
    end loop;
  end loop;
  return query select * from event_registration where id = any (created_ids);
end;
$$;


--
-- Name: register_using_invitation(text, text, text, uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.register_using_invitation(email text, login text, passwd text, token uuid, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
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
  insert into users (u_login, u_email, u_pass) values (trim(login), email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  insert into user_proxy (user_id, person_id) values (usr.id, invitation.person_id);
  update person_invitation set used_at=now() where access_token=token;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
end
$$;


--
-- Name: register_without_invitation(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.register_without_invitation(email text, login text, passwd text, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_salt text;
begin
  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  insert into users (u_login, u_email, u_pass) values (trim(login), email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
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
    csts_id text,
    wdsf_id text,
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

COMMENT ON TABLE public.membership_application IS '@simpleCollections only';


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
    AS $$
declare
  v_tenant tenant;
  v_user users;
  v_token otp_token;
  v_people jsonb;
  v_payload jsonb := null;
begin
  for v_user in (select * from users where u_email = email) loop
    insert into otp_token (user_id)
    values (v_user.u_id) returning * into v_token;

    select jsonb_agg(person_name(person.*)) into v_people
    from user_proxy join person on person_id=person.id
    where active and user_id = v_user.u_id;

    v_payload := coalesce(v_payload, jsonb_build_array()) || jsonb_build_object(
      'login', v_user.u_login,
      'email', v_user.u_email,
      'token', v_token.access_token,
      'people', v_people
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
      if trainer.member_payout_45min is not null then
        actual_payout := remaining_amount * (trainer.member_payout_45min).amount / (trainer.member_price_45min).amount;
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

COMMENT ON FUNCTION public.resolve_payment_with_credit(p public.payment) IS '@omit';


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
-- Name: tenant_account(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.tenant_account(c text, OUT acc public.account) RETURNS public.account
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  select * into acc from account
  where person_id is null and currency=c and tenant_id=current_tenant_id();

  if not found then
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), null, coalesce(c, 'CZK'))
    on conflict on constraint account_tenant_id_person_id_currency_idx do nothing
    returning * into acc;
  end if;
end;
$$;


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
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    active boolean GENERATED ALWAYS AS ((status = 'active'::public.relationship_status)) STORED NOT NULL
);


--
-- Name: TABLE tenant_membership; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant_membership IS '@simpleCollections only';


--
-- Name: COLUMN tenant_membership.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.tenant_membership.active_range IS '@omit';


--
-- Name: tenant_couples(public.tenant); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.tenant_couples(t public.tenant) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT DISTINCT couple.id,
     couple.man_id,
     couple.woman_id,
     couple.since,
     couple.until,
     couple.created_at,
     couple.updated_at,
     couple.legacy_pary_id,
     couple.active_range,
     couple.status,
     couple.active
    FROM (public.couple
      JOIN public.tenant_membership ON (((couple.man_id = tenant_membership.person_id) OR (couple.woman_id = tenant_membership.person_id))))
   WHERE (couple.active AND tenant_membership.active AND (tenant_membership.tenant_id = (tenant_couples.t).id))
   ORDER BY couple.active_range;
END;


--
-- Name: FUNCTION tenant_couples(t public.tenant); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.tenant_couples(t public.tenant) IS '@simpleCollections only';


--
-- Name: update_event_attendance(bigint, bigint, public.attendance_type, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.update_event_attendance(instance_id bigint, person_id bigint, status public.attendance_type, note text) RETURNS public.event_attendance
    LANGUAGE plpgsql
    AS $_$
declare
  att event_attendance;
  reg event_registration;
begin
  select event_registration.* into reg
  from event_registration
  join event_instance on event_registration.event_id=event_instance.event_id
  left join couple on couple_id=couple.id
  where event_instance.id=$1 and $2 in (event_registration.person_id, man_id, woman_id);

  insert into event_attendance (registration_id, instance_id, person_id, status, note)
  values (reg.id, $1, $2, $3, $4)
  on conflict on constraint event_attendance_unique_event_person_key do update set status=$3, note=$4
  returning * into att;
  return att;
end
$_$;


--
-- Name: upsert_event(public.event, public.event_instance[], public.event_trainer[], public.event_target_cohort[], public.event_registration[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.upsert_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]) RETURNS public.event
    LANGUAGE plpgsql
    AS $$
declare
  instance event_instance;
  trainer event_trainer;
  cohort event_target_cohort;
  registration event_registration;
begin
  if info.id is not null then
    update event set name=info.name, summary=info.summary, description=info.description, type=info.type, location_id=info.location_id, location_text=info.location_text, capacity=info.capacity, is_visible=info.is_visible, is_public=info.is_public, is_locked=info.is_locked, enable_notes=info.enable_notes where id=info.id;
  else
    insert into event (name, summary, description, type, location_id, location_text, capacity, is_visible, is_public, is_locked, enable_notes)
    values (info.name, info.summary, info.description, info.type, info.location_id, info.location_text, info.capacity, info.is_visible, info.is_public, info.is_locked, info.enable_notes)
    returning * into info;
  end if;

  foreach instance in array instances loop
    if instance.id is not null then
      if instance.since is null and instance.until is null then
        delete from event_instance where id=instance.id;
      else
        update event_instance set since=instance.since, until=instance.until where id=instance.id;
      end if;
    else
      insert into event_instance (event_id, since, until) values (info.id, instance.since, instance.until);
    end if;
  end loop;

  foreach trainer in array trainers loop
    if trainer.id is not null then
      if trainer.person_id is null then
        delete from event_trainer where id=trainer.id;
      else
        update event_trainer set lessons_offered=trainer.lessons_offered where id=trainer.id;
      end if;
    else
      insert into event_trainer (event_id, person_id, lessons_offered) values (info.id, trainer.person_id, coalesce(trainer.lessons_offered, 0));
    end if;
  end loop;

  foreach cohort in array cohorts loop
    if cohort.id is not null then
      if cohort.cohort_id is null then
        delete from event_target_cohort where id=cohort.id;
      end if;
    else
      insert into event_target_cohort (event_id, cohort_id) values (info.id, cohort.cohort_id);
    end if;
  end loop;

  foreach registration in array registrations loop
    if registration.id is not null then
      if registration.person_id is null and registration.couple_id is null then
        delete from event_registration where id=registration.id;
      else
        update event_registration
        set person_id=registration.person_id, couple_id=registration.couple_id
        where id=registration.id;
      end if;
    else
      insert into event_registration (event_id, person_id, couple_id)
      values (info.id, registration.person_id, registration.couple_id);
    end if;
  end loop;
end;
$$;


--
-- Name: FUNCTION upsert_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.upsert_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]) IS '@arg0variant patch
@arg1variant patch
@arg2variant patch
@arg3variant patch
@arg4variant patch
';


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
-- Name: array_accum(anycompatiblearray); Type: AGGREGATE; Schema: app_private; Owner: -
--

CREATE AGGREGATE app_private.array_accum(anycompatiblearray) (
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
-- Name: pary_navrh; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.pary_navrh (
    pn_id bigint NOT NULL,
    pn_navrhl bigint NOT NULL,
    pn_partner bigint NOT NULL,
    pn_partnerka bigint NOT NULL,
    id bigint GENERATED ALWAYS AS (pn_id) STORED NOT NULL
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
-- Name: skupiny; Type: TABLE; Schema: app_private; Owner: -
--

CREATE TABLE app_private.skupiny (
    s_id bigint NOT NULL,
    s_name text NOT NULL,
    s_description text DEFAULT ''::text NOT NULL,
    s_color_rgb text NOT NULL,
    s_location text DEFAULT ''::text NOT NULL,
    s_visible boolean DEFAULT true NOT NULL,
    ordering integer DEFAULT 1 NOT NULL,
    cohort_group bigint,
    id bigint GENERATED ALWAYS AS (s_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);


--
-- Name: skupiny_s_id_seq; Type: SEQUENCE; Schema: app_private; Owner: -
--

CREATE SEQUENCE app_private.skupiny_s_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: skupiny_s_id_seq; Type: SEQUENCE OWNED BY; Schema: app_private; Owner: -
--

ALTER SEQUENCE app_private.skupiny_s_id_seq OWNED BY app_private.skupiny.s_id;


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
    range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE accounting_period; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.accounting_period IS '@omit';


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
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now(),
    id bigint GENERATED ALWAYS AS (at_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    title_photo_url text,
    at_timestamp timestamp with time zone GENERATED ALWAYS AS (updated_at) STORED NOT NULL,
    at_timestamp_add timestamp with time zone GENERATED ALWAYS AS (created_at) STORED NOT NULL
);


--
-- Name: COLUMN aktuality.at_kat; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.aktuality.at_kat IS '@deprecated';


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
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    active boolean GENERATED ALWAYS AS ((status = 'active'::public.relationship_status)) STORED NOT NULL
);


--
-- Name: TABLE tenant_administrator; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant_administrator IS '@simpleCollections only';


--
-- Name: COLUMN tenant_administrator.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.tenant_administrator.active_range IS '@omit';


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
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL,
    member_price_45min public.price DEFAULT NULL::public.price_type,
    member_payout_45min public.price DEFAULT NULL::public.price_type,
    guest_price_45min public.price DEFAULT NULL::public.price_type,
    guest_payout_45min public.price DEFAULT NULL::public.price_type,
    create_payout_payments boolean DEFAULT true NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    active boolean GENERATED ALWAYS AS ((status = 'active'::public.relationship_status)) STORED NOT NULL
);


--
-- Name: TABLE tenant_trainer; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant_trainer IS '@simpleCollections only';


--
-- Name: COLUMN tenant_trainer.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.tenant_trainer.active_range IS '@omit';


--
-- Name: auth_details_view; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.auth_details_view AS
 SELECT person.id AS person_id,
    array_remove(array_agg(couple.id), NULL::bigint) AS couple_ids,
    array_remove(array_agg(cohort_membership.cohort_id), NULL::bigint) AS cohort_memberships,
    array_remove(array_agg(tenant_membership.tenant_id), NULL::bigint) AS tenant_memberships,
    array_remove(array_agg(tenant_trainer.tenant_id), NULL::bigint) AS tenant_trainers,
    array_remove(array_agg(tenant_administrator.tenant_id), NULL::bigint) AS tenant_administrators,
    array_remove(((array_agg(tenant_administrator.tenant_id) || array_agg(tenant_trainer.tenant_id)) || array_agg(tenant_membership.tenant_id)), NULL::bigint) AS allowed_tenants
   FROM (((((public.person
     LEFT JOIN public.couple ON ((((person.id = couple.man_id) OR (person.id = couple.woman_id)) AND (couple.status = 'active'::public.relationship_status))))
     LEFT JOIN public.cohort_membership ON (((person.id = cohort_membership.person_id) AND (cohort_membership.status = 'active'::public.relationship_status))))
     LEFT JOIN public.tenant_membership ON (((person.id = tenant_membership.person_id) AND (tenant_membership.status = 'active'::public.relationship_status))))
     LEFT JOIN public.tenant_trainer ON (((person.id = tenant_trainer.person_id) AND (tenant_trainer.status = 'active'::public.relationship_status))))
     LEFT JOIN public.tenant_administrator ON (((person.id = tenant_administrator.person_id) AND (tenant_administrator.status = 'active'::public.relationship_status))))
  GROUP BY person.id;


--
-- Name: VIEW auth_details_view; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.auth_details_view IS '@omit';


--
-- Name: auth_details; Type: MATERIALIZED VIEW; Schema: public; Owner: -
--

CREATE MATERIALIZED VIEW public.auth_details AS
 SELECT person_id,
    couple_ids,
    cohort_memberships,
    tenant_memberships,
    tenant_trainers,
    tenant_administrators,
    allowed_tenants
   FROM public.auth_details_view
  WITH NO DATA;


--
-- Name: MATERIALIZED VIEW auth_details; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON MATERIALIZED VIEW public.auth_details IS '@omit';


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
    ordering integer DEFAULT 1 NOT NULL
);


--
-- Name: TABLE cohort; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.cohort IS '@simpleCollections only';


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
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    id bigint GENERATED ALWAYS AS (d_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    d_timestamp timestamp with time zone GENERATED ALWAYS AS (updated_at) STORED
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
-- Name: event_external_registration; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_external_registration (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    first_name text NOT NULL,
    last_name text NOT NULL,
    prefix_title text DEFAULT ''::text NOT NULL,
    suffix_title text DEFAULT ''::text NOT NULL,
    nationality text NOT NULL,
    birth_date date,
    tax_identification_number text,
    email public.citext NOT NULL,
    phone text NOT NULL,
    note text,
    created_by bigint DEFAULT public.current_user_id(),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE event_external_registration; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.event_external_registration IS '@omit update
@simpleCollections only';


--
-- Name: event_external_registration_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.event_external_registration ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.event_external_registration_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


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
-- Name: event_instance_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.event_instance ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.event_instance_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


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

ALTER TABLE public.event_registration ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.event_registration_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


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

ALTER TABLE public.event_trainer ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
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
    id bigint GENERATED ALWAYS AS (gd_id) STORED NOT NULL,
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
    updated_at timestamp with time zone,
    id bigint GENERATED ALWAYS AS (gf_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    gf_timestamp timestamp with time zone GENERATED ALWAYS AS (updated_at) STORED
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

COMMENT ON TABLE public.payment_recipient IS '@omit create,update,delete
@simpleCollections only';


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
    email public.citext NOT NULL
);


--
-- Name: TABLE person_invitation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.person_invitation IS '@omit update
@simpleCollections only';


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
-- Name: pghero_query_stats; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pghero_query_stats (
    id bigint NOT NULL,
    database text,
    "user" text,
    query text,
    query_hash bigint,
    total_time double precision,
    calls bigint,
    captured_at timestamp without time zone
);


--
-- Name: TABLE pghero_query_stats; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.pghero_query_stats IS '@omit';


--
-- Name: pghero_query_stats_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.pghero_query_stats_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: pghero_query_stats_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.pghero_query_stats_id_seq OWNED BY public.pghero_query_stats.id;


--
-- Name: pghero_space_stats; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pghero_space_stats (
    id bigint NOT NULL,
    database text,
    schema text,
    relation text,
    size bigint,
    captured_at timestamp without time zone
);


--
-- Name: TABLE pghero_space_stats; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.pghero_space_stats IS '@omit';


--
-- Name: pghero_space_stats_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.pghero_space_stats_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: pghero_space_stats_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.pghero_space_stats_id_seq OWNED BY public.pghero_space_stats.id;


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
    id bigint GENERATED ALWAYS AS (pc_id) STORED NOT NULL,
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
    id bigint GENERATED ALWAYS AS (pcg_id) STORED NOT NULL,
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
    id bigint GENERATED ALWAYS AS (pg_id) STORED NOT NULL,
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
    id bigint GENERATED ALWAYS AS (pgs_id) STORED NOT NULL,
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
    id bigint GENERATED ALWAYS AS (pi_id) STORED NOT NULL,
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
    id bigint GENERATED ALWAYS AS (pr_id) STORED NOT NULL,
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
-- Name: response_cache_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.response_cache ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.response_cache_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: room; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.room (
    id bigint NOT NULL,
    name text NOT NULL,
    description jsonb NOT NULL,
    location_id bigint
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
          WHERE (cohort_membership.active AND (cohort_membership.tenant_id = public.current_tenant_id()))
        ), attendances AS (
         SELECT event_attendance.person_id,
                CASE
                    WHEN (event.type = 'lesson'::public.event_type) THEN 1
                    ELSE 0
                END AS lesson_score,
                CASE
                    WHEN (event.type = 'group'::public.event_type) THEN floor(((EXTRACT(epoch FROM (i.until - i.since)) / (60)::numeric) / (45)::numeric))
                    ELSE (0)::numeric
                END AS group_score,
                CASE
                    WHEN (event.type = 'camp'::public.event_type) THEN (3 + (2 * ((EXTRACT(epoch FROM (i.until - i.since)) > (86400)::numeric))::integer))
                    ELSE 0
                END AS event_score,
            i.since
           FROM (((public.event_attendance
             JOIN public.event_registration ON ((event_registration.id = event_attendance.registration_id)))
             JOIN public.event ON ((event.id = event_registration.event_id)))
             JOIN public.event_instance i ON ((event_attendance.instance_id = i.id)))
          WHERE (((event_attendance.status = 'attended'::public.attendance_type) OR (event.type = 'lesson'::public.event_type)) AND (event.type <> 'reservation'::public.event_type) AND (NOT i.is_cancelled) AND (i.since > '2023-09-01 00:00:00+00'::timestamp with time zone) AND (i.until < date_trunc('day'::text, now())) AND (event_attendance.person_id IN ( SELECT members.id
                   FROM members)))
        ), per_day AS (
         SELECT attendances.person_id,
            LEAST(sum(attendances.lesson_score), (4)::bigint) AS lesson_score,
            sum(attendances.group_score) AS group_score,
            sum(attendances.event_score) AS event_score,
            (((LEAST(sum(attendances.lesson_score), (4)::bigint))::numeric + sum(attendances.group_score)) + (sum(attendances.event_score))::numeric) AS total_score,
            attendances.since
           FROM attendances
          GROUP BY attendances.person_id, attendances.since
        )
 SELECT person_id,
    (sum(lesson_score))::bigint AS lesson_total_score,
    (sum(group_score))::bigint AS group_total_score,
    (sum(event_score))::bigint AS event_total_score,
    (sum((((lesson_score)::numeric + group_score) + (event_score)::numeric)))::bigint AS total_score,
    rank() OVER (ORDER BY (sum(((lesson_score)::numeric + group_score))) DESC) AS ranking
   FROM per_day
  GROUP BY person_id
  ORDER BY ((sum((((lesson_score)::numeric + group_score) + (event_score)::numeric)))::bigint) DESC;


--
-- Name: VIEW scoreboard; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.scoreboard IS '@foreignKey (person_id) references person (id)
@simpleCollections only';


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
    id bigint NOT NULL,
    name text NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    address public.address_domain,
    is_public boolean DEFAULT true,
    tenant_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE tenant_location; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.tenant_location IS '@simpleCollections only';


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
-- Name: upozorneni_skupiny; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.upozorneni_skupiny (
    ups_id bigint NOT NULL,
    ups_id_rodic bigint NOT NULL,
    ups_id_skupina bigint NOT NULL,
    ups_color text NOT NULL,
    id bigint GENERATED ALWAYS AS (ups_id) STORED NOT NULL,
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
    id bigint NOT NULL,
    since timestamp with time zone,
    until timestamp with time zone,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    active boolean GENERATED ALWAYS AS ((status = 'active'::public.relationship_status)) STORED NOT NULL
);


--
-- Name: TABLE user_proxy; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_proxy IS '@simpleCollections only';


--
-- Name: COLUMN user_proxy.active_range; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_proxy.active_range IS '@omit';


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
-- Name: skupiny s_id; Type: DEFAULT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.skupiny ALTER COLUMN s_id SET DEFAULT nextval('app_private.skupiny_s_id_seq'::regclass);


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
-- Name: pghero_query_stats id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pghero_query_stats ALTER COLUMN id SET DEFAULT nextval('public.pghero_query_stats_id_seq'::regclass);


--
-- Name: pghero_space_stats id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pghero_space_stats ALTER COLUMN id SET DEFAULT nextval('public.pghero_space_stats_id_seq'::regclass);


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

COMMENT ON MATERIALIZED VIEW public.account_balances IS '@omit';


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
-- Name: pary_navrh idx_23840_primary; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.pary_navrh
    ADD CONSTRAINT idx_23840_primary PRIMARY KEY (pn_id);


--
-- Name: skupiny idx_23934_primary; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.skupiny
    ADD CONSTRAINT idx_23934_primary PRIMARY KEY (s_id);


--
-- Name: pary_navrh pary_navrh_unique_id; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.pary_navrh
    ADD CONSTRAINT pary_navrh_unique_id UNIQUE (id);


--
-- Name: skupiny skupiny_unique_id; Type: CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.skupiny
    ADD CONSTRAINT skupiny_unique_id UNIQUE (id);


--
-- Name: account account_tenant_id_person_id_currency_idx; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_tenant_id_person_id_currency_idx UNIQUE NULLS NOT DISTINCT (tenant_id, person_id, currency);


--
-- Name: accounting_period accounting_period_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.accounting_period
    ADD CONSTRAINT accounting_period_pkey PRIMARY KEY (id);


--
-- Name: aktuality aktuality_unique_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_unique_id UNIQUE (id);


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
-- Name: couple couple_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.couple
    ADD CONSTRAINT couple_pkey PRIMARY KEY (id);


--
-- Name: dokumenty dokumenty_unique_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dokumenty
    ADD CONSTRAINT dokumenty_unique_id UNIQUE (id);


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
-- Name: event_external_registration event_external_registration_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_external_registration
    ADD CONSTRAINT event_external_registration_pkey PRIMARY KEY (id);


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
-- Name: event_trainer event_trainer_trainer_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_trainer_id_key UNIQUE (event_id, person_id);


--
-- Name: form_responses form_responses_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.form_responses
    ADD CONSTRAINT form_responses_pkey PRIMARY KEY (id);


--
-- Name: galerie_dir galerie_dir_unique_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.galerie_dir
    ADD CONSTRAINT galerie_dir_unique_id UNIQUE (id);


--
-- Name: galerie_foto galerie_foto_unique_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.galerie_foto
    ADD CONSTRAINT galerie_foto_unique_id UNIQUE (id);


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
-- Name: pghero_query_stats pghero_query_stats_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pghero_query_stats
    ADD CONSTRAINT pghero_query_stats_pkey PRIMARY KEY (id);


--
-- Name: pghero_space_stats pghero_space_stats_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pghero_space_stats
    ADD CONSTRAINT pghero_space_stats_pkey PRIMARY KEY (id);


--
-- Name: platby_category_group platby_category_group_unique_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_category_group
    ADD CONSTRAINT platby_category_group_unique_id UNIQUE (id);


--
-- Name: platby_category platby_category_unique_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_category
    ADD CONSTRAINT platby_category_unique_id UNIQUE (id);


--
-- Name: platby_group_skupina platby_group_skupina_unique_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_unique_id UNIQUE (id);


--
-- Name: platby_group platby_group_unique_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group
    ADD CONSTRAINT platby_group_unique_id UNIQUE (id);


--
-- Name: platby_item platby_item_unique_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_item
    ADD CONSTRAINT platby_item_unique_id UNIQUE (id);


--
-- Name: platby_raw platby_raw_unique_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_raw
    ADD CONSTRAINT platby_raw_unique_id UNIQUE (id);


--
-- Name: posting posting_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posting
    ADD CONSTRAINT posting_pkey PRIMARY KEY (id);


--
-- Name: response_cache response_cache_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.response_cache
    ADD CONSTRAINT response_cache_pkey PRIMARY KEY (id);


--
-- Name: response_cache response_cache_url_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.response_cache
    ADD CONSTRAINT response_cache_url_key UNIQUE (url);


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
-- Name: transaction transaction_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_pkey PRIMARY KEY (id);


--
-- Name: upozorneni_skupiny upozorneni_skupiny_unique_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT upozorneni_skupiny_unique_id UNIQUE (id);


--
-- Name: upozorneni upozorneni_unique_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni
    ADD CONSTRAINT upozorneni_unique_id UNIQUE (id);


--
-- Name: user_proxy user_proxy_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_proxy
    ADD CONSTRAINT user_proxy_pkey PRIMARY KEY (id);


--
-- Name: users users_unique_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_unique_id UNIQUE (id);


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
-- Name: account_balances_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX account_balances_id_idx ON public.account_balances USING btree (id);


--
-- Name: auth_details_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX auth_details_person_id_idx ON public.auth_details USING btree (person_id);


--
-- Name: cohort_membership_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_membership_active_idx ON public.cohort_membership USING btree (active);


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
-- Name: cohort_membership_status_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX cohort_membership_status_idx ON public.cohort_membership USING btree (status);


--
-- Name: couple_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX couple_active_idx ON public.couple USING btree (active);


--
-- Name: couple_range_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX couple_range_idx ON public.couple USING gist (active_range, man_id, woman_id);


--
-- Name: event_attendance_instance_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_attendance_instance_id_idx ON public.event_attendance USING btree (instance_id);


--
-- Name: event_attendance_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_attendance_person_id_idx ON public.event_attendance USING btree (person_id);


--
-- Name: event_instance_event_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_instance_event_id_idx ON public.event_instance USING btree (event_id);


--
-- Name: event_instance_range_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_instance_range_idx ON public.event_instance USING btree (tenant_id, since, until);


--
-- Name: event_instance_since_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_instance_since_idx ON public.event_instance USING btree (since);


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
-- Name: event_lesson_demand_registration_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_lesson_demand_registration_id_idx ON public.event_lesson_demand USING btree (registration_id);


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
-- Name: event_registration_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_person_id_idx ON public.event_registration USING btree (person_id);


--
-- Name: event_registration_target_cohort_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_registration_target_cohort_id_idx ON public.event_registration USING btree (target_cohort_id);


--
-- Name: event_target_cohort_cohort_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_target_cohort_cohort_id_idx ON public.event_target_cohort USING btree (cohort_id);


--
-- Name: event_target_cohort_event_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_target_cohort_event_id_idx ON public.event_target_cohort USING btree (event_id);


--
-- Name: event_trainer_event_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_trainer_event_idx ON public.event_trainer USING btree (event_id, person_id);


--
-- Name: event_trainer_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_trainer_person_id_idx ON public.event_trainer USING btree (person_id);


--
-- Name: event_trainer_tenant_event_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX event_trainer_tenant_event_idx ON public.event_trainer USING btree (tenant_id, event_id);


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

CREATE INDEX idx_23943_up_timestamp_add ON public.upozorneni USING btree (created_at);


--
-- Name: idx_23955_upozorneni_skupiny_ups_id_rodic_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23955_upozorneni_skupiny_ups_id_rodic_fkey ON public.upozorneni_skupiny USING btree (ups_id_rodic);


--
-- Name: idx_23955_upozorneni_skupiny_ups_id_skupina_fkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_23955_upozorneni_skupiny_ups_id_skupina_fkey ON public.upozorneni_skupiny USING btree (ups_id_skupina);


--
-- Name: idx_cm_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_cm_tenant ON public.cohort_membership USING btree (tenant_id);


--
-- Name: idx_e_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_e_tenant ON public.event USING btree (tenant_id);


--
-- Name: idx_event_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_event_tenant ON public.event USING btree (tenant_id, is_visible);


--
-- Name: idx_up_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_up_tenant ON public.upozorneni USING btree (tenant_id);


--
-- Name: idx_ups_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ups_tenant ON public.upozorneni_skupiny USING btree (tenant_id);


--
-- Name: is_visible; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX is_visible ON public.event USING btree (is_visible);


--
-- Name: object_name; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX object_name ON public.location_attachment USING btree (object_name);


--
-- Name: pghero_query_stats_database_captured_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX pghero_query_stats_database_captured_at_idx ON public.pghero_query_stats USING btree (database, captured_at);


--
-- Name: pghero_space_stats_database_captured_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX pghero_space_stats_database_captured_at_idx ON public.pghero_space_stats USING btree (database, captured_at);


--
-- Name: room_attachment_object_name_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX room_attachment_object_name_idx ON public.room_attachment USING btree (object_name);


--
-- Name: since; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX since ON public.event USING btree (since);


--
-- Name: tenant_administrator_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_administrator_person_id_idx ON public.tenant_administrator USING btree (person_id);


--
-- Name: tenant_administrator_range_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_administrator_range_idx ON public.tenant_administrator USING gist (active_range, tenant_id, person_id);


--
-- Name: tenant_attachment_object_name_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_attachment_object_name_idx ON public.tenant_attachment USING btree (object_name);


--
-- Name: tenant_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_id ON public.aktuality USING btree (tenant_id);


--
-- Name: tenant_membership_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_membership_active_idx ON public.tenant_membership USING btree (active);


--
-- Name: tenant_membership_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_membership_person_id_idx ON public.tenant_membership USING btree (person_id);


--
-- Name: tenant_membership_range_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_membership_range_idx ON public.tenant_membership USING gist (active_range, tenant_id, person_id);


--
-- Name: tenant_membership_status_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX tenant_membership_status_idx ON public.tenant_membership USING btree (status);


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
-- Name: user_proxy_person_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_proxy_person_id_idx ON public.user_proxy USING btree (person_id);


--
-- Name: user_proxy_range_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_proxy_range_idx ON public.user_proxy USING gist (active_range, person_id, user_id);


--
-- Name: user_proxy_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_proxy_user_id_idx ON public.user_proxy USING btree (user_id);


--
-- Name: users_email_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX users_email_key ON public.users USING btree (u_email) WHERE (u_id <> ALL (ARRAY[(916)::bigint, (914)::bigint, (915)::bigint, (587)::bigint, (765)::bigint, (696)::bigint, (786)::bigint, (259)::bigint, (306)::bigint, (540)::bigint, (443)::bigint, (1042)::bigint, (585)::bigint, (825)::bigint, (413)::bigint, (428)::bigint, (985)::bigint, (935)::bigint, (218)::bigint, (581)::bigint, (827)::bigint, (826)::bigint, (165)::bigint, (207)::bigint, (232)::bigint, (945)::bigint, (990)::bigint, (1039)::bigint, (1040)::bigint, (606)::bigint, (607)::bigint, (896)::bigint, (975)::bigint, (496)::bigint, (511)::bigint, (898)::bigint, (920)::bigint, (970)::bigint, (724)::bigint, (725)::bigint, (958)::bigint, (542)::bigint, (543)::bigint, (886)::bigint, (223)::bigint, (348)::bigint, (23)::bigint, (973)::bigint, (128)::bigint, (988)::bigint, (517)::bigint, (978)::bigint, (928)::bigint, (930)::bigint, (968)::bigint, (939)::bigint, (951)::bigint, (950)::bigint, (808)::bigint, (723)::bigint, (557)::bigint, (1013)::bigint, (1014)::bigint, (1015)::bigint, (820)::bigint, (841)::bigint, (599)::bigint, (681)::bigint, (31)::bigint, (40)::bigint, (120)::bigint, (360)::bigint, (417)::bigint, (419)::bigint, (545)::bigint, (39)::bigint, (643)::bigint, (670)::bigint, (782)::bigint, (790)::bigint, (668)::bigint, (894)::bigint, (922)::bigint, (925)::bigint, (803)::bigint, (812)::bigint, (153)::bigint, (602)::bigint, (198)::bigint, (239)::bigint, (397)::bigint, (686)::bigint, (846)::bigint, (537)::bigint, (893)::bigint, (974)::bigint, (993)::bigint, (755)::bigint, (805)::bigint, (337)::bigint, (155)::bigint, (629)::bigint, (630)::bigint, (554)::bigint, (994)::bigint, (661)::bigint, (891)::bigint, (434)::bigint, (436)::bigint, (640)::bigint, (829)::bigint, (683)::bigint, (505)::bigint, (1)::bigint, (648)::bigint, (649)::bigint, (677)::bigint, (4)::bigint, (162)::bigint, (17)::bigint, (565)::bigint, (700)::bigint, (701)::bigint, (952)::bigint, (999)::bigint, (1003)::bigint, (1006)::bigint, (346)::bigint, (576)::bigint, (986)::bigint, (582)::bigint, (315)::bigint, (753)::bigint, (76)::bigint, (93)::bigint, (316)::bigint, (359)::bigint, (370)::bigint, (508)::bigint, (506)::bigint, (509)::bigint, (510)::bigint, (754)::bigint, (811)::bigint, (430)::bigint, (654)::bigint, (598)::bigint, (612)::bigint, (698)::bigint, (923)::bigint, (943)::bigint, (971)::bigint, (679)::bigint, (798)::bigint, (799)::bigint]));


--
-- Name: users_login_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX users_login_key ON public.users USING btree (u_login) WHERE (u_id <> ALL (ARRAY[(1050)::bigint, (533)::bigint, (882)::bigint, (1075)::bigint, (489)::bigint, (82)::bigint, (1138)::bigint, (689)::bigint, (45)::bigint, (433)::bigint, (13)::bigint, (142)::bigint, (223)::bigint, (1046)::bigint, (498)::bigint, (1106)::bigint, (105)::bigint, (130)::bigint]));


--
-- Name: crm_activity _100_timestamps; Type: TRIGGER; Schema: app_private; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON app_private.crm_activity FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: crm_prospect _100_timestamps; Type: TRIGGER; Schema: app_private; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON app_private.crm_prospect FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


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

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: event_attendance _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_attendance FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: event_external_registration _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_external_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


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
-- Name: galerie_foto _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.galerie_foto FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: membership_application _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.membership_application FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: payment _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.payment FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: person _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: posting _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.posting FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


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
-- Name: upozorneni _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.upozorneni FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: user_proxy _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.user_proxy FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


--
-- Name: users _100_timestamps; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


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
-- Name: cohort_membership _200_refresh_auth_details; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_refresh_auth_details AFTER INSERT OR DELETE OR UPDATE ON public.cohort_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg_auth_details__refresh();


--
-- Name: couple _200_refresh_auth_details; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_refresh_auth_details AFTER INSERT OR DELETE OR UPDATE ON public.couple FOR EACH ROW EXECUTE FUNCTION app_private.tg_auth_details__refresh();


--
-- Name: tenant_administrator _200_refresh_auth_details; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_refresh_auth_details AFTER INSERT OR DELETE OR UPDATE ON public.tenant_administrator FOR EACH ROW EXECUTE FUNCTION app_private.tg_auth_details__refresh();


--
-- Name: tenant_membership _200_refresh_auth_details; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_refresh_auth_details AFTER INSERT OR DELETE OR UPDATE ON public.tenant_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg_auth_details__refresh();


--
-- Name: tenant_trainer _200_refresh_auth_details; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _200_refresh_auth_details AFTER INSERT OR DELETE OR UPDATE ON public.tenant_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg_auth_details__refresh();


--
-- Name: transaction _300_effective_date; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _300_effective_date BEFORE INSERT OR UPDATE ON public.transaction FOR EACH ROW EXECUTE FUNCTION app_private.tg_transaction__effective_date();


--
-- Name: users _300_trim_login; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _300_trim_login BEFORE INSERT OR UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg_users__trim_login();


--
-- Name: event_instance _500_create_attendance; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_create_attendance AFTER INSERT ON public.event_instance FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_instance__create_attendance();


--
-- Name: event_registration _500_create_attendance; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_create_attendance AFTER INSERT ON public.event_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_registration__create_attendance();


--
-- Name: event_instance _500_delete_on_cancellation; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_delete_on_cancellation AFTER UPDATE ON public.event_instance FOR EACH ROW WHEN ((old.is_cancelled IS DISTINCT FROM new.is_cancelled)) EXECUTE FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation();


--
-- Name: cohort_membership _500_on_status; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_on_status AFTER INSERT OR UPDATE ON public.cohort_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg_cohort_membership__on_status();


--
-- Name: tenant_membership _500_on_status; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_on_status AFTER UPDATE ON public.tenant_membership FOR EACH ROW WHEN ((old.status IS DISTINCT FROM new.status)) EXECUTE FUNCTION app_private.tg_tenant_membership__on_status();


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
-- Name: event_instance _500_update_parent_range; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _500_update_parent_range AFTER INSERT OR DELETE OR UPDATE ON public.event_instance FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation();


--
-- Name: account _900_fix_balance_accounts; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_fix_balance_accounts AFTER INSERT OR DELETE OR UPDATE OF opening_balance OR TRUNCATE ON public.account FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_account_balances__update();


--
-- Name: posting _900_fix_balance_entries; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER _900_fix_balance_entries AFTER INSERT OR DELETE OR UPDATE OF amount OR TRUNCATE ON public.posting FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_account_balances__update();


--
-- Name: aktuality on_update_author; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_author BEFORE UPDATE ON public.aktuality FOR EACH ROW EXECUTE FUNCTION public.on_update_author_aktuality();


--
-- Name: upozorneni on_update_author_upozorneni; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER on_update_author_upozorneni BEFORE INSERT OR UPDATE ON public.upozorneni FOR EACH ROW EXECUTE FUNCTION public.on_update_author_upozorneni();


--
-- Name: crm_activity crm_activity_prospect_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.crm_activity
    ADD CONSTRAINT crm_activity_prospect_fkey FOREIGN KEY (prospect) REFERENCES app_private.crm_prospect(id) ON DELETE CASCADE;


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
-- Name: skupiny skupiny_cohort_group_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.skupiny
    ADD CONSTRAINT skupiny_cohort_group_fkey FOREIGN KEY (cohort_group) REFERENCES public.cohort_group(id) ON DELETE SET NULL;


--
-- Name: skupiny skupiny_tenant_id_fkey; Type: FK CONSTRAINT; Schema: app_private; Owner: -
--

ALTER TABLE ONLY app_private.skupiny
    ADD CONSTRAINT skupiny_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


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
-- Name: aktuality aktuality_at_foto_main_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_at_foto_main_fkey FOREIGN KEY (at_foto_main) REFERENCES public.galerie_foto(gf_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: CONSTRAINT aktuality_at_foto_main_fkey ON aktuality; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT aktuality_at_foto_main_fkey ON public.aktuality IS '@fieldName galerieFotoByAtFotoMain';


--
-- Name: aktuality aktuality_at_kdo_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_at_kdo_fkey FOREIGN KEY (at_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: aktuality aktuality_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: attachment attachment_uploaded_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT attachment_uploaded_by_fkey FOREIGN KEY (uploaded_by) REFERENCES public.users(u_id) ON DELETE SET NULL;


--
-- Name: cohort cohort_cohort_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_cohort_group_id_fkey FOREIGN KEY (cohort_group_id) REFERENCES public.cohort_group(id) ON DELETE SET NULL;


--
-- Name: cohort_group cohort_group_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_group
    ADD CONSTRAINT cohort_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: cohort_membership cohort_membership_cohort_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.cohort(id);


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
-- Name: cohort_subscription cohort_subscription_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_subscription
    ADD CONSTRAINT cohort_subscription_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: cohort_subscription cohort_subscription_cohort_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_subscription
    ADD CONSTRAINT cohort_subscription_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.cohort(id);


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
-- Name: CONSTRAINT dokumenty_d_kdo_fkey ON dokumenty; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT dokumenty_d_kdo_fkey ON public.dokumenty IS '@fieldName userByDKdo';


--
-- Name: dokumenty dokumenty_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dokumenty
    ADD CONSTRAINT dokumenty_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


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
-- Name: event_external_registration event_external_registration_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_external_registration
    ADD CONSTRAINT event_external_registration_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(id);


--
-- Name: event_external_registration event_external_registration_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_external_registration
    ADD CONSTRAINT event_external_registration_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_external_registration event_external_registration_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_external_registration
    ADD CONSTRAINT event_external_registration_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_instance event_instance_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_instance event_instance_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.tenant_location(id);


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
-- Name: event event_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.tenant_location(id);


--
-- Name: event event_payment_recipient_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_payment_recipient_id_fkey FOREIGN KEY (payment_recipient_id) REFERENCES public.account(id);


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
-- Name: event_registration event_registration_person_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_registration event_registration_target_cohort_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_target_cohort_id_fkey FOREIGN KEY (target_cohort_id) REFERENCES public.event_target_cohort(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_registration event_registration_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: event_target_cohort event_target_cohort_cohort_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.cohort(id);


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
    ADD CONSTRAINT event_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


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
    ADD CONSTRAINT form_responses_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: galerie_dir galerie_dir_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.galerie_dir
    ADD CONSTRAINT galerie_dir_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


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
    ADD CONSTRAINT galerie_foto_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: location_attachment location_attachment_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location_attachment
    ADD CONSTRAINT location_attachment_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.location(id) ON DELETE CASCADE;


--
-- Name: location_attachment location_attachment_object_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location_attachment
    ADD CONSTRAINT location_attachment_object_name_fkey FOREIGN KEY (object_name) REFERENCES public.attachment(object_name) ON DELETE CASCADE;


--
-- Name: membership_application membership_application_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.membership_application
    ADD CONSTRAINT membership_application_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(u_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: membership_application membership_application_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.membership_application
    ADD CONSTRAINT membership_application_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: otp_token otp_token_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.otp_token
    ADD CONSTRAINT otp_token_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: otp_token otp_token_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.otp_token
    ADD CONSTRAINT otp_token_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(u_id) ON DELETE CASCADE;


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
-- Name: payment payment_event_instance_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_event_instance_id_fkey FOREIGN KEY (event_instance_id) REFERENCES public.event_instance(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: payment payment_event_registration_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_event_registration_id_fkey FOREIGN KEY (event_registration_id) REFERENCES public.event_registration(id) ON UPDATE CASCADE ON DELETE CASCADE;


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
-- Name: payment payment_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


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
    ADD CONSTRAINT platby_category_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: platby_category platby_category_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_category
    ADD CONSTRAINT platby_category_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: platby_group_skupina platby_group_skupina_pgs_id_group_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_pgs_id_group_fkey FOREIGN KEY (pgs_id_group) REFERENCES public.platby_group(pg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: platby_group_skupina platby_group_skupina_pgs_id_skupina_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_pgs_id_skupina_fkey FOREIGN KEY (pgs_id_skupina) REFERENCES public.cohort(id);


--
-- Name: platby_group_skupina platby_group_skupina_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group_skupina
    ADD CONSTRAINT platby_group_skupina_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: platby_group platby_group_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_group
    ADD CONSTRAINT platby_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


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
    ADD CONSTRAINT platby_item_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: platby_raw platby_raw_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.platby_raw
    ADD CONSTRAINT platby_raw_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: posting posting_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posting
    ADD CONSTRAINT posting_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id);


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
-- Name: posting posting_transaction_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posting
    ADD CONSTRAINT posting_transaction_id_fkey FOREIGN KEY (transaction_id) REFERENCES public.transaction(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: room_attachment room_attachment_object_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room_attachment
    ADD CONSTRAINT room_attachment_object_name_fkey FOREIGN KEY (object_name) REFERENCES public.attachment(object_name) ON DELETE CASCADE;


--
-- Name: room_attachment room_attachment_room_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room_attachment
    ADD CONSTRAINT room_attachment_room_id_fkey FOREIGN KEY (room_id) REFERENCES public.room(id) ON DELETE CASCADE;


--
-- Name: room room_location_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room
    ADD CONSTRAINT room_location_fkey FOREIGN KEY (location_id) REFERENCES public.location(id) ON DELETE CASCADE;


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
    ADD CONSTRAINT tenant_attachment_object_name_fkey FOREIGN KEY (object_name) REFERENCES public.attachment(object_name) ON DELETE CASCADE;


--
-- Name: tenant_attachment tenant_attachment_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tenant_attachment
    ADD CONSTRAINT tenant_attachment_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


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
-- Name: transaction transaction_accounting_period_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_accounting_period_id_fkey FOREIGN KEY (accounting_period_id) REFERENCES public.accounting_period(id);


--
-- Name: transaction transaction_payment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES public.payment(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: transaction transaction_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: upozorneni_skupiny upozorneni_skupiny_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT upozorneni_skupiny_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: upozorneni_skupiny upozorneni_skupiny_ups_id_rodic_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT upozorneni_skupiny_ups_id_rodic_fkey FOREIGN KEY (ups_id_rodic) REFERENCES public.upozorneni(up_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: upozorneni_skupiny upozorneni_skupiny_ups_id_skupina_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni_skupiny
    ADD CONSTRAINT upozorneni_skupiny_ups_id_skupina_fkey FOREIGN KEY (ups_id_skupina) REFERENCES public.cohort(id);


--
-- Name: CONSTRAINT upozorneni_skupiny_ups_id_skupina_fkey ON upozorneni_skupiny; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT upozorneni_skupiny_ups_id_skupina_fkey ON public.upozorneni_skupiny IS '@fieldName cohortByUpsIdSkupina';


--
-- Name: upozorneni upozorneni_tenant_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni
    ADD CONSTRAINT upozorneni_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: upozorneni upozorneni_up_kdo_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.upozorneni
    ADD CONSTRAINT upozorneni_up_kdo_fkey FOREIGN KEY (up_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: CONSTRAINT upozorneni_up_kdo_fkey ON upozorneni; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT upozorneni_up_kdo_fkey ON public.upozorneni IS '@fieldName userByUpKdo';


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
    ADD CONSTRAINT users_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;


--
-- Name: pary_navrh admin_all; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY admin_all ON app_private.pary_navrh TO administrator USING (true) WITH CHECK (true);


--
-- Name: skupiny admin_all; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY admin_all ON app_private.skupiny TO administrator USING (true) WITH CHECK (true);


--
-- Name: skupiny all_view; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY all_view ON app_private.skupiny FOR SELECT USING (true);


--
-- Name: pary_navrh manage_own; Type: POLICY; Schema: app_private; Owner: -
--

CREATE POLICY manage_own ON app_private.pary_navrh USING (((pn_navrhl = public.current_user_id()) OR (pn_partner = public.current_user_id()) OR (pn_partnerka = public.current_user_id()))) WITH CHECK (((pn_navrhl = public.current_user_id()) AND ((pn_partner = public.current_user_id()) OR (pn_partnerka = public.current_user_id()))));


--
-- Name: pary_navrh; Type: ROW SECURITY; Schema: app_private; Owner: -
--

ALTER TABLE app_private.pary_navrh ENABLE ROW LEVEL SECURITY;

--
-- Name: skupiny; Type: ROW SECURITY; Schema: app_private; Owner: -
--

ALTER TABLE app_private.skupiny ENABLE ROW LEVEL SECURITY;

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
-- Name: attachment admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.attachment TO administrator USING (true) WITH CHECK (true);


--
-- Name: cohort admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.cohort TO administrator USING (true) WITH CHECK (true);


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
-- Name: event_attendance admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_attendance TO administrator USING (true);


--
-- Name: event_external_registration admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.event_external_registration TO administrator USING (true);


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

CREATE POLICY admin_all ON public.form_responses TO administrator USING (true);


--
-- Name: galerie_dir admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.galerie_dir TO administrator USING (true);


--
-- Name: galerie_foto admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.galerie_foto TO administrator USING (true);


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
-- Name: platby_category admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.platby_category TO administrator USING (true);


--
-- Name: platby_category_group admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.platby_category_group TO administrator USING (true);


--
-- Name: platby_group admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.platby_group TO administrator USING (true);


--
-- Name: platby_group_skupina admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.platby_group_skupina TO administrator USING (true);


--
-- Name: platby_item admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.platby_item TO administrator USING (true);


--
-- Name: platby_raw admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.platby_raw TO administrator USING (true);


--
-- Name: room admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.room TO administrator USING (true) WITH CHECK (true);


--
-- Name: room_attachment admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.room_attachment TO administrator USING (true) WITH CHECK (true);


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

CREATE POLICY admin_all ON public.tenant_attachment TO administrator USING (true);


--
-- Name: tenant_location admin_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_all ON public.tenant_location TO administrator USING (true);


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

CREATE POLICY admin_all ON public.upozorneni TO administrator USING (true);


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
-- Name: event_external_registration admin_my; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_my ON public.event_external_registration TO member USING ((( SELECT public.event_is_registration_open(event.*) AS event_is_registration_open
   FROM public.event
  WHERE (event_external_registration.event_id = event.id)) AND (created_by = public.current_user_id())));


--
-- Name: person admin_myself; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_myself ON public.person FOR UPDATE USING ((id = ANY (public.current_person_ids())));


--
-- Name: event admin_same_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_same_tenant ON public.event TO administrator USING (true);


--
-- Name: event_instance admin_same_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_same_tenant ON public.event_instance TO administrator USING (true);


--
-- Name: event_attendance admin_trainer; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_trainer ON public.event_attendance FOR UPDATE TO trainer USING ((EXISTS ( SELECT 1
   FROM ((public.event_instance
     LEFT JOIN public.event_trainer ON ((event_instance.event_id = event_trainer.event_id)))
     LEFT JOIN public.event_instance_trainer ON ((event_instance.id = event_instance_trainer.instance_id)))
  WHERE ((event_attendance.instance_id = event_instance.id) AND ((event_instance_trainer.person_id = ANY (public.current_person_ids())) OR (event_trainer.person_id = ANY (public.current_person_ids())))))));


--
-- Name: event_attendance admin_trainer_insert; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY admin_trainer_insert ON public.event_attendance FOR INSERT TO trainer WITH CHECK ((EXISTS ( SELECT 1
   FROM ((public.event_instance
     LEFT JOIN public.event_trainer ON ((event_instance.event_id = event_trainer.event_id)))
     LEFT JOIN public.event_instance_trainer ON ((event_instance.id = event_instance_trainer.instance_id)))
  WHERE ((event_attendance.instance_id = event_instance.id) AND ((event_instance_trainer.person_id = ANY (public.current_person_ids())) OR (event_trainer.person_id = ANY (public.current_person_ids())))))));


--
-- Name: aktuality; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.aktuality ENABLE ROW LEVEL SECURITY;

--
-- Name: cohort all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.cohort FOR SELECT USING (true);


--
-- Name: users all_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY all_view ON public.users FOR SELECT TO member USING (true);


--
-- Name: attachment; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.attachment ENABLE ROW LEVEL SECURITY;

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
-- Name: event_attendance current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.event_attendance AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: event_instance current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.event_instance AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: event_instance_trainer current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.event_instance_trainer AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


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
-- Name: galerie_dir current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.galerie_dir AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: galerie_foto current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.galerie_foto AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


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
-- Name: platby_category current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.platby_category AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: platby_category_group current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.platby_category_group AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: platby_group current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.platby_group AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: platby_group_skupina current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.platby_group_skupina AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: platby_item current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.platby_item AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: platby_raw current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.platby_raw AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: posting current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.posting AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: tenant_attachment current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.tenant_attachment AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));


--
-- Name: tenant_location current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.tenant_location AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: transaction current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.transaction AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: upozorneni current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.upozorneni AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: upozorneni_skupiny current_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY current_tenant ON public.upozorneni_skupiny AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));


--
-- Name: event_registration delete_my; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY delete_my ON public.event_registration FOR DELETE USING ((( SELECT public.event_is_registration_open(event.*) AS event_is_registration_open
   FROM public.event
  WHERE (event_registration.event_id = event.id)) AND ((person_id = ANY (public.current_person_ids())) OR (couple_id = ANY (public.current_couple_ids())))));


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
-- Name: event_external_registration; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.event_external_registration ENABLE ROW LEVEL SECURITY;

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
-- Name: membership_application manage_admin; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_admin ON public.membership_application TO administrator USING (true);


--
-- Name: membership_application manage_my; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_my ON public.membership_application USING ((created_by = public.current_user_id()));


--
-- Name: users manage_own; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY manage_own ON public.users USING ((u_id = public.current_user_id())) WITH CHECK ((u_id = public.current_user_id()));


--
-- Name: account member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.account FOR SELECT TO member USING (true);


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
-- Name: event_instance_trainer member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.event_instance_trainer FOR SELECT TO member USING (true);


--
-- Name: event_target_cohort member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.event_target_cohort FOR SELECT TO member USING (true);


--
-- Name: event_trainer member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.event_trainer FOR SELECT TO member USING (true);


--
-- Name: payment member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.payment FOR SELECT TO member USING (true);


--
-- Name: payment_debtor member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.payment_debtor FOR SELECT TO member USING (true);


--
-- Name: payment_recipient member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.payment_recipient FOR SELECT TO member USING (true);


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
-- Name: platby_raw member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.platby_raw FOR SELECT TO member USING ((EXISTS ( SELECT
   FROM public.platby_item
  WHERE ((platby_item.pi_id_raw = platby_raw.pr_id) AND (platby_item.pi_id_user = public.current_user_id())))));


--
-- Name: posting member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.posting FOR SELECT TO member USING (true);


--
-- Name: transaction member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.transaction FOR SELECT TO member USING (true);


--
-- Name: upozorneni member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.upozorneni FOR SELECT TO member USING (true);


--
-- Name: upozorneni_skupiny member_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY member_view ON public.upozorneni_skupiny FOR SELECT TO member USING (true);


--
-- Name: membership_application; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.membership_application ENABLE ROW LEVEL SECURITY;

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

CREATE POLICY public_view ON public.aktuality FOR SELECT USING (true);


--
-- Name: attachment public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.attachment FOR SELECT TO anonymous USING (true);


--
-- Name: cohort_group public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.cohort_group FOR SELECT USING (true);


--
-- Name: event public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.event FOR SELECT TO anonymous USING (is_public);


--
-- Name: galerie_dir public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.galerie_dir FOR SELECT USING (true);


--
-- Name: galerie_foto public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.galerie_foto FOR SELECT USING (true);


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

CREATE POLICY public_view ON public.tenant_attachment FOR SELECT USING (true);


--
-- Name: tenant_location public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant_location FOR SELECT USING (true);


--
-- Name: tenant_trainer public_view; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY public_view ON public.tenant_trainer FOR SELECT USING (true);


--
-- Name: event_external_registration register_public; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY register_public ON public.event_external_registration FOR INSERT TO anonymous WITH CHECK (( SELECT event.is_public
   FROM public.event
  WHERE (event_external_registration.event_id = event.id)));


--
-- Name: room; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.room ENABLE ROW LEVEL SECURITY;

--
-- Name: room_attachment; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.room_attachment ENABLE ROW LEVEL SECURITY;

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
-- Name: event trainer_same_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_same_tenant ON public.event TO trainer USING (app_private.can_trainer_edit_event(id)) WITH CHECK (true);


--
-- Name: event_external_registration trainer_same_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_same_tenant ON public.event_external_registration TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);


--
-- Name: event_instance trainer_same_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_same_tenant ON public.event_instance TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);


--
-- Name: event_instance_trainer trainer_same_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_same_tenant ON public.event_instance_trainer TO trainer USING (app_private.can_trainer_edit_event(( SELECT i.event_id
   FROM public.event_instance i
  WHERE (i.id = event_instance_trainer.instance_id)))) WITH CHECK (true);


--
-- Name: event_registration trainer_same_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_same_tenant ON public.event_registration TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);


--
-- Name: event_target_cohort trainer_same_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_same_tenant ON public.event_target_cohort TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);


--
-- Name: event_trainer trainer_same_tenant; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY trainer_same_tenant ON public.event_trainer TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);


--
-- Name: transaction; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.transaction ENABLE ROW LEVEL SECURITY;

--
-- Name: event_registration update_my; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY update_my ON public.event_registration FOR UPDATE USING ((( SELECT public.event_is_registration_open(event.*) AS event_is_registration_open
   FROM public.event
  WHERE (event_registration.event_id = event.id)) AND ((person_id = ANY (public.current_person_ids())) OR (couple_id = ANY (public.current_couple_ids())))));


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
-- Name: cohort_membership view_all; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_all ON public.cohort_membership FOR SELECT USING ((public.current_tenant_id() = tenant_id));


--
-- Name: platby_item view_my; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_my ON public.platby_item FOR SELECT TO member USING ((pi_id_user = public.current_user_id()));


--
-- Name: user_proxy view_personal; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_personal ON public.user_proxy FOR SELECT USING ((user_id = public.current_user_id()));


--
-- Name: person view_tenant_or_trainer; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_tenant_or_trainer ON public.person FOR SELECT USING (( SELECT ((( SELECT public.current_tenant_id() AS current_tenant_id) = ANY (auth_details.allowed_tenants)) AND ((( SELECT public.current_tenant_id() AS current_tenant_id) IN ( SELECT public.my_tenant_ids() AS my_tenant_ids)) OR (( SELECT public.current_tenant_id() AS current_tenant_id) = ANY (auth_details.tenant_trainers)) OR (( SELECT public.current_tenant_id() AS current_tenant_id) = ANY (auth_details.tenant_administrators))))
   FROM public.auth_details
  WHERE (auth_details.person_id = person.id)));


--
-- Name: event_attendance view_visible_event; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_event ON public.event_attendance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event_instance
  WHERE (event_attendance.instance_id = event_instance.id))));


--
-- Name: event_external_registration view_visible_event; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_event ON public.event_external_registration FOR SELECT TO member USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_external_registration.event_id = event.id))));


--
-- Name: event_instance view_visible_event; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_event ON public.event_instance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_instance.event_id = event.id))));


--
-- Name: event_lesson_demand view_visible_event; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_event ON public.event_lesson_demand FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event_registration
  WHERE (event_lesson_demand.registration_id = event_registration.id))));


--
-- Name: event_registration view_visible_event; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_event ON public.event_registration FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_registration.event_id = event.id))));


--
-- Name: couple view_visible_person; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY view_visible_person ON public.couple FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.person
  WHERE ((couple.man_id = person.id) OR (couple.woman_id = person.id)))));


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
-- Name: TABLE event_lesson_demand; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_lesson_demand TO anonymous;


--
-- Name: TABLE event_instance; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_instance TO anonymous;


--
-- Name: TABLE event_registration; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_registration TO anonymous;


--
-- Name: TABLE payment; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.payment TO anonymous;


--
-- Name: TABLE transaction; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.transaction TO anonymous;


--
-- Name: FUNCTION can_trainer_edit_event(eid bigint); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.can_trainer_edit_event(eid bigint) TO anonymous;


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
-- Name: FUNCTION cron_update_memberships(); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.cron_update_memberships() TO administrator;


--
-- Name: TABLE cohort_membership; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.cohort_membership TO anonymous;


--
-- Name: FUNCTION tg_cohort_membership__on_status(); Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON FUNCTION app_private.tg_cohort_membership__on_status() TO trainer;


--
-- Name: TABLE account; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.account TO anonymous;


--
-- Name: TABLE posting; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.posting TO anonymous;


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
-- Name: FUNCTION change_password(new_pass text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.change_password(new_pass text) TO anonymous;


--
-- Name: FUNCTION immutable_concat_ws(text, VARIADIC text[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.immutable_concat_ws(text, VARIADIC text[]) TO anonymous;


--
-- Name: TABLE person; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.person TO anonymous;


--
-- Name: FUNCTION confirm_membership_application(application_id bigint); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.confirm_membership_application(application_id bigint) TO administrator;


--
-- Name: TABLE couple; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.couple TO anonymous;


--
-- Name: FUNCTION couple_event_instances(p public.couple); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.couple_event_instances(p public.couple) TO anonymous;


--
-- Name: FUNCTION create_credit_transaction_for_person(v_person_id bigint, v_description text, v_amount numeric, v_currency text, v_date timestamp with time zone); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.create_credit_transaction_for_person(v_person_id bigint, v_description text, v_amount numeric, v_currency text, v_date timestamp with time zone) TO anonymous;


--
-- Name: TABLE event; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event TO anonymous;


--
-- Name: TABLE event_target_cohort; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_target_cohort TO anonymous;


--
-- Name: TABLE event_trainer; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_trainer TO anonymous;


--
-- Name: FUNCTION create_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.create_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]) TO anonymous;


--
-- Name: FUNCTION create_event_instance_payment(i public.event_instance); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.create_event_instance_payment(i public.event_instance) TO anonymous;


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
-- Name: FUNCTION edit_registration(registration_id bigint, note text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.edit_registration(registration_id bigint, note text) TO anonymous;


--
-- Name: FUNCTION event_instance_approx_price(v_instance public.event_instance); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_instance_approx_price(v_instance public.event_instance) TO anonymous;


--
-- Name: FUNCTION event_instance_attendance_summary(e public.event_instance); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_instance_attendance_summary(e public.event_instance) TO anonymous;


--
-- Name: TABLE event_instance_trainer; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_instance_trainer TO anonymous;


--
-- Name: FUNCTION event_instance_trainer_name(t public.event_instance_trainer); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_instance_trainer_name(t public.event_instance_trainer) TO anonymous;


--
-- Name: FUNCTION event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean, trainer_ids bigint[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean, trainer_ids bigint[]) TO anonymous;


--
-- Name: FUNCTION event_is_registration_open(e public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_is_registration_open(e public.event) TO anonymous;


--
-- Name: FUNCTION event_my_registrations(e public.event); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_my_registrations(e public.event) TO anonymous;


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
-- Name: FUNCTION event_trainer_lessons_remaining(e public.event_trainer); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_trainer_lessons_remaining(e public.event_trainer) TO anonymous;


--
-- Name: FUNCTION event_trainer_name(t public.event_trainer); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.event_trainer_name(t public.event_trainer) TO anonymous;


--
-- Name: TABLE response_cache; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.response_cache TO anonymous;


--
-- Name: FUNCTION fetch_with_cache(input_url text, headers public.http_header[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.fetch_with_cache(input_url text, headers public.http_header[]) TO trainer;


--
-- Name: FUNCTION filtered_people(is_trainer boolean, is_admin boolean, in_cohorts bigint[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.filtered_people(is_trainer boolean, is_admin boolean, in_cohorts bigint[]) TO anonymous;


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
-- Name: FUNCTION http(request public.http_request); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.http(request public.http_request) TO trainer;


--
-- Name: FUNCTION http_header(field character varying, value character varying); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.http_header(field character varying, value character varying) TO anonymous;


--
-- Name: FUNCTION invitation_info(token uuid); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.invitation_info(token uuid) TO anonymous;


--
-- Name: FUNCTION log_in_as(id bigint, OUT usr public.users, OUT jwt public.jwt_token); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.log_in_as(id bigint, OUT usr public.users, OUT jwt public.jwt_token) TO administrator;


--
-- Name: FUNCTION login(login character varying, passwd character varying, OUT usr public.users, OUT jwt public.jwt_token); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.login(login character varying, passwd character varying, OUT usr public.users, OUT jwt public.jwt_token) TO anonymous;


--
-- Name: FUNCTION move_event_instance(id bigint, since timestamp with time zone, until timestamp with time zone, trainer_person_id bigint, location_id bigint, location_text text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.move_event_instance(id bigint, since timestamp with time zone, until timestamp with time zone, trainer_person_id bigint, location_id bigint, location_text text) TO anonymous;


--
-- Name: FUNCTION my_announcements(archive boolean); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.my_announcements(archive boolean) TO anonymous;


--
-- Name: FUNCTION my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, only_mine boolean) TO anonymous;


--
-- Name: FUNCTION my_tenant_ids(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.my_tenant_ids() TO anonymous;


--
-- Name: FUNCTION otp_login(token uuid, OUT usr public.users, OUT jwt public.jwt_token); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.otp_login(token uuid, OUT usr public.users, OUT jwt public.jwt_token) TO anonymous;


--
-- Name: TABLE payment_debtor; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.payment_debtor TO anonymous;


--
-- Name: FUNCTION payment_debtor_is_unpaid(p public.payment_debtor); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.payment_debtor_is_unpaid(p public.payment_debtor) TO anonymous;


--
-- Name: FUNCTION payment_debtor_price(p public.payment_debtor); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.payment_debtor_price(p public.payment_debtor) TO anonymous;


--
-- Name: FUNCTION person_account(p_id bigint, c text, OUT acc public.account); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_account(p_id bigint, c text, OUT acc public.account) TO anonymous;


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
-- Name: FUNCTION person_name(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_name(p public.person) TO anonymous;


--
-- Name: TABLE event_attendance; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.event_attendance TO anonymous;


--
-- Name: FUNCTION person_recent_attendance(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_recent_attendance(p public.person) TO anonymous;


--
-- Name: FUNCTION person_weekly_attendance(p public.person); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.person_weekly_attendance(p public.person) TO anonymous;


--
-- Name: FUNCTION refresh_jwt(); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.refresh_jwt() TO anonymous;


--
-- Name: FUNCTION register_to_event(INOUT registration public.event_registration, lessons public.event_lesson_demand[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.register_to_event(INOUT registration public.event_registration, lessons public.event_lesson_demand[]) TO anonymous;


--
-- Name: FUNCTION register_to_event_many(registrations public.register_to_event_type[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.register_to_event_many(registrations public.register_to_event_type[]) TO anonymous;


--
-- Name: FUNCTION register_using_invitation(email text, login text, passwd text, token uuid, OUT usr public.users, OUT jwt public.jwt_token); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.register_using_invitation(email text, login text, passwd text, token uuid, OUT usr public.users, OUT jwt public.jwt_token) TO anonymous;


--
-- Name: FUNCTION register_without_invitation(email text, login text, passwd text, OUT usr public.users, OUT jwt public.jwt_token); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.register_without_invitation(email text, login text, passwd text, OUT usr public.users, OUT jwt public.jwt_token) TO anonymous;


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
-- Name: FUNCTION tenant_account(c text, OUT acc public.account); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.tenant_account(c text, OUT acc public.account) TO anonymous;


--
-- Name: TABLE tenant_membership; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant_membership TO anonymous;


--
-- Name: FUNCTION tenant_couples(t public.tenant); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.tenant_couples(t public.tenant) TO anonymous;


--
-- Name: FUNCTION update_event_attendance(instance_id bigint, person_id bigint, status public.attendance_type, note text); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.update_event_attendance(instance_id bigint, person_id bigint, status public.attendance_type, note text) TO anonymous;


--
-- Name: FUNCTION upsert_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]); Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON FUNCTION public.upsert_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]) TO anonymous;


--
-- Name: TABLE pary_navrh; Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON TABLE app_private.pary_navrh TO anonymous;


--
-- Name: SEQUENCE pary_navrh_pn_id_seq; Type: ACL; Schema: app_private; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE app_private.pary_navrh_pn_id_seq TO anonymous;


--
-- Name: TABLE skupiny; Type: ACL; Schema: app_private; Owner: -
--

GRANT ALL ON TABLE app_private.skupiny TO anonymous;


--
-- Name: SEQUENCE skupiny_s_id_seq; Type: ACL; Schema: app_private; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE app_private.skupiny_s_id_seq TO anonymous;


--
-- Name: TABLE accounting_period; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.accounting_period TO anonymous;


--
-- Name: TABLE aktuality; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.aktuality TO anonymous;


--
-- Name: SEQUENCE aktuality_at_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.aktuality_at_id_seq TO anonymous;


--
-- Name: TABLE tenant_administrator; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant_administrator TO anonymous;


--
-- Name: TABLE tenant_trainer; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.tenant_trainer TO anonymous;


--
-- Name: TABLE auth_details_view; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.auth_details_view TO anonymous;


--
-- Name: TABLE auth_details; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.auth_details TO anonymous;


--
-- Name: TABLE cohort; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.cohort TO anonymous;


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
-- Name: TABLE event_external_registration; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,REFERENCES,DELETE,TRIGGER,TRUNCATE,MAINTAIN,UPDATE ON TABLE public.event_external_registration TO anonymous;


--
-- Name: COLUMN event_external_registration.event_id; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(event_id) ON TABLE public.event_external_registration TO anonymous;


--
-- Name: COLUMN event_external_registration.first_name; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(first_name) ON TABLE public.event_external_registration TO anonymous;


--
-- Name: COLUMN event_external_registration.last_name; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(last_name) ON TABLE public.event_external_registration TO anonymous;


--
-- Name: COLUMN event_external_registration.prefix_title; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(prefix_title) ON TABLE public.event_external_registration TO anonymous;


--
-- Name: COLUMN event_external_registration.suffix_title; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(suffix_title) ON TABLE public.event_external_registration TO anonymous;


--
-- Name: COLUMN event_external_registration.nationality; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(nationality) ON TABLE public.event_external_registration TO anonymous;


--
-- Name: COLUMN event_external_registration.birth_date; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(birth_date) ON TABLE public.event_external_registration TO anonymous;


--
-- Name: COLUMN event_external_registration.tax_identification_number; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(tax_identification_number) ON TABLE public.event_external_registration TO anonymous;


--
-- Name: COLUMN event_external_registration.email; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(email) ON TABLE public.event_external_registration TO anonymous;


--
-- Name: COLUMN event_external_registration.phone; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(phone) ON TABLE public.event_external_registration TO anonymous;


--
-- Name: COLUMN event_external_registration.note; Type: ACL; Schema: public; Owner: -
--

GRANT INSERT(note) ON TABLE public.event_external_registration TO anonymous;


--
-- Name: SEQUENCE event_id_seq; Type: ACL; Schema: public; Owner: -
--

GRANT SELECT,USAGE ON SEQUENCE public.event_id_seq TO anonymous;


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
-- Name: TABLE payment_recipient; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.payment_recipient TO anonymous;


--
-- Name: TABLE person_invitation; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.person_invitation TO anonymous;


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
-- Name: TABLE account_balances; Type: ACL; Schema: public; Owner: -
--

GRANT ALL ON TABLE public.account_balances TO anonymous;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: public; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT SELECT,USAGE ON SEQUENCES TO anonymous;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: public; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT ALL ON FUNCTIONS TO anonymous;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: -; Owner: -
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres REVOKE ALL ON FUNCTIONS FROM PUBLIC;


--
-- PostgreSQL database dump complete
--

