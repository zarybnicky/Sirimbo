--! Previous: sha1:eb5a6288eb4d8a39271d9f59c13d77a2e52f0d74
--! Hash: sha1:c04523ddc34020b2ae8b57f5f0df0860369d6af0

--! split: 1-current.sql
alter type jwt_token
  alter attribute my_person_ids type bigint[],
  alter attribute my_tenant_ids type bigint[],
  alter attribute my_cohort_ids type bigint[],
  alter attribute my_couple_ids type bigint[];

drop function if exists public.my_tenant_ids;

alter table event
  drop column if exists guest_price,
  drop column if exists member_price,
  drop column if exists title_image_legacy,
  drop column if exists description_member;

alter table event_instance
  drop column if exists location_id;

alter table payment
  drop column if exists tags;

do $$ begin
  if not exists (
    select 1 from pg_type t
    join pg_catalog.pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public' and typname = 'login_result'
  ) then
    create type login_result as (
      usr users,
      jwt jwt_token
    );
  end if;
end $$;

comment on type login_result is '@name result';

drop function if exists otp_login;

CREATE or replace FUNCTION otp_login(token uuid)
  RETURNS login_result
  LANGUAGE plpgsql STRICT SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
  AS $$
declare
  v_token otp_token;
  usr users;
  jwt jwt_token;
begin
  select * into v_token from otp_token where access_token = token and used_at is null and expires_at > now();
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
  update otp_token set used_at = now() where id = v_token.id;
  return (usr, jwt);
end;
$$;

GRANT ALL ON FUNCTION otp_login TO anonymous;
select verify_function('public.otp_login');

CREATE or replace FUNCTION register_to_event_many(registrations register_to_event_type[]) RETURNS SETOF event_registration
    LANGUAGE plpgsql
    SET search_path TO pg_catalog, public, pg_temp
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
    if registration.lessons is not null then
      foreach demand in array registration.lessons loop
        perform set_lesson_demand(created.id, demand.trainer_id, demand.lesson_count);
      end loop;
    end if;
  end loop;
  return query select * from event_registration where id = any (created_ids);
end;
$$ security definer;

select verify_function('public.register_to_event_many');
GRANT ALL ON FUNCTION public.register_to_event_many TO anonymous;

drop function if exists register_using_invitation;

CREATE or replace FUNCTION public.register_using_invitation(email text, passwd text, token uuid, login text default null)
  RETURNS login_result
  LANGUAGE plpgsql SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
  AS $$
declare
  invitation person_invitation;
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  select * into invitation from person_invitation where access_token=token;

  if invitation is null then
    raise exception 'INVITATION_NOT_FOUND' using errcode = '28000';
  end if;
  if invitation.used_at is not null then
    raise exception 'INVITATION_ALREADY_USED' using errcode = '28P01';
  end if;
  if email is null or email = '' then
    raise exception 'INVALID_EMAIL' using errcode = '28P01';
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
  return (usr, jwt);
end
$$;

select verify_function('register_using_invitation');
GRANT ALL ON FUNCTION public.register_using_invitation TO anonymous;

drop function if exists register_without_invitation;

CREATE or replace FUNCTION register_without_invitation(email text, passwd text) RETURNS login_result
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO pg_catalog, public, pg_temp
    AS $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  insert into users (u_email, u_pass) values (email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  return (usr, jwt);
end
$$;

GRANT ALL ON FUNCTION register_without_invitation TO anonymous;

CREATE or replace FUNCTION public.reset_password(email character varying) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO pg_catalog, public, pg_temp
    AS $$
declare
  v_tenant tenant;
  v_user users;
  v_token otp_token;
  v_payload jsonb := null;
begin
  for v_user in (select * from users where u_email = email) loop
    insert into otp_token (user_id)
    values (v_user.id) returning * into v_token;

    v_payload := coalesce(v_payload, jsonb_build_array()) || jsonb_build_object(
      'login', v_user.u_login,
      'email', v_user.u_email,
      'token', v_token.access_token,
      'people', (
        select jsonb_agg(person_name(person.*))
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

GRANT ALL ON FUNCTION public.reset_password(email character varying) TO anonymous;
select verify_function('reset_password');

CREATE or replace FUNCTION tenant_account(in_currency text, OUT acc account) RETURNS account
  language sql security definer
  SET search_path TO pg_catalog, public, pg_temp
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

grant all on function tenant_account to anonymous;

create or replace function app_private.create_jwt_token(u users) returns jwt_token
    language sql stable
as $$
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
    u.u_login as username,
    u.u_email as email,
    coalesce((select array_agg(p.person_id) from person_ids p), '{}'::bigint[]) as my_person_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_ids p), '{}'::bigint[]) as my_person_ids,
    coalesce((select array_agg(p.cohort_id) from cohort_ids p), '{}'::bigint[]) as my_person_ids,
    coalesce((select array_agg(p.id) from couple_ids p), '{}'::bigint[]) as my_person_ids,
    exists (select 1 from tenant_ids t where t.tenant_id = (select current_tenant_id())) as is_member,
    exists (select 1 from tenant_trainers t where t.tenant_id = (select current_tenant_id())) as is_trainer,
    exists (select 1 from tenant_admins a where a.tenant_id = (select current_tenant_id())) as is_admin,
    app_private.is_system_admin(u.id) as is_system_admin;
$$;


drop schema if exists federated cascade;

update crawler.frontier set process_status = 'pending'
where fetch_status in ('ok', 'pending');

create extension if not exists btree_gist;
CREATE EXTENSION IF NOT EXISTS unaccent;

create schema federated;

grant usage on schema federated to anonymous;
GRANT SELECT ON ALL TABLES IN SCHEMA federated TO anonymous;
ALTER DEFAULT PRIVILEGES IN SCHEMA federated GRANT SELECT ON TABLES TO anonymous;

CREATE OR REPLACE FUNCTION federated.normalize_name(text) RETURNS text AS $$
  SELECT lower(public.unaccent('public.unaccent', $1));
$$ LANGUAGE sql IMMUTABLE PARALLEL SAFE STRICT;

CREATE TABLE federated.federation (
  code text NOT NULL,
  name text NOT NULL,
  PRIMARY KEY (code)
);

INSERT INTO federated.federation (code, name)
VALUES ('wdsf', 'World DanceSport Federation'),
       ('csts', 'Český svaz tanečního sportu'),
       ('szts', 'Slovenský zväz tanečných športov');


CREATE TABLE federated.dance (
  code       text PRIMARY KEY,
  name       text NOT NULL,
  discipline text NOT NULL
);

INSERT INTO federated.dance (code, name, discipline) VALUES
  ('SW',  'Waltz',            'standard'),
  ('TG',  'Tango',            'standard'),
  ('VW',  'Viennese Waltz',   'standard'),
  ('SF',  'Slow Foxtrot',     'standard'),
  ('QS',  'Quickstep',        'standard'),
  ('SA',  'Samba',            'latin'),
  ('CH',  'Cha Cha',          'latin'),
  ('RU',  'Rumba',            'latin'),
  ('PD',  'Paso Doble',       'latin'),
  ('JI',  'Jive',             'latin');


CREATE TABLE federated.dance_program (
  id          bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code        text UNIQUE,                     -- 'STD5','LAT5','LAT3_ENTRY',...
  name        text NOT NULL,                  -- human label
  discipline  text,                           -- optional: 'standard','latin',...
  is_default  boolean NOT NULL DEFAULT false,
  created_at  timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE federated.dance_program_dance (
  program_id  bigint NOT NULL REFERENCES federated.dance_program(id) ON DELETE CASCADE,
  dance_code  text NOT NULL REFERENCES federated.dance(code),
  dance_order integer NOT NULL,                 -- 1..N within program
  PRIMARY KEY (program_id, dance_code),
  UNIQUE (program_id, dance_order)
);

CREATE INDEX ON federated.dance_program_dance (program_id);
CREATE INDEX ON federated.dance_program_dance (dance_code);

-- single cross-federation category
CREATE TABLE federated.category (
  id bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  name          text NOT NULL,
  series        text NOT NULL,       -- TPV, Amateur, Professional
  discipline    text NOT NULL,       -- standard, latin, 10-dance, etc.
  age_group     text NOT NULL,       -- junior, youth, adult, senior...
  gender_group  text NOT NULL DEFAULT 'mixed',       -- male, female, same-sex, mixed
  class         text NOT NULL,       -- A,B,C,Open,...
  base_dance_program_id bigint REFERENCES federated.dance_program(id),
  UNIQUE (series, discipline, age_group, gender_group, class)
);

CREATE TABLE federated.federation_category (
  federation  text NOT NULL REFERENCES federated.federation(code),
  external_id text,
  category_id bigint NOT NULL REFERENCES federated.category(id),
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (federation, category_id),
  UNIQUE (federation, external_id)
);
CREATE INDEX ON federated.federation_category (category_id);

CREATE TYPE federated.gender AS ENUM (
  'male',
  'female',
  'other',
  'unknown'
);

CREATE TABLE federated.person (
  id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  canonical_name text,
  first_name     text,
  last_name      text,
  search_name    text GENERATED ALWAYS AS (
    federated.normalize_name(coalesce(canonical_name, public.immutable_concat_ws(' ', first_name, last_name)))
  ) STORED,
  gender         federated.gender,
  dob            date,
  nationality    text,
  created_at     timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE federated.judge (
  id bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  person_id bigint NOT NULL REFERENCES federated.person(id),
  created_at  timestamptz NOT NULL DEFAULT now(),
  UNIQUE (person_id)
);

CREATE TABLE federated.federation_judge (
  federation   text NOT NULL REFERENCES federated.federation(code),
  external_id  text NOT NULL,
  judge_id   bigint NULL REFERENCES federated.judge(id),
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (federation, external_id)
);
CREATE INDEX ON federated.federation_judge (judge_id);
CREATE UNIQUE INDEX ON federated.federation_judge (federation, judge_id)
  WHERE judge_id IS NOT NULL;


CREATE TABLE federated.athlete (
  id bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  person_id bigint NOT NULL REFERENCES federated.person(id),
  created_at  timestamptz NOT NULL DEFAULT now(),
  UNIQUE (person_id)
);

CREATE TABLE federated.federation_athlete (
  federation   text NOT NULL REFERENCES federated.federation(code),
  external_id  text NOT NULL,
  athlete_id   bigint NULL REFERENCES federated.athlete(id),
  age_group    text,
  medical_checkup_expiration date,
  medical_checkup_type text,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (federation, external_id)
);
CREATE INDEX ON federated.federation_athlete (athlete_id);
CREATE UNIQUE INDEX ON federated.federation_athlete (federation, athlete_id)
  WHERE athlete_id IS NOT NULL;

CREATE TYPE federated.competitor_type AS ENUM (
  'couple',
  'solo',
  'duo',
  'formation',
  'team'
);

CREATE TYPE federated.competitor_role AS ENUM (
  'lead',
  'follow',
  'member',
  'substitute'
);


-- immutable, federations create a new competitor entry if the composition changes
-- updates happen only when, e.g. federations merge duplicate athlete records
CREATE TABLE federated.competitor (
  id bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  competitor_type federated.competitor_type NOT NULL,
  name            text,
  component_sig   text,
  created_at      timestamptz NOT NULL DEFAULT now(),
  UNIQUE (competitor_type, component_sig)
);

CREATE TABLE federated.competitor_component (
  competitor_id   bigint NOT NULL REFERENCES federated.competitor(id) ON DELETE CASCADE,
  athlete_id      bigint NOT NULL REFERENCES federated.athlete(id),
  role            federated.competitor_role NOT NULL,
  PRIMARY KEY (competitor_id, athlete_id)
);

CREATE INDEX ON federated.competitor_component (athlete_id);

CREATE TABLE federated.federation_competitor (
  federation      text NOT NULL REFERENCES federated.federation(code),
  external_id     text NOT NULL,
  competitor_id   bigint NULL REFERENCES federated.competitor(id),
  age_group       text,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (federation, external_id),
  UNIQUE (federation, competitor_id)
);

CREATE TABLE federated.competitor_category_progress (
  federation      text NOT NULL REFERENCES federated.federation(code),
  competitor_id   bigint NOT NULL REFERENCES federated.competitor(id),
  category_id     bigint NOT NULL REFERENCES federated.category(id),
  points          numeric(10, 3) NOT NULL DEFAULT 0,
  domestic_finale int NOT NULL DEFAULT 0,
  foreign_finale  int NOT NULL DEFAULT 0,
  PRIMARY KEY (federation, competitor_id, category_id)
);
CREATE INDEX ON federated.competitor_category_progress (competitor_id);

CREATE TABLE federated.federation_club (
  id          bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  federation  text NOT NULL REFERENCES federated.federation(code),
  external_id text NOT NULL,
  name        text NOT NULL,
  city        text,
  country     text,
  created_at  timestamptz NOT NULL DEFAULT now(),
  UNIQUE (federation, external_id),
  UNIQUE (federation, id)
);

CREATE TABLE federated.athlete_club_membership (
  athlete_id bigint NOT NULL REFERENCES federated.athlete(id),
  club_id    bigint NOT NULL REFERENCES federated.federation_club(id),
  valid_from date NOT NULL,
  valid_to   date,
  PRIMARY KEY (athlete_id, club_id, valid_from),
  CHECK (valid_to IS NULL OR valid_to >= valid_from),
  -- non-exclusive due to federation shenanigans
  EXCLUDE USING gist (
    athlete_id WITH =,
    club_id    WITH =,
    daterange(valid_from, COALESCE(valid_to, 'infinity'::date), '[)') WITH &&
  )
);

CREATE INDEX ON federated.athlete_club_membership (athlete_id, valid_from, valid_to);
CREATE INDEX ON federated.athlete_club_membership (club_id, valid_from, valid_to);

CREATE TABLE federated.competitor_club_affiliation (
  competitor_id bigint NOT NULL REFERENCES federated.competitor(id),
  club_id       bigint NOT NULL REFERENCES federated.federation_club(id),
  valid_from    date NOT NULL,
  valid_to      date,
  PRIMARY KEY (competitor_id, club_id, valid_from),
  CHECK (valid_to IS NULL OR valid_to >= valid_from),
  -- non-exclusive due to federation shenanigans
  EXCLUDE USING gist (
    competitor_id WITH =,
    club_id       WITH =,
    daterange(valid_from, COALESCE(valid_to, 'infinity'::date), '[)') WITH &&
  )
);
CREATE INDEX ON federated.competitor_club_affiliation (club_id, valid_from, valid_to);


CREATE TABLE federated.event (
  id bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  federation  text NOT NULL REFERENCES federated.federation(code),
  external_id text NOT NULL,
  name        text,
  start_date  date NOT NULL,
  end_date    date,
  location    text,
  country     text,
  organizing_club_id bigint NULL REFERENCES federated.federation_club(id),
  range       daterange GENERATED ALWAYS AS (
    daterange(start_date, (coalesce(end_date, start_date) + 1), '[)')
  ) STORED,
  UNIQUE (federation, external_id),
  UNIQUE (federation, id),
  FOREIGN KEY (federation, organizing_club_id)
    REFERENCES federated.federation_club (federation, id),
  CHECK (end_date IS NULL OR end_date >= start_date)
);
CREATE INDEX ON federated.event (federation, start_date);
CREATE INDEX ON federated.event USING gist (range);

CREATE TABLE federated.competition (
  id bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  federation    text NOT NULL REFERENCES federated.federation(code),
  external_id   text NOT NULL,
  event_id      bigint not null references federated.event(id),
  category_id   bigint not null references federated.category(id),
  start_date    date not null,
  end_date      date,
  UNIQUE (federation, external_id),
  UNIQUE (federation, id),
  UNIQUE (id, event_id),
  UNIQUE (id, category_id),
  FOREIGN KEY (federation, event_id)
    REFERENCES federated.event (federation, id),
  CHECK (end_date IS NULL OR end_date >= start_date)
);
CREATE INDEX ON federated.competition (event_id);
CREATE INDEX ON federated.competition (category_id);

CREATE TYPE federated.scoring_method AS ENUM (
  'skating_marks',
  'skating_places',
  'ajs-3.0'
);

CREATE TABLE federated.competition_round (
  id               bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  competition_id   bigint NOT NULL REFERENCES federated.competition(id),
  round_label      text,             -- original localized '1. kolo'/'Semifinále','Finále', 1/2/3/F, ...
  round_key        text NOT NULL,    -- 1/2/3/F for WDSF, 1/2/3/SF/F for CSTS
  round_index      integer,          -- 1,2,3,... within competition
  dance_program_id bigint NOT NULL REFERENCES federated.dance_program(id),
  scoring_method   federated.scoring_method NOT NULL,
  created_at       timestamptz NOT NULL DEFAULT now(),
  UNIQUE (competition_id, round_key),
  UNIQUE (id, competition_id)
);
CREATE INDEX ON federated.competition_round (competition_id, round_key);
CREATE INDEX ON federated.competition_round (dance_program_id);


CREATE TABLE federated.round_dance (
  round_id   bigint NOT NULL REFERENCES federated.competition_round(id),
  dance_code text NOT NULL REFERENCES federated.dance(code),
  PRIMARY KEY (round_id, dance_code)
);
CREATE INDEX ON federated.round_dance (round_id);

CREATE FUNCTION federated.tg_round_dance__dance_program() RETURNS TRIGGER LANGUAGE plpgsql AS $$
begin
  IF NOT EXISTS (
    SELECT 1
    FROM federated.competition_round cr
    JOIN federated.dance_program_dance dpd ON dpd.program_id = cr.dance_program_id
    WHERE cr.id = NEW.round_id AND dpd.dance_code = NEW.dance_code
  ) THEN
    RAISE EXCEPTION 'Round dance % not in round dance program %', NEW.dance_code, NEW.round_id;
  END IF;
  RETURN NEW;
END;
$$;
create trigger _100_round_dance__dance_program before insert on federated.round_dance
  for each row execute function federated.tg_round_dance__dance_program();

CREATE TABLE federated.competition_entry (
  competition_id  bigint NOT NULL REFERENCES federated.competition(id),
  competitor_id   bigint NOT NULL REFERENCES federated.competitor(id),
  cancelled       boolean NOT NULL DEFAULT false,
  created_at      timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (competition_id, competitor_id)
);
CREATE INDEX ON federated.competition_entry (competitor_id);

-- Result per competitor per round (aggregated per round)
CREATE TABLE federated.competition_round_result (
  round_id          bigint NOT NULL REFERENCES federated.competition_round(id),
  competitor_id     bigint NOT NULL REFERENCES federated.competitor(id),
  overall_ranking   integer NOT NULL,       -- place in round (if defined)
  overall_ranking_to integer,      -- ties
  qualified_next    boolean,       -- progressed to next round?
  overall_score     numeric(10,3), -- aggregate across dances, if provided/derived
  PRIMARY KEY (round_id, competitor_id),
  CHECK (overall_ranking_to IS NULL OR overall_ranking_to >= overall_ranking)
);
CREATE INDEX ON federated.competition_round_result (competitor_id);
CREATE INDEX ON federated.competition_round_result (round_id, overall_ranking);


CREATE TYPE federated.score_component AS ENUM (
  'mark',         -- simple yes/no
  'places',       -- 1..6 etc.
  'ajs_tq',
  'ajs_mm',
  'ajs_ps',
  'ajs_cp'
);

-- Judge scores per dance (round × dance × judge × competitor × component)
CREATE TABLE federated.judge_score (
  federation     text NOT NULL REFERENCES federated.federation(code),
  event_date     date NOT NULL,
  event_id       bigint NOT NULL REFERENCES federated.event(id),
  competition_id bigint NOT NULL REFERENCES federated.competition(id),
  category_id    bigint NOT NULL REFERENCES federated.category(id),
  round_id       bigint NOT NULL REFERENCES federated.competition_round(id),
  dance_code     text   NOT NULL REFERENCES federated.dance(code),
  judge_id       bigint NOT NULL REFERENCES federated.judge(id),
  competitor_id  bigint NOT NULL REFERENCES federated.competitor(id),
  component      federated.score_component NOT NULL,
  score          numeric(10,3) NOT NULL,   -- 0/1, 1..6, AJS values, numeric scores...
  raw_score      text,                     -- original symbol/string
  created_at     timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (round_id, dance_code, judge_id, competitor_id, component),
  foreign key (round_id, dance_code)
    references federated.round_dance (round_id, dance_code),
  FOREIGN KEY (round_id, competition_id)
    REFERENCES federated.competition_round (id, competition_id),
  FOREIGN KEY (competition_id, event_id)
    REFERENCES federated.competition (id, event_id),
  FOREIGN KEY (competition_id, category_id)
    REFERENCES federated.competition (id, category_id),
  FOREIGN KEY (federation, competition_id)
    REFERENCES federated.competition (federation,id)
);

CREATE INDEX ON federated.judge_score (federation, judge_id, event_date);
CREATE INDEX ON federated.judge_score (federation, competitor_id, event_date);
CREATE INDEX ON federated.judge_score (federation, category_id, event_date);

create view federated.round_judge as
select distinct round_id, judge_id
from federated.judge_score;

CREATE TABLE federated.competition_result (
  competition_id bigint NOT NULL REFERENCES federated.competition(id),
  competitor_id  bigint NOT NULL REFERENCES federated.competitor(id),
  start_number   text,
  ranking        integer NOT NULL,
  ranking_to     integer,
  point_gain     numeric(10,3),
  final_gain     numeric(10,3),
  PRIMARY KEY (competition_id, competitor_id),
  UNIQUE (competition_id, start_number),
  CHECK (ranking_to IS NULL OR ranking_to >= ranking)
);

CREATE INDEX ON federated.competition_result (competitor_id);
CREATE INDEX ON federated.competition_result (competition_id, ranking);


CREATE TABLE federated.ranklist (
  id bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  federation  text NOT NULL REFERENCES federated.federation(code),
  category_id bigint NOT NULL REFERENCES federated.category(id),
  name        text NOT NULL,
  UNIQUE (federation, category_id)
);

CREATE TABLE federated.ranklist_snapshot (
  id bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  ranklist_id bigint NOT NULL REFERENCES federated.ranklist(id),
  kind        text NOT NULL DEFAULT 'default',
  as_of_date  date NOT NULL,
  UNIQUE (ranklist_id, as_of_date, kind)
);

CREATE TABLE federated.ranklist_entry (
  snapshot_id   bigint NOT NULL REFERENCES federated.ranklist_snapshot(id),
  competitor_id bigint NOT NULL REFERENCES federated.competitor(id),
  ranking       integer NOT NULL,
  ranking_to    integer,
  points        numeric(10, 3),
  PRIMARY KEY (snapshot_id, competitor_id),
  CHECK (ranking_to IS NULL OR ranking_to >= ranking)
);
CREATE INDEX ON federated.ranklist_entry (competitor_id);
CREATE INDEX ON federated.ranklist_entry (snapshot_id, ranking);


CREATE TYPE federated.competitor_component_input AS (
  athlete_id bigint,
  role federated.competitor_role
);

CREATE OR REPLACE FUNCTION federated.competitor_component_sig(
  in_components federated.competitor_component_input[]
) RETURNS text
  LANGUAGE sql
  IMMUTABLE PARALLEL SAFE
AS $$
  SELECT
    CASE
      WHEN in_components IS NULL OR cardinality(in_components) = 0 THEN NULL
      ELSE (
        SELECT jsonb_agg(jsonb_build_array(athlete_id, role::text) ORDER BY athlete_id, role::text)::text
        FROM (
          SELECT DISTINCT ON (athlete_id) athlete_id, role
          FROM unnest(in_components) AS c (athlete_id, role)
          ORDER BY athlete_id, role
        ) t
      )
  END
$$;

CREATE OR REPLACE FUNCTION federated.upsert_category(
  in_series       text,
  in_discipline   text,
  in_age_group    text,
  in_gender_group text,
  in_class        text,
  in_name         text DEFAULT NULL
)
  RETURNS bigint
  LANGUAGE sql
  SET SEARCH_PATH = federated, pg_temp
AS $$
  INSERT INTO federated.category (
    series,
    discipline,
    age_group,
    gender_group,
    class,
    name
  )
  VALUES (
    in_series,
    in_discipline,
    in_age_group,
    in_gender_group,
    in_class,
    COALESCE(
      in_name,
      concat_ws(' ', in_series, in_discipline, in_age_group, in_class)
    )
  )
  ON CONFLICT (series, discipline, age_group, gender_group, class)
    DO UPDATE SET name = EXCLUDED.name
RETURNING id;
$$;

CREATE OR REPLACE FUNCTION federated.upsert_athlete(
  in_federation     text,
  in_external_id    text,
  in_canonical_name text,
  in_gender         federated.gender
)
  RETURNS bigint
  LANGUAGE plpgsql
  SET SEARCH_PATH = federated, pg_temp
AS $$
DECLARE
  v_person_id  bigint;
  v_athlete_id bigint;
BEGIN
  -- Ensure the mapping row exists and lock it (even if athlete_id is NULL).
  INSERT INTO federated.federation_athlete (federation, external_id, athlete_id)
  VALUES (in_federation, in_external_id, NULL)
  ON CONFLICT (federation, external_id) DO NOTHING;

  SELECT athlete_id
  INTO v_athlete_id
  FROM federated.federation_athlete
  WHERE federation=in_federation AND external_id=in_external_id
    FOR UPDATE;

  -- If no mapping, create person + athlete
  IF v_athlete_id IS NULL THEN
    INSERT INTO person (canonical_name, gender)
    VALUES (in_canonical_name, in_gender)
    RETURNING id INTO v_person_id;

    INSERT INTO athlete (person_id)
    VALUES (v_person_id)
    RETURNING id INTO v_athlete_id;

    UPDATE federated.federation_athlete
    SET athlete_id = v_athlete_id
    WHERE federation = in_federation
      AND external_id = in_external_id;
  END IF;

  RETURN v_athlete_id;
END;
$$;
SELECT verify_function('federated.upsert_athlete');

CREATE OR REPLACE FUNCTION federated.merge_competitor(
  in_old bigint,
  in_keep bigint
) RETURNS void
  LANGUAGE plpgsql
  SET search_path = federated, pg_temp
AS $$
BEGIN
  IF in_old IS NULL OR in_keep IS NULL OR in_old = in_keep THEN
    RETURN;
  END IF;

  -- Tables where UPDATE is not safe (competitor_id part of PK/unique)
  INSERT INTO federated.competition_entry (competition_id, competitor_id, cancelled, created_at)
  SELECT competition_id, in_keep, cancelled, created_at
  FROM federated.competition_entry WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.competition_round_result (round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score)
  SELECT round_id, in_keep, overall_ranking, overall_ranking_to, qualified_next, overall_score
  FROM federated.competition_round_result WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.competition_result (competition_id, competitor_id, start_number, ranking, ranking_to, point_gain, final_gain)
  SELECT competition_id, in_keep, start_number, ranking, ranking_to, point_gain, final_gain
  FROM federated.competition_result WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.judge_score (federation, event_date, event_id, competition_id, category_id, round_id, dance_code, judge_id, competitor_id, component, score, raw_score)
  SELECT federation, event_date, event_id, competition_id, category_id, round_id, dance_code, judge_id, in_keep, component, score, raw_score
  FROM federated.judge_score WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.ranklist_entry (snapshot_id, competitor_id, ranking, ranking_to, points)
  SELECT snapshot_id, in_keep, ranking, ranking_to, points
  FROM federated.ranklist_entry WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.competitor_category_progress (federation, competitor_id, category_id, points, domestic_finale, foreign_finale)
  SELECT federation, in_keep, category_id, points, domestic_finale, foreign_finale
  FROM federated.competitor_category_progress WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.competitor_club_affiliation (competitor_id, club_id, valid_from, valid_to)
  SELECT in_keep, club_id, valid_from, valid_to
  FROM federated.competitor_club_affiliation WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.federation_competitor (federation, external_id, competitor_id, age_group)
  SELECT federation, external_id, in_keep, age_group
  FROM federated.federation_competitor WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  DELETE FROM federated.competition_entry WHERE competitor_id = in_old;
  DELETE FROM federated.competition_round_result WHERE competitor_id = in_old;
  DELETE FROM federated.competition_result WHERE competitor_id = in_old;
  DELETE FROM federated.judge_score WHERE competitor_id = in_old;
  DELETE FROM federated.ranklist_entry WHERE competitor_id = in_old;
  DELETE FROM federated.competitor_category_progress WHERE competitor_id = in_old;
  DELETE FROM federated.competitor_club_affiliation WHERE competitor_id = in_old;
  DELETE FROM federated.federation_competitor WHERE competitor_id = in_old;

  DELETE FROM federated.competitor WHERE id = in_old;
END;
$$;


CREATE OR REPLACE FUNCTION federated.upsert_competitor(
  in_federation     text,
  in_external_id    text,
  in_type           federated.competitor_type,
  in_label          text,
  in_components     federated.competitor_component_input[]
)
  RETURNS bigint
  LANGUAGE plpgsql
  SET SEARCH_PATH = federated, pg_temp
AS $$
DECLARE
  v_competitor_id bigint;
  v_sig text;
  v_competitor_id_by_sig bigint;
BEGIN
  if in_federation is null or in_external_id is null or in_type is null then
    raise exception 'Missing argument to upsert_competitor';
  end if;

  -- Ensure the mapping row exists and lock it (even if we don't have a competitor yet).
  INSERT INTO federated.federation_competitor (federation, external_id, competitor_id)
  VALUES (in_federation, in_external_id, NULL)
  ON CONFLICT (federation, external_id) DO NOTHING;

  SELECT competitor_id
  INTO v_competitor_id
  FROM federated.federation_competitor
  WHERE federation = in_federation
    AND external_id = in_external_id
    FOR UPDATE;

  -- signature is null if in_components is null or empty
  v_sig = federated.competitor_component_sig(in_components);

  IF v_sig IS NOT NULL THEN
    SELECT c.id
    INTO v_competitor_id_by_sig
    FROM federated.competitor c
    WHERE c.competitor_type = in_type
      AND c.component_sig = v_sig;

    -- if mapping exists but points elsewhere, merge old -> canonical
    IF v_competitor_id_by_sig IS NOT NULL AND v_competitor_id IS NOT NULL AND v_competitor_id_by_sig <> v_competitor_id THEN
      PERFORM federated.merge_competitor(v_competitor_id, v_competitor_id_by_sig);
      v_competitor_id := v_competitor_id_by_sig;
    END IF;

    -- if mapping is missing, reuse link
    IF v_competitor_id IS NULL AND v_competitor_id_by_sig IS NOT NULL THEN
      v_competitor_id := v_competitor_id_by_sig;
    END IF;
  END IF;

  -- create if still missing (dedupe by unique (type,sig) when sig known)
  IF v_competitor_id IS NULL THEN
    IF v_sig IS NOT NULL THEN
      INSERT INTO federated.competitor (competitor_type, name, component_sig)
      VALUES (in_type, in_label, v_sig)
      ON CONFLICT (competitor_type, component_sig)
        DO UPDATE SET name = EXCLUDED.name
      RETURNING id INTO v_competitor_id;
    ELSE
      INSERT INTO federated.competitor (competitor_type, name)
      VALUES (in_type, in_label)
      RETURNING id INTO v_competitor_id;
    END IF;
  END IF;

  -- re-label if necessary
  if in_label is not null then
    UPDATE federated.competitor SET name = in_label WHERE id = v_competitor_id AND name IS DISTINCT FROM in_label;
  end if;

  -- if in_components provided and differ from existing ones, update them
  IF v_sig IS NOT NULL AND v_competitor_id_by_sig IS NULL THEN
    WITH src AS MATERIALIZED (
      SELECT
        (c).athlete_id AS athlete_id,
        min((c).role)  AS role
      FROM unnest(in_components) AS c
      GROUP BY 1
    ), upserted AS (
      INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)
        SELECT v_competitor_id, s.athlete_id, s.role
        FROM src s
        ON CONFLICT (competitor_id, athlete_id) DO UPDATE SET role = EXCLUDED.role
        WHERE federated.competitor_component.role IS DISTINCT FROM EXCLUDED.role
        RETURNING 1
    ), deleted AS (
      DELETE FROM federated.competitor_component t
        WHERE t.competitor_id = v_competitor_id
          AND NOT EXISTS (SELECT 1 FROM src s WHERE s.athlete_id = t.athlete_id)
             RETURNING 1
    )
    UPDATE federated.competitor c
    SET component_sig = v_sig
    WHERE c.id = v_competitor_id
      AND (SELECT count(*) FROM upserted) >= 0
      AND (SELECT count(*) FROM deleted)  >= 0;
  END IF;

  -- Update the already-locked mapping row (cheap).
  UPDATE federated.federation_competitor
  SET competitor_id = v_competitor_id
  WHERE federation = in_federation
    AND external_id = in_external_id
    AND competitor_id IS DISTINCT FROM v_competitor_id;

  RETURN v_competitor_id;
END;
$$;
SELECT verify_function('federated.upsert_competitor');

CREATE OR REPLACE FUNCTION federated.upsert_competitor_category_progress(
  in_federation      text,
  in_competitor_id   bigint,
  in_category_id     bigint,
  in_points          numeric(10,3),
  in_domestic_finale integer,
  in_foreign_finale  integer
)
  RETURNS void
  LANGUAGE sql
  SET SEARCH_PATH = federated, pg_temp
AS $$
  INSERT INTO federated.competitor_category_progress (
    federation,
    competitor_id,
    category_id,
    points,
    domestic_finale,
    foreign_finale
  )
  VALUES (
    in_federation,
    in_competitor_id,
    in_category_id,
    in_points,
    in_domestic_finale,
    in_foreign_finale
  )
  ON CONFLICT (federation, competitor_id, category_id)
    DO UPDATE
    SET points          = EXCLUDED.points,
        domestic_finale = EXCLUDED.domestic_finale,
        foreign_finale  = EXCLUDED.foreign_finale;
$$;


CREATE OR REPLACE FUNCTION federated.upsert_ranklist_snapshot(
  in_federation  text,
  in_category_id bigint,
  in_ranklist_name text,
  in_as_of_date  date,
  in_kind        text DEFAULT 'default',
  in_entries     jsonb DEFAULT '[]'::jsonb
)
  RETURNS bigint
  LANGUAGE plpgsql
  SET SEARCH_PATH = federated, pg_temp
AS $$
DECLARE
  v_ranklist_id  bigint;
  v_snapshot_id  bigint;
BEGIN
  INSERT INTO federated.ranklist (federation, category_id, name)
  VALUES (in_federation, in_category_id, in_ranklist_name)
  ON CONFLICT (federation, category_id)
    DO UPDATE SET name = EXCLUDED.name
  RETURNING id INTO v_ranklist_id;

  -- 2) upsert snapshot
  INSERT INTO federated.ranklist_snapshot (ranklist_id, as_of_date, kind)
  VALUES (v_ranklist_id, in_as_of_date, COALESCE(in_kind, 'default'))
  ON CONFLICT (ranklist_id, as_of_date, kind)
    DO UPDATE SET kind = EXCLUDED.kind
  RETURNING id INTO v_snapshot_id;

  -- 3) replace entries for this snapshot (so removals are reflected too)
  DELETE FROM federated.ranklist_entry
  WHERE snapshot_id = v_snapshot_id;

  INSERT INTO federated.ranklist_entry (
    snapshot_id,
    competitor_id,
    ranking,
    ranking_to,
    points
  )
  SELECT
    v_snapshot_id,
    e.competitor_id,
    e.ranking,
    e.ranking_to,
    e.points
  FROM jsonb_to_recordset(COALESCE(in_entries, '[]'::jsonb)) AS e(
    competitor_id bigint,
    ranking integer,
    ranking_to integer,
    points numeric(10,3)
  );

  RETURN v_snapshot_id;
END;
$$;
SELECT verify_function('federated.upsert_ranklist_snapshot');


------------------------------------
-- Dependent objects in public API
------------------------------------

create or replace function public.csts_athlete(idt int) returns text as $$
select canonical_name
from federated.person
       join federated.athlete on person.id = athlete.person_id
       join federated.federation_athlete on athlete.id = federation_athlete.athlete_id
where federation = 'csts' and external_id = idt::text;
$$ language sql stable;

create or replace function public.wdsf_athlete(min int) returns text as $$
select canonical_name
from federated.person
       join federated.athlete on person.id = athlete.person_id
       join federated.federation_athlete on athlete.id = federation_athlete.athlete_id
where federation = 'wdsf' and external_id = min::text;
$$ language sql stable;

grant all on function public.csts_athlete to anonymous;
grant all on function public.wdsf_athlete to anonymous;

drop function if exists public.person_csts_progress;
create or replace function public.person_csts_progress(in_person public.person) returns table (
  competitor_name text,
  category federated.category,
  points numeric(10, 3),
  finals integer
) as $$
select
  competitor.name as competitor_name,
  row(category.*) as category,
  ccp.points,
  ccp.domestic_finale + ccp.foreign_finale as finals
from federated.federation_athlete fa
       join federated.athlete on athlete.id = fa.athlete_id
       join federated.competitor_component cp on cp.athlete_id = athlete.id
       join federated.competitor on competitor.id = cp.competitor_id
       join federated.competitor_category_progress ccp on competitor.id = ccp.competitor_id and fa.federation = ccp.federation
       join federated.category on ccp.category_id = category.id
where fa.federation = 'csts' and fa.external_id = in_person.csts_id;
$$ language sql stable;

comment on function public.person_csts_progress is '@simpleCollections only';
grant all on function public.person_csts_progress to anonymous;
