update crawler.frontier set process_status = 'pending'
where fetch_status in ('ok', 'pending');

select graphile_worker.complete_jobs(array_agg(id)) from graphile_worker.jobs where task_identifier in ('frontier_schedule', 'frontier_process');

create extension if not exists btree_gist;
CREATE EXTENSION IF NOT EXISTS unaccent;
CREATE EXTENSION IF NOT EXISTS pg_trgm;

drop schema if exists federated cascade;

create schema federated;

grant usage on schema federated to anonymous;
GRANT SELECT ON ALL TABLES IN SCHEMA federated TO anonymous;
ALTER DEFAULT PRIVILEGES IN SCHEMA federated GRANT SELECT ON TABLES TO anonymous;

CREATE OR REPLACE FUNCTION app_private.normalize_name(text) RETURNS text AS $$
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
  ('JI',  'Jive',             'latin'),
  ('PK',  'Polka',            'latin'),
  ('BA',  'Bachata',          'caribbean'),
  ('ME',  'Merengue',         'caribbean'),
  ('SL',  'Salsa',            'caribbean'),
  ('KRB', 'Caribbean',        'caribbean'),
  ('OT',  'Other',            'other');


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

CREATE TYPE federated.competitor_type AS ENUM (
  'couple',
  'solo',
  'duo',
  'trio',
  'formation',
  'group',
  'team'
);

-- single cross-federation category
CREATE TABLE federated.category (
  id bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  name          text NOT NULL,
  series        text NOT NULL,       -- TPV, Amateur, Professional
  discipline    text NOT NULL,       -- standard, latin, 10-dance, etc.
  age_group     text NOT NULL,       -- junior, youth, adult, senior...
  gender_group  text NOT NULL DEFAULT 'mixed',       -- male, female, same-sex, mixed
  class         text NOT NULL,       -- A,B,C,Open,...
  competitor_type  federated.competitor_type NOT NULL DEFAULT 'couple',
  base_dance_program_id bigint REFERENCES federated.dance_program(id),
  UNIQUE (series, discipline, age_group, gender_group, class, competitor_type)
);

CREATE TYPE federated.gender AS ENUM (
  'male',
  'female',
  'other',
  'unknown'
);

CREATE TABLE federated.person (
  id             text NOT NULL GENERATED ALWAYS AS (federation || ':' || external_id::text) STORED PRIMARY KEY,
  federation     text NOT NULL REFERENCES federated.federation(code),
  external_id    bigint NOT NULL,
  canonical_name text,
  first_name     text,
  last_name      text,
  search_name    text GENERATED ALWAYS AS (
    app_private.normalize_name(coalesce(canonical_name, public.immutable_concat_ws(' ', first_name, last_name)))
  ) STORED,
  gender         federated.gender,
  dob            date,
  nationality    text,
  age_group      text,
  medical_checkup_expiration date,
  medical_checkup_type text,
  created_at     timestamptz NOT NULL DEFAULT now(),
  UNIQUE (federation, external_id)
);
CREATE INDEX IF NOT EXISTS idx_person_search_name_trgm
  ON federated.person USING gin (search_name gin_trgm_ops);

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

CREATE TABLE federated.person_license (
  person_id      text NOT NULL REFERENCES federated.person(id) ON DELETE CASCADE,
  federation     text NOT NULL REFERENCES federated.federation(code),
  kind           federated.person_license_kind NOT NULL,
  discipline     federated.person_license_discipline NOT NULL DEFAULT 'general',
  grade          text,
  valid_until    date,
  status         federated.person_license_status NOT NULL DEFAULT 'unknown',
  created_at     timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (person_id, kind, discipline)
);
CREATE INDEX ON federated.person_license (federation, person_id);

CREATE TYPE federated.competitor_role AS ENUM (
  'lead',
  'follow',
  'member',
  'substitute'
);

CREATE TYPE federated.official_role AS ENUM (
  'adjudicator',
  'chairperson',
  'invigilator',
  'scrutineer',
  'lead_scrutineer'
);

CREATE TYPE federated.competition_type AS ENUM (
  'cup',
  'ranking',
  'league',
  'championship',
  'top_level',
  'super_league',
  'g_cup',
  'unknown',
  'open'
);

-- immutable, federations create a new competitor entry if the composition changes
-- updates happen only when, e.g. federations merge duplicate athlete records
CREATE TABLE federated.competitor (
  id              text NOT NULL GENERATED ALWAYS AS (federation || ':' || external_id::text) STORED PRIMARY KEY,
  federation      text NOT NULL REFERENCES federated.federation(code),
  external_id     bigint NOT NULL,
  competitor_type federated.competitor_type NOT NULL,
  age_group       text,
  name            text,
  created_at      timestamptz NOT NULL DEFAULT now(),
  UNIQUE (federation, external_id)
);

CREATE TABLE federated.competitor_component (
  competitor_id   text NOT NULL REFERENCES federated.competitor(id) ON DELETE CASCADE,
  person_id       text NOT NULL REFERENCES federated.person(id),
  role            federated.competitor_role NOT NULL,
  PRIMARY KEY (competitor_id, person_id)
);

CREATE INDEX ON federated.competitor_component (person_id);

CREATE TABLE federated.competitor_category_progress (
  competitor_id   text   NOT NULL REFERENCES federated.competitor(id),
  category_id     bigint NOT NULL REFERENCES federated.category(id),
  points          numeric(10, 3) NOT NULL DEFAULT 0,
  domestic_finals int NOT NULL DEFAULT 0,
  foreign_finals  int NOT NULL DEFAULT 0,
  PRIMARY KEY (competitor_id, category_id)
);
CREATE INDEX ON federated.competitor_category_progress (competitor_id);

CREATE TABLE federated.federation_club (
  id          bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  federation  text   NOT NULL REFERENCES federated.federation(code),
  external_id text   NOT NULL,
  name        text   NOT NULL,
  city        text,
  country     text,
  created_at  timestamptz NOT NULL DEFAULT now(),
  UNIQUE (federation, external_id),
  UNIQUE (federation, id)
);

CREATE TABLE federated.person_club_membership (
  person_id  text   NOT NULL REFERENCES federated.person(id),
  club_id    bigint NOT NULL REFERENCES federated.federation_club(id),
  valid_from date   NOT NULL,
  valid_to   date,
  PRIMARY KEY (person_id, club_id, valid_from),
  CHECK (valid_to IS NULL OR valid_to >= valid_from),
  -- non-exclusive due to federation shenanigans
  EXCLUDE USING gist (
    person_id WITH =,
    club_id    WITH =,
    daterange(valid_from, COALESCE(valid_to, 'infinity'::date), '[)') WITH &&
  )
);

CREATE INDEX ON federated.person_club_membership (person_id, valid_from, valid_to);
CREATE INDEX ON federated.person_club_membership (club_id, valid_from, valid_to);

CREATE TABLE federated.competitor_club_affiliation (
  competitor_id text   NOT NULL REFERENCES federated.competitor(id),
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
  id          bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  federation  text   NOT NULL REFERENCES federated.federation(code),
  external_id text   NOT NULL,
  name        text,
  start_date  date NOT NULL,
  end_date    date,
  location    text,
  city        text,
  country     text,
  street_address text,
  postal_code text,
  address_note text,
  geo_reference text,
  floor_size text,
  contact_name text,
  contact_phone text,
  contact_email text,
  website_url text,
  organizing_club_id bigint NULL REFERENCES federated.federation_club(id),
  range       daterange GENERATED ALWAYS AS (
    daterange(start_date, (coalesce(end_date, start_date) + 1), '[)')
  ) STORED,
  venue_lat double precision,
  venue_lng double precision,
  venue_location_source text,
  venue_location_ref text,
  UNIQUE (federation, external_id),
  UNIQUE (federation, id),
  FOREIGN KEY (federation, organizing_club_id)
    REFERENCES federated.federation_club (federation, id),
  CHECK (end_date IS NULL OR end_date >= start_date)
);
CREATE INDEX ON federated.event (federation, start_date);
CREATE INDEX ON federated.event USING gist (range);
CREATE INDEX ON federated.event USING gist (federation, location, country, range);

CREATE TABLE federated.event_official (
  event_id   bigint NOT NULL REFERENCES federated.event(id) ON DELETE CASCADE,
  person_id  text   NOT NULL REFERENCES federated.person(id),
  role       federated.official_role NOT NULL,
  discipline text   NOT NULL DEFAULT '',
  grade      text,
  PRIMARY KEY (event_id, person_id, role, discipline)
);
CREATE INDEX ON federated.event_official (person_id);
CREATE INDEX ON federated.event_official (event_id, role);

CREATE TABLE federated.competition (
  id            bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  federation    text   NOT NULL REFERENCES federated.federation(code),
  external_id   text   NOT NULL,
  event_id      bigint not null references federated.event(id),
  category_id   bigint not null references federated.category(id),
  start_date    date not null,
  end_date      date,
  check_in_end  time,
  registration_fee numeric(10,3),
  participants_total integer,
  excused_total integer,
  completed_at timestamptz,
  competition_type federated.competition_type,
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

CREATE TABLE federated.competition_official (
  competition_id bigint NOT NULL REFERENCES federated.competition(id) ON DELETE CASCADE,
  external_id    text,
  person_id      text   NOT NULL REFERENCES federated.person(id),
  role           federated.official_role NOT NULL,
  PRIMARY KEY (competition_id, person_id, role)
);
CREATE INDEX ON federated.competition_official (person_id);
CREATE INDEX ON federated.competition_official (competition_id, role);
CREATE INDEX ON federated.competition_official (competition_id, external_id)
  WHERE external_id IS NOT NULL;

CREATE TYPE federated.scoring_method AS ENUM (
  'skating_marks',
  'skating_places',
  'ajs-3.0'
);

CREATE TABLE federated.competition_round (
  id               bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  competition_id   bigint NOT NULL REFERENCES federated.competition(id),
  round_label      text,             -- original localized '1. kolo'/'Semifinále','Finále', 1/2/3/F, ...
  round_key        text NOT NULL,    -- 1/2/3/F for WDSF, 1/2/3/SF/F for CSTS
  round_index      integer,          -- 1,2,3,... within competition
  dance_program_id bigint NOT NULL REFERENCES federated.dance_program(id),
  scoring_method   federated.scoring_method NOT NULL,
  created_at       timestamptz NOT NULL DEFAULT now(),
  UNIQUE (competition_id, round_key),
  UNIQUE (id, competition_id),
  UNIQUE (id, dance_program_id)
);
CREATE INDEX ON federated.competition_round (competition_id, round_key);
CREATE INDEX ON federated.competition_round (dance_program_id);


CREATE TABLE federated.round_dance (
  round_id         bigint NOT NULL,
  dance_program_id bigint NOT NULL,
  dance_code       text   NOT NULL,
  dance_order      int    NOT NULL,
  PRIMARY KEY (round_id, dance_order),
  UNIQUE (round_id, dance_order, dance_code),
  FOREIGN KEY (round_id, dance_program_id)
    REFERENCES federated.competition_round (id, dance_program_id) ON DELETE CASCADE,
  FOREIGN KEY (dance_program_id, dance_code)
    REFERENCES federated.dance_program_dance (program_id, dance_code)
);

CREATE TABLE federated.competition_entry (
  competition_id  bigint  NOT NULL REFERENCES federated.competition(id),
  competitor_id   text    NOT NULL REFERENCES federated.competitor(id),
  cancelled       boolean NOT NULL DEFAULT false,
  created_at      timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (competition_id, competitor_id)
);
CREATE INDEX ON federated.competition_entry (competitor_id);
CREATE INDEX ON federated.competition_entry (competition_id);

-- Result per competitor per round (aggregated per round)
CREATE TABLE federated.competition_round_result (
  round_id          bigint  NOT NULL REFERENCES federated.competition_round(id) ON DELETE CASCADE,
  competitor_id     text    NOT NULL REFERENCES federated.competitor(id),
  overall_ranking   integer,       -- place in round (if defined)
  overall_ranking_to integer,      -- ties
  qualified_next    boolean,       -- progressed to next round?
  overall_score     numeric(10,3), -- aggregate across dances, if provided/derived
  dance_results     real[],
  PRIMARY KEY (round_id, competitor_id),
  CHECK (overall_ranking IS NULL OR overall_ranking_to IS NULL OR overall_ranking_to >= overall_ranking)
);
CREATE INDEX ON federated.competition_round_result (competitor_id);
CREATE INDEX ON federated.competition_round_result (round_id, overall_ranking);

CREATE TABLE IF NOT EXISTS federated.competition_round_judge (
  round_id        bigint NOT NULL REFERENCES federated.competition_round(id) ON DELETE CASCADE,
  person_judge_id text   NOT NULL REFERENCES federated.person(id),
  judge_index     int NOT NULL,
  judge_label text,
  PRIMARY KEY (round_id, person_judge_id),
  UNIQUE (round_id, judge_index)
);

CREATE INDEX IF NOT EXISTS competition_round_judge_judge
  ON federated.competition_round_judge (person_judge_id);

CREATE TYPE federated.score_component AS ENUM (
  'mark',         -- simple yes/no
  'places',       -- 1..6 etc.
  'ajs_tq',
  'ajs_mm',
  'ajs_ps',
  'ajs_cp',
  'ajs_reduction'
);

-- Judge scores per dance (round × dance × judge × competitor × component)
-- This is a denormalized analytics fact table. Scalar parent FKs are omitted
-- where the composite FKs below already validate the denormalized values.
CREATE TABLE federated.judge_score (
  federation      text   NOT NULL,
  event_date      date   NOT NULL,
  event_id        bigint NOT NULL,
  competition_id  bigint NOT NULL,
  category_id     bigint NOT NULL,
  round_id        bigint NOT NULL,
  dance_order     int    NOT NULL,
  dance_code      text   NOT NULL,
  judge_person_id bigint NOT NULL,
  competitor_id   bigint NOT NULL,
  component       federated.score_component NOT NULL,
  score           numeric(10,3) NOT NULL,   -- 0/1, 1..6, AJS values, numeric scores...
  raw_score       text,                     -- original symbol/string
  created_at      timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (round_id, dance_order, judge_person_id, competitor_id, component),
  foreign key (round_id, dance_order, dance_code)
    references federated.round_dance (round_id, dance_order, dance_code) ON DELETE CASCADE,
  FOREIGN KEY (round_id, competition_id)
    REFERENCES federated.competition_round (id, competition_id) ON UPDATE CASCADE ON DELETE CASCADE,
  FOREIGN KEY (competition_id, event_id)
    REFERENCES federated.competition (id, event_id) ON UPDATE CASCADE,
  FOREIGN KEY (competition_id, category_id)
    REFERENCES federated.competition (id, category_id) ON UPDATE CASCADE,
  FOREIGN KEY (federation, competition_id)
    REFERENCES federated.competition (federation,id) ON UPDATE CASCADE,
  FOREIGN KEY (federation, judge_person_id)
    REFERENCES federated.person (federation, external_id),
  FOREIGN KEY (federation, competitor_id)
    REFERENCES federated.competitor (federation, external_id)
);

CREATE INDEX ON federated.judge_score (federation, judge_person_id, event_date);
CREATE INDEX ON federated.judge_score (federation, competitor_id, event_date);
CREATE INDEX ON federated.judge_score (federation, category_id, event_date);

CREATE TABLE federated.competition_result (
  competition_id bigint NOT NULL REFERENCES federated.competition(id),
  competitor_id  text   NOT NULL REFERENCES federated.competitor(id),
  start_number   text,
  ranking        integer NOT NULL,
  ranking_to     integer,
  point_gain     numeric(10,3),
  final_gain     numeric(10,3),
  is_final       boolean,
  completion_status text,
  last_round     text,
  last_dance     text,
  PRIMARY KEY (competition_id, competitor_id),
  CHECK (ranking_to IS NULL OR ranking_to >= ranking)
);

CREATE INDEX ON federated.competition_result (competitor_id);
CREATE INDEX ON federated.competition_result (competition_id, ranking);


CREATE TABLE federated.ranklist (
  id          bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  federation  text   NOT NULL REFERENCES federated.federation(code),
  category_id bigint NOT NULL REFERENCES federated.category(id),
  name        text NOT NULL,
  UNIQUE (federation, category_id)
);

CREATE TABLE federated.ranklist_snapshot (
  id          bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  ranklist_id bigint NOT NULL REFERENCES federated.ranklist(id),
  kind        text NOT NULL DEFAULT 'default',
  as_of_date  date NOT NULL,
  UNIQUE (ranklist_id, as_of_date, kind)
);

CREATE TABLE federated.ranklist_entry (
  snapshot_id   bigint NOT NULL REFERENCES federated.ranklist_snapshot(id),
  competitor_id text   NOT NULL REFERENCES federated.competitor(id),
  ranking       integer NOT NULL,
  ranking_to    integer,
  points        numeric(10, 3),
  PRIMARY KEY (snapshot_id, competitor_id),
  CHECK (ranking_to IS NULL OR ranking_to >= ranking)
);
CREATE INDEX ON federated.ranklist_entry (competitor_id);
CREATE INDEX ON federated.ranklist_entry (snapshot_id, ranking);

create or replace function public.csts_athlete(idt int) returns text as $$
  select canonical_name from federated.person where federation = 'csts' and external_id = idt;
$$ language sql stable;

create or replace function public.wdsf_athlete(min int) returns text as $$
  select canonical_name from federated.person where federation = 'wdsf' and external_id = min;
$$ language sql stable;

grant all on function public.csts_athlete to anonymous;
grant all on function public.wdsf_athlete to anonymous;

alter table public.person
  add column if not exists search_name text generated always as (
    app_private.normalize_name(public.immutable_concat_ws(' ', first_name, last_name))
  ) stored;

comment on column public.person.search_name is '@omit';

create index if not exists person_csts_id_idx
  on public.person (csts_id)
  where csts_id is not null;

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
  ccp.domestic_finals + ccp.foreign_finals as finals
from federated.person p
       join federated.competitor_component cp on cp.person_id = p.id
       join federated.competitor on competitor.id = cp.competitor_id
       join federated.competitor_category_progress ccp on competitor.id = ccp.competitor_id
       join federated.category on ccp.category_id = category.id
where p.federation = 'csts'
  and p.external_id = in_person.csts_id::bigint;
$$ language sql stable;

comment on function public.person_csts_progress is '@simpleCollections only';
grant all on function public.person_csts_progress to anonymous;

drop function if exists public.person_csts_candidates(public.person, integer);
drop function if exists public.person_csts_candidates(public.person, integer, real);

create or replace function public.person_csts_candidates(in_person public.person, "limit" integer default 10, threshold real default 0.4) returns table (
  id integer,
  name text,
  age_group text,
  similarity real
) as $$
  with params as (
    select
      greatest(0.3::real, least(coalesce(threshold, 0.4), 1::real)) as effective_threshold,
      greatest(0, least(coalesce($2, 10), 50)) as effective_limit
  ),
  scored_candidates as (
    select
      fp.external_id::integer as id,
      fp.canonical_name as name,
      fp.age_group,
      score.name_score,
      case
        when in_person.birth_date is not null and fp.dob is not null and fp.dob = in_person.birth_date then 1
        else 0
      end as dob_score
    from params
    join federated.person fp on true
    cross join lateral (
      select public.similarity(fp.search_name, in_person.search_name) as name_score
    ) score
    where in_person.search_name is not null
      and fp.federation = 'csts'
      and fp.external_id between 0 and 2147483647
      and fp.search_name % in_person.search_name
      and score.name_score >= params.effective_threshold
      and not exists (
        select 1
        from public.person existing
        where existing.id <> in_person.id
          and existing.csts_id = fp.external_id::integer
      )
  )
  select scored_candidates.id, scored_candidates.name, scored_candidates.age_group, scored_candidates.name_score
  from scored_candidates
  order by
    scored_candidates.dob_score desc,
    scored_candidates.name_score desc,
    scored_candidates.name,
    scored_candidates.id
  limit (select effective_limit from params);
$$ language sql stable;

comment on function public.person_csts_candidates is '@simpleCollections only';
grant all on function public.person_csts_candidates to anonymous;

drop function if exists public.competition_brief(text[], date, date);
drop function if exists public.competition_report(text[], date, date);
drop function if exists public.competition_brief(date, date, bigint, bigint[]);
drop function if exists public.competition_report(date, date, bigint, bigint[]);
drop type if exists public.competition_participation_record;

create type public.competition_participation_record as (
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
  check_in_end time,
  category federated.category,
  dances text[],
  participants integer,
  ranking integer,
  ranking_to integer,
  point_gain numeric(10,3),
  is_final boolean,
  has_result boolean,
  competition_type federated.competition_type
);

create or replace function public.competition_brief(
  p_since date default null,
  p_until date default null,
  p_cohort_id bigint default null,
  p_person_ids bigint[] default null
) returns setof public.competition_participation_record
language sql stable as $$
  with params as (
    select
      coalesce(p_since, date_trunc('week', now())::date + 5) as since,
      coalesce(p_until, date_trunc('week', now())::date + 7) as until
  ),
  scoped_people as (
    select distinct p.id, p.name, p.csts_id, p.wdsf_id
    from public.current_tenant_membership tm
    join public.person p on p.id = tm.person_id
    where (p_person_ids is null or p.id = any(p_person_ids))
      and (
        p_cohort_id is null
        or exists (
          select 1
          from public.current_cohort_membership cm
          where cm.person_id = p.id
            and cm.cohort_id = p_cohort_id
        )
      )
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    cross join lateral (
      values
        ('csts'::text, sp.csts_id::bigint),
        ('wdsf'::text, sp.wdsf_id::bigint)
    ) ids(federation, external_id)
    join federated.person fp
      on fp.federation = ids.federation
     and fp.external_id = ids.external_id
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
    comp.competition_type
  from params
  join federated.competition comp
    on comp.start_date >= params.since
   and comp.start_date < params.until
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  join federated.competition_entry ce
    on ce.competition_id = comp.id
   and not ce.cancelled
  join federated.competitor c on c.id = ce.competitor_id
  join federated.competitor_component cc on cc.competitor_id = c.id
  join federated_people fp on fp.federated_person_id = cc.person_id
  left join lateral (
    select coalesce(array_agg(d.name order by dpd.dance_order), '{}'::text[]) as dances
    from federated.dance_program_dance dpd
    join federated.dance d on d.code = dpd.dance_code
    where dpd.program_id = cat.base_dance_program_id
  ) dances on true
  order by
    comp.start_date,
    comp.check_in_end nulls last,
    fp.person_name,
    cat.discipline,
    cat.class,
    c.name;
$$;

comment on function public.competition_brief(date, date, bigint, bigint[]) is '@simpleCollections only';
grant all on function public.competition_brief(date, date, bigint, bigint[]) to anonymous;

create or replace function public.competition_report(
  p_since date default null,
  p_until date default null,
  p_cohort_id bigint default null,
  p_person_ids bigint[] default null
) returns setof public.competition_participation_record
language sql stable as $$
  with params as (
    select
      coalesce(p_since, date_trunc('week', now())::date - 2) as since,
      coalesce(p_until, date_trunc('week', now())::date) as until
  ),
  scoped_people as (
    select distinct p.id, p.name, p.csts_id, p.wdsf_id
    from public.current_tenant_membership tm
    join public.person p on p.id = tm.person_id
    where (p_person_ids is null or p.id = any(p_person_ids))
      and (
        p_cohort_id is null
        or exists (
          select 1
          from public.current_cohort_membership cm
          where cm.person_id = p.id
            and cm.cohort_id = p_cohort_id
        )
      )
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    cross join lateral (
      values
        ('csts'::text, sp.csts_id::bigint),
        ('wdsf'::text, sp.wdsf_id::bigint)
    ) ids(federation, external_id)
    join federated.person fp
      on fp.federation = ids.federation
     and fp.external_id = ids.external_id
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
    cr.ranking,
    cr.ranking_to,
    cr.point_gain,
    cr.is_final,
    true as has_result,
    comp.competition_type
  from params
  join federated.competition comp
    on comp.start_date >= params.since
   and comp.start_date < params.until
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  join federated.competition_result cr on cr.competition_id = comp.id
  join federated.competitor c on c.id = cr.competitor_id
  join federated.competitor_component cc on cc.competitor_id = c.id
  join federated_people fp on fp.federated_person_id = cc.person_id
  left join lateral (
    select coalesce(array_agg(d.name order by dpd.dance_order), '{}'::text[]) as dances
    from federated.dance_program_dance dpd
    join federated.dance d on d.code = dpd.dance_code
    where dpd.program_id = cat.base_dance_program_id
  ) dances on true
  order by
    comp.start_date,
    fp.person_name,
    cat.discipline,
    cat.class,
    cr.ranking,
    c.name;
$$;

comment on function public.competition_report(date, date, bigint, bigint[]) is '@simpleCollections only';
grant all on function public.competition_report(date, date, bigint, bigint[]) to anonymous;

--! include functions/activity_timeline.sql

select graphile_worker.add_job('frontier_process', json_build_object('isFullRebuild', true));
