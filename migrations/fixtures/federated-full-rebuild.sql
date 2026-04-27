update crawler.frontier set process_status = 'pending'
where fetch_status in ('ok', 'pending');

select graphile_worker.complete_jobs(array_agg(id)) from graphile_worker.jobs where task_identifier in ('frontier_schedule', 'frontier_process');

create extension if not exists btree_gist;
CREATE EXTENSION IF NOT EXISTS unaccent;

drop schema if exists federated cascade;

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

CREATE TYPE federated.competitor_type AS ENUM (
  'couple',
  'solo',
  'duo',
  'trio',
  'formation',
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
  id             text NOT NULL GENERATED ALWAYS AS (federation || ':' || external_id) STORED PRIMARY KEY,
  federation     text NOT NULL REFERENCES federated.federation(code),
  external_id    text NOT NULL,
  canonical_name text,
  first_name     text,
  last_name      text,
  search_name    text GENERATED ALWAYS AS (
    federated.normalize_name(coalesce(canonical_name, public.immutable_concat_ws(' ', first_name, last_name)))
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

CREATE TYPE federated.competitor_role AS ENUM (
  'lead',
  'follow',
  'member',
  'substitute'
);

-- immutable, federations create a new competitor entry if the composition changes
-- updates happen only when, e.g. federations merge duplicate athlete records
CREATE TABLE federated.competitor (
  id              text NOT NULL GENERATED ALWAYS AS (federation || ':' || external_id) STORED PRIMARY KEY,
  federation      text NOT NULL REFERENCES federated.federation(code),
  external_id     text NOT NULL,
  competitor_type federated.competitor_type NOT NULL,
  age_group       text,
  name            text,
  component_sig   text,
  created_at      timestamptz NOT NULL DEFAULT now(),
  UNIQUE (competitor_type, component_sig)
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
  domestic_finale int NOT NULL DEFAULT 0,
  foreign_finale  int NOT NULL DEFAULT 0,
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
CREATE INDEX ON federated.event USING gist (federation, location, country, range);

CREATE TABLE federated.competition (
  id            bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  federation    text   NOT NULL REFERENCES federated.federation(code),
  external_id   text   NOT NULL,
  event_id      bigint not null references federated.event(id),
  category_id   bigint not null references federated.category(id),
  start_date    date not null,
  end_date      date,
  participants_total integer,
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
  id               bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
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
  dance_code text   NOT NULL REFERENCES federated.dance(code),
  dance_order int,
  PRIMARY KEY (round_id, dance_code),
  UNIQUE (round_id, dance_order)
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
  round_id          bigint  NOT NULL REFERENCES federated.competition_round(id),
  competitor_id     text    NOT NULL REFERENCES federated.competitor(id),
  overall_ranking   integer,       -- place in round (if defined)
  overall_ranking_to integer,      -- ties
  qualified_next    boolean,       -- progressed to next round?
  overall_score     numeric(10,3), -- aggregate across dances, if provided/derived
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
CREATE TABLE federated.judge_score (
  federation      text   NOT NULL REFERENCES federated.federation(code),
  event_date      date   NOT NULL,
  event_id        bigint NOT NULL REFERENCES federated.event(id),
  competition_id  bigint NOT NULL REFERENCES federated.competition(id),
  category_id     bigint NOT NULL REFERENCES federated.category(id),
  round_id        bigint NOT NULL REFERENCES federated.competition_round(id),
  dance_code      text   NOT NULL REFERENCES federated.dance(code),
  judge_person_id text   NOT NULL REFERENCES federated.person(id),
  competitor_id   text   NOT NULL REFERENCES federated.competitor(id),
  component       federated.score_component NOT NULL,
  score           numeric(10,3) NOT NULL,   -- 0/1, 1..6, AJS values, numeric scores...
  raw_score       text,                     -- original symbol/string
  created_at      timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (round_id, dance_code, judge_person_id, competitor_id, component),
  foreign key (round_id, dance_code)
    references federated.round_dance (round_id, dance_code),
  FOREIGN KEY (round_id, competition_id)
    REFERENCES federated.competition_round (id, competition_id) ON UPDATE CASCADE,
  FOREIGN KEY (competition_id, event_id)
    REFERENCES federated.competition (id, event_id) ON UPDATE CASCADE,
  FOREIGN KEY (competition_id, category_id)
    REFERENCES federated.competition (id, category_id) ON UPDATE CASCADE,
  FOREIGN KEY (federation, competition_id)
    REFERENCES federated.competition (federation,id) ON UPDATE CASCADE
);

CREATE INDEX ON federated.judge_score (federation, judge_person_id, event_date);
CREATE INDEX ON federated.judge_score (federation, competitor_id, event_date);
CREATE INDEX ON federated.judge_score (federation, category_id, event_date);
CREATE INDEX ON federated.judge_score (round_id);

CREATE TABLE federated.competition_result (
  competition_id bigint NOT NULL REFERENCES federated.competition(id),
  competitor_id  text   NOT NULL REFERENCES federated.competitor(id),
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


CREATE TYPE federated.competitor_component_input AS (
  person_id text,
  role federated.competitor_role
);

CREATE TYPE federated.competitor_category_progress_input AS (
  category_id bigint,
  points numeric(10,3),
  domestic_finale integer,
  foreign_finale integer
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
        SELECT jsonb_agg(jsonb_build_array(person_id, role::text) ORDER BY person_id, role::text)::text
        FROM (
          SELECT DISTINCT ON (person_id) person_id, role
          FROM unnest(in_components) AS c (person_id, role)
          ORDER BY person_id, role
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
  in_competitor_type federated.competitor_type,
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
    competitor_type,
    name
  )
  VALUES (
    in_series,
    in_discipline,
    in_age_group,
    in_gender_group,
    in_class,
    in_competitor_type,
    COALESCE(
      in_name,
      concat_ws(' ', in_series, in_age_group, nullif(in_competitor_type, 'couple'), in_class, in_discipline)
    )
  )
  ON CONFLICT (series, discipline, age_group, gender_group, class, competitor_type)
    DO UPDATE SET name = EXCLUDED.name
RETURNING id;
$$;

CREATE OR REPLACE FUNCTION federated.upsert_competitor(
  in_federation     text,
  in_external_id    text,
  in_type           federated.competitor_type,
  in_label          text,
  in_components     federated.competitor_component_input[]
)
  RETURNS text
  LANGUAGE sql
  SET SEARCH_PATH = federated, pg_temp
AS $$
  with competitor as (
    INSERT INTO federated.competitor (federation, external_id, competitor_type, name, component_sig)
    VALUES (in_federation, in_external_id, in_type, in_label,
            federated.competitor_component_sig(in_components))
    ON CONFLICT (id)
      DO UPDATE SET name = EXCLUDED.name
    RETURNING id
  ), src AS MATERIALIZED (
    SELECT
      (c).person_id AS person_id,
      min((c).role)  AS role
    FROM unnest(in_components) AS c
    GROUP BY 1
  ), upserted AS (
    INSERT INTO federated.competitor_component (competitor_id, person_id, role)
      SELECT competitor.id, s.person_id, s.role
      FROM competitor, src s
    ON CONFLICT (competitor_id, person_id) DO UPDATE SET role = EXCLUDED.role
    WHERE federated.competitor_component.role IS DISTINCT FROM EXCLUDED.role
    RETURNING 1
  ), deleted AS (
    DELETE FROM federated.competitor_component t
      WHERE t.competitor_id = (select id from competitor)
        AND NOT EXISTS (SELECT 1 FROM src s WHERE s.person_id = t.person_id)
    RETURNING 1
  )
  SELECT competitor.id
  FROM competitor, upserted, deleted;
$$;

CREATE OR REPLACE FUNCTION federated.replace_competitor_category_progress(
  in_competitor_id text,
  in_entries       federated.competitor_category_progress_input[] DEFAULT '{}'::federated.competitor_category_progress_input[]
)
  RETURNS void
  LANGUAGE plpgsql
  SET SEARCH_PATH = federated, pg_temp
AS $$
BEGIN
  DELETE FROM federated.competitor_category_progress
  WHERE competitor_id = in_competitor_id;

  INSERT INTO federated.competitor_category_progress (
    competitor_id,
    category_id,
    points,
    domestic_finale,
    foreign_finale
  )
  SELECT
    in_competitor_id,
    (entry).category_id,
    COALESCE((entry).points, 0),
    COALESCE((entry).domestic_finale, 0),
    COALESCE((entry).foreign_finale, 0)
  FROM unnest(COALESCE(in_entries, '{}'::federated.competitor_category_progress_input[])) AS entry
  WHERE (entry).category_id IS NOT NULL;
END;
$$;
SELECT verify_function('federated.replace_competitor_category_progress');


CREATE OR REPLACE FUNCTION federated.upsert_ranklist_snapshot(
  in_federation    text,
  in_category_id   bigint,
  in_ranklist_name text,
  in_as_of_date    date,
  in_kind          text DEFAULT 'default',
  in_entries       jsonb DEFAULT '[]'::jsonb
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
    competitor_id text,
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
  select canonical_name from federated.person where federation = 'csts' and external_id = idt::text;
$$ language sql stable;

create or replace function public.wdsf_athlete(min int) returns text as $$
  select canonical_name from federated.person where federation = 'wdsf' and external_id = min::text;
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
from federated.person p
       join federated.competitor_component cp on cp.person_id = p.id
       join federated.competitor on competitor.id = cp.competitor_id
       join federated.competitor_category_progress ccp on competitor.id = ccp.competitor_id
       join federated.category on ccp.category_id = category.id
where p.federation = 'csts' and p.external_id = in_person.csts_id;
$$ language sql stable;

comment on function public.person_csts_progress is '@simpleCollections only';
grant all on function public.person_csts_progress to anonymous;

select graphile_worker.add_job('frontier_process', json_build_object('isFullRebuild', true));
