drop schema if exists federated cascade;

create schema federated;

CREATE EXTENSION IF NOT EXISTS unaccent;
CREATE OR REPLACE FUNCTION federated.unaccent(text) RETURNS text
AS $$
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
  category_id bigint NULL REFERENCES federated.category(id),
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
    federated.unaccent(coalesce(canonical_name, (first_name || ' ' || last_name)))
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
CREATE TABLE federated.competitor (
  id bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  competitor_type federated.competitor_type NOT NULL,
  name            text,         -- team name, couple label, or derived
  created_at      timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE federated.competitor_component (
  competitor_id   bigint NOT NULL REFERENCES federated.competitor(id),
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
  UNIQUE (federation, external_id)
);

CREATE TABLE federated.athlete_club_membership (
  athlete_id bigint NOT NULL REFERENCES federated.athlete(id),
  club_id    bigint NOT NULL REFERENCES federated.federation_club(id),
  valid_from date NOT NULL,
  valid_to   date,
  PRIMARY KEY (athlete_id, club_id, valid_from),
  CHECK (valid_to IS NULL OR valid_to >= valid_from),
  EXCLUDE USING gist (
    athlete_id WITH =,
    club_id    WITH =,
    daterange(valid_from, COALESCE(valid_to, 'infinity'::date), '[]') WITH &&
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
  EXCLUDE USING gist (
    competitor_id WITH =,
    club_id       WITH =,
    daterange(valid_from, COALESCE(valid_to, 'infinity'::date), '[]') WITH &&
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
    CASE
      WHEN end_date   IS NULL THEN daterange(start_date, start_date, '[]')
      ELSE daterange(start_date, end_date, '[]')
    END
  ) STORED,
  UNIQUE (federation, external_id),
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
  start_date    date,
  end_date      date,
  UNIQUE (federation, external_id),
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
  id             bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  competition_id bigint NOT NULL REFERENCES federated.competition(id),
  round_index    integer NOT NULL,           -- 1,2,3,... within competition
  round_type     text,                       -- '1st round','QF','SF','Final',...
  dance_program_id bigint NOT NULL REFERENCES federated.dance_program(id),
  scoring_method federated.scoring_method NOT NULL,
  created_at     timestamptz NOT NULL DEFAULT now(),
  UNIQUE (competition_id, round_index)
);
CREATE INDEX ON federated.competition_round (competition_id, round_index);
CREATE INDEX ON federated.competition_round (dance_program_id);


CREATE TABLE federated.round_dance (
  id         bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  round_id   bigint NOT NULL REFERENCES federated.competition_round(id),
  dance_code text NOT NULL REFERENCES federated.dance(code),
  UNIQUE (round_id, dance_code)
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


CREATE TABLE federated.round_judge (
  round_id bigint NOT NULL REFERENCES federated.competition_round(id),
  judge_id bigint NOT NULL REFERENCES federated.judge(id),
  is_shadow boolean NOT NULL DEFAULT false,
  PRIMARY KEY (round_id, judge_id)
);
CREATE INDEX ON federated.round_judge (judge_id);

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
  round_dance_id bigint NOT NULL REFERENCES federated.round_dance(id),
  judge_id       bigint NOT NULL REFERENCES federated.judge(id),
  competitor_id  bigint NOT NULL REFERENCES federated.competitor(id),
  component      federated.score_component NOT NULL,
  score          numeric(10,3) NOT NULL,   -- 0/1, 1..6, AJS values, numeric scores...
  raw_score      text,                     -- original symbol/string
  created_at     timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (round_dance_id, judge_id, competitor_id, component)
);

CREATE INDEX ON federated.judge_score (round_dance_id);
CREATE INDEX ON federated.judge_score (competitor_id);
CREATE INDEX ON federated.judge_score (judge_id);

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
  UNIQUE (snapshot_id, competitor_id),
  CHECK (ranking_to IS NULL OR ranking_to >= ranking)
);
CREATE INDEX ON federated.ranklist_entry (competitor_id);
CREATE INDEX ON federated.ranklist_entry (snapshot_id, ranking);
