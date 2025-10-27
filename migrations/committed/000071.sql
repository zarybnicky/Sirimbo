--! Previous: sha1:2411173030e9edc8b55a3f7eb87abd2787ac3add
--! Hash: sha1:c0184a3febe5cf8de247e05b490a981dc8b2cd1a

--! split: 1-current.sql


--! split: 2-csts-schema.sql
create schema if not exists csts;

create table if not exists csts.ingest (
  type text not null,
  url text not null,
  hash text not null,
  payload jsonb not null,
  created_at timestamptz not null default now(),
  checked_at timestamptz not null default now(),
  primary key (type, url, hash)
);

create table if not exists csts.athlete (
  idt integer primary key,
  name text not null,
  age_category text not null,
  sex text not null,
  medical_checkup_expiration date,
  fetched_at timestamptz not null default now()
);

create table if not exists csts.couple (
  id integer primary key,
  couple_idt integer not null,
  man_idt integer not null references csts.athlete(idt) on delete cascade,
  woman_idt integer not null references csts.athlete(idt) on delete cascade,
  formed_at timestamptz not null,
  unique (couple_idt)
);

create table if not exists csts.competitor_ranking (
  competitor_id integer not null,
  discipline text not null,
  ranking_points_age text not null,
  ranking_age text not null,
  competitor_age text not null,
  series text not null,
  competitors text not null,
  class text,
  points integer,
  domestic_finale_count integer,
  foreign_finale_count integer,
  ranklist_ranking integer,
  ranklist_points integer,
  athlete_idt integer references csts.athlete(idt) on delete cascade,
  couple_id integer references csts.couple(id) on delete cascade,
  primary key (competitor_id, discipline),
  unique (athlete_idt, discipline),
  unique (couple_id, discipline),
  check (
    (athlete_idt is not null and couple_id is null)
    or (athlete_idt is null and couple_id is not null)
  )
);

create table if not exists csts.athlete_ranking (
  athlete_id integer not null references csts.athlete(idt) on delete cascade,
  discipline text not null,
  series text not null,
  personal_class text,
  personal_points integer,
  personal_domestic_finale_count integer,
  personal_foreign_finale_count integer,
  personal_approved boolean,
  primary key (athlete_id, discipline, series)
);

create index if not exists couple_man_idx
  on csts.couple (man_idt);

create index if not exists couple_woman_idx
  on csts.couple (woman_idt);

create index if not exists competitor_ranking_athlete_idx
  on csts.competitor_ranking (athlete_idt);

create index if not exists competitor_ranking_couple_idx
  on csts.competitor_ranking (couple_id);
