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
  barcode_url text not null,
  fetched_at timestamptz not null default now()
);

create table if not exists csts.couple (
  id integer primary key,
  couple_idt integer not null,
  man_idt integer not null references csts.athlete(idt) on delete cascade,
  woman_idt integer not null references csts.athlete(idt) on delete cascade,
  medical_checkup_expiration date,
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
  class text not null,
  points integer not null,
  domestic_finale_count integer not null,
  foreign_finale_count integer not null,
  ranklist_ranking integer not null,
  ranklist_points integer not null,
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
  personal_class text not null,
  personal_points integer not null,
  personal_domestic_finale_count integer not null,
  personal_foreign_finale_count integer not null,
  personal_approved boolean not null,
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
