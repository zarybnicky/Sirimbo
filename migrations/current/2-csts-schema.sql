create schema if not exists csts;
comment on schema csts is 'ČSTS integration tables for data mirrored from the public API.';

create table if not exists csts.athlete (
  idt integer primary key,
  name text not null,
  valid_for date not null,
  age_category text not null,
  sex text not null,
  medical_checkup_expiration date,
  barcode_url text not null,
  fetched_at timestamptz not null default now()
);
comment on table csts.athlete is 'People registered with ČSTS as exported by the public athlete endpoint.';
comment on column csts.athlete.idt is 'Eight-digit ČSTS athlete identifier (EAN-8 with computed checksum).';
comment on column csts.athlete.valid_for is 'Date until which the athlete''s ČSTS registration is valid.';
comment on column csts.athlete.age_category is 'ČSTS age category the athlete currently belongs to.';
comment on column csts.athlete.sex is 'ČSTS reported gender marker.';
comment on column csts.athlete.medical_checkup_expiration is 'Validity date for the athlete''s medical checkup, if provided.';
comment on column csts.athlete.barcode_url is 'Link to the ČSTS generated barcode for the athlete''s membership card.';
comment on column csts.athlete.fetched_at is 'Timestamp when the record was last refreshed from ČSTS.';

do $$
begin
  if to_regclass('csts.athlete_ranking') is not null then
    drop table if exists csts.athlete_primary_category;
    drop table if exists csts.athlete_ranking cascade;
  end if;
end;
$$;

create table if not exists csts.couple (
  id integer primary key,
  couple_idt integer not null,
  first_athlete_idt integer not null,
  second_athlete_idt integer not null,
  medical_checkup_expiration date,
  formed_at timestamptz not null,
  check (first_athlete_idt < second_athlete_idt)
);
comment on table csts.couple is 'Competitive pairings registered with ČSTS.';
comment on column csts.couple.id is 'ČSTS competitor identifier for the couple.';
comment on column csts.couple.couple_idt is 'ČSTS-issued EAN-8 identifier for the couple registration.';
comment on column csts.couple.first_athlete_idt is 'Lower ČSTS athlete identifier participating in the couple.';
comment on column csts.couple.second_athlete_idt is 'Higher ČSTS athlete identifier participating in the couple.';
comment on column csts.couple.medical_checkup_expiration is 'Medical clearance validity reported for the couple, if provided.';
comment on column csts.couple.formed_at is 'Timestamp recorded by ČSTS when the couple registration was created.';

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
  personal_class text not null,
  personal_points integer not null,
  personal_domestic_finale_count integer not null,
  personal_foreign_finale_count integer not null,
  personal_approved boolean not null,
  ranklist_ranking integer not null,
  ranklist_points integer not null,
  athlete_idt integer references csts.athlete(idt) on delete cascade,
  couple_id integer references csts.couple(id) on delete cascade,
  primary key (competitor_id, discipline),
  check (
    (athlete_idt is not null and couple_id is null)
    or (athlete_idt is null and couple_id is not null)
  )
);
comment on table csts.competitor_ranking is 'Discipline-level ČSTS ranking data keyed by athlete or couple competitors.';
comment on column csts.competitor_ranking.competitor_id is 'Identifier of the ČSTS competitor entry for the ranking record.';
comment on column csts.competitor_ranking.discipline is 'Discipline (Standard/Latin/10 Dance) for the ranking data.';
comment on column csts.competitor_ranking.ranking_points_age is 'Age category attached to ČSTS ranklist points (e.g. Adult, Junior I).';
comment on column csts.competitor_ranking.ranking_age is 'Age category the ranklist entry applies to.';
comment on column csts.competitor_ranking.competitor_age is 'Age band assigned to the competitor record returned by ČSTS.';
comment on column csts.competitor_ranking.series is 'ČSTS ranking series for the competitor (e.g. DanceSport).';
comment on column csts.competitor_ranking.competitors is 'ČSTS category describing how many dancers the record covers (couple, solo, etc.).';
comment on column csts.competitor_ranking.class is 'ČSTS class assigned to the competitor for the discipline.';
comment on column csts.competitor_ranking.points is 'Current ČSTS ranking points accumulated for the discipline.';
comment on column csts.competitor_ranking.domestic_finale_count is 'Domestic finale count credited to the competitor in the discipline.';
comment on column csts.competitor_ranking.foreign_finale_count is 'Foreign finale count credited to the competitor in the discipline.';
comment on column csts.competitor_ranking.personal_class is 'Athlete-specific class for the discipline.';
comment on column csts.competitor_ranking.personal_points is 'Athlete-specific points assigned within the discipline.';
comment on column csts.competitor_ranking.personal_domestic_finale_count is 'Domestic finale count credited to the individual athlete.';
comment on column csts.competitor_ranking.personal_foreign_finale_count is 'Foreign finale count credited to the individual athlete.';
comment on column csts.competitor_ranking.personal_approved is 'Whether the athlete''s class for the discipline is approved.';
comment on column csts.competitor_ranking.ranklist_ranking is 'ČSTS published ranklist position for the discipline and age.';
comment on column csts.competitor_ranking.ranklist_points is 'ČSTS published ranklist points for the discipline and age.';
comment on column csts.competitor_ranking.athlete_idt is 'Links the ranking entry directly to an athlete for solo categories.';
comment on column csts.competitor_ranking.couple_id is 'Links the ranking entry to a couple competitor for partnered categories.';

create table if not exists csts.athlete_primary_category (
  athlete_idt integer not null references csts.athlete(idt) on delete cascade,
  category text not null,
  competitor_id integer not null,
  discipline text not null,
  primary key (athlete_idt, category),
  foreign key (competitor_id, discipline)
    references csts.competitor_ranking (competitor_id, discipline)
    on delete cascade,
  check (category in ('stt', 'lat'))
);
comment on table csts.athlete_primary_category is 'Maps ČSTS provided category shortcuts (stt/lat) to discipline-specific ranking records.';
comment on column csts.athlete_primary_category.category is 'ČSTS shortcut key referencing the discipline-specific ranking record.';

create unique index if not exists couple_couple_idt_key
  on csts.couple (couple_idt);

create index if not exists couple_first_athlete_idx
  on csts.couple (first_athlete_idt);

create index if not exists couple_second_athlete_idx
  on csts.couple (second_athlete_idt);

create unique index if not exists competitor_ranking_athlete_unique
  on csts.competitor_ranking (athlete_idt, discipline)
  where athlete_idt is not null;

create unique index if not exists competitor_ranking_couple_unique
  on csts.competitor_ranking (couple_id, discipline)
  where couple_id is not null;

create index if not exists competitor_ranking_athlete_idx
  on csts.competitor_ranking (athlete_idt);

create index if not exists competitor_ranking_couple_idx
  on csts.competitor_ranking (couple_id);
