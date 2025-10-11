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

create table if not exists csts.athlete_ranking (
  athlete_idt integer not null references csts.athlete(idt) on delete cascade,
  competitor_id integer not null,
  discipline text not null,
  ranking_points_age text not null,
  ranking_age text not null,
  competitor_age text not null,
  series text not null,
  competitors text not null,
  couple_class text not null,
  couple_points integer not null,
  couple_domestic_finale_count integer not null,
  couple_foreign_finale_count integer not null,
  personal_class text not null,
  personal_points integer not null,
  personal_domestic_finale_count integer not null,
  personal_foreign_finale_count integer not null,
  personal_approved boolean not null,
  ranklist_ranking integer not null,
  ranklist_points integer not null,
  couple_idt integer not null,
  partner_name text not null,
  partner_idt integer not null,
  medical_checkup_expiration date,
  couple_created_at timestamptz not null,
  primary key (athlete_idt, competitor_id, discipline)
);
comment on table csts.athlete_ranking is 'Ranking metadata for each athlete across ČSTS disciplines and series.';
comment on column csts.athlete_ranking.ranking_points_age is 'Age category attached to ČSTS ranklist points (e.g. Adult, Junior I).';
comment on column csts.athlete_ranking.ranking_age is 'Age category the ranklist entry applies to.';
comment on column csts.athlete_ranking.competitor_age is 'Age band assigned to the competitor record returned by ČSTS.';
comment on column csts.athlete_ranking.discipline is 'Discipline (Standard/Latin/10 Dance) associated with the competitor record.';
comment on column csts.athlete_ranking.series is 'ČSTS ranking series for the competitor (e.g. DanceSport).';
comment on column csts.athlete_ranking.competitors is 'ČSTS category describing how many dancers the record covers (couple, solo, etc.).';
comment on column csts.athlete_ranking.couple_class is 'ČSTS class assigned to the couple for the relevant discipline.';
comment on column csts.athlete_ranking.couple_points is 'Current ČSTS ranking points accumulated by the couple for the discipline.';
comment on column csts.athlete_ranking.couple_domestic_finale_count is 'Domestic finale count credited to the couple in the discipline.';
comment on column csts.athlete_ranking.couple_foreign_finale_count is 'Foreign finale count credited to the couple in the discipline.';
comment on column csts.athlete_ranking.personal_class is 'Athlete-specific class within the couple for the discipline.';
comment on column csts.athlete_ranking.personal_points is 'Athlete-specific points assigned within the couple for the discipline.';
comment on column csts.athlete_ranking.personal_domestic_finale_count is 'Domestic finale count credited to the individual athlete.';
comment on column csts.athlete_ranking.personal_foreign_finale_count is 'Foreign finale count credited to the individual athlete.';
comment on column csts.athlete_ranking.personal_approved is 'Whether the athlete''s class for the discipline is approved.';
comment on column csts.athlete_ranking.ranklist_ranking is 'ČSTS published ranklist position for the discipline and age.';
comment on column csts.athlete_ranking.ranklist_points is 'ČSTS published ranklist points for the discipline and age.';
comment on column csts.athlete_ranking.couple_idt is 'ČSTS identifier of the competitive pairing or registration entry.';
comment on column csts.athlete_ranking.partner_name is 'Name of the athlete''s partner within the competitive pairing.';
comment on column csts.athlete_ranking.partner_idt is 'ČSTS identifier of the partner within the competitive pairing.';
comment on column csts.athlete_ranking.medical_checkup_expiration is 'Validity date for the couple''s medical clearance, if present.';
comment on column csts.athlete_ranking.couple_created_at is 'Timestamp recorded by ČSTS for when the competitive pairing was formed.';

create table if not exists csts.athlete_primary_category (
  athlete_idt integer not null references csts.athlete(idt) on delete cascade,
  category text not null,
  competitor_id integer not null,
  discipline text not null,
  primary key (athlete_idt, category),
  foreign key (athlete_idt, competitor_id, discipline)
    references csts.athlete_ranking (athlete_idt, competitor_id, discipline)
    on delete cascade,
  check (category in ('stt', 'lat'))
);
comment on table csts.athlete_primary_category is 'Maps ČSTS provided category shortcuts (stt/lat) to concrete ranking records.';
comment on column csts.athlete_primary_category.category is 'ČSTS shortcut key referencing the discipline-specific ranking record.';

create index if not exists athlete_ranking_partner_idx
  on csts.athlete_ranking (partner_idt);

create index if not exists athlete_ranking_competitor_idx
  on csts.athlete_ranking (competitor_id);
