create schema if not exists csts;

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

do $$
declare
  athlete_ranking_is_view boolean;
begin
  select exists (
    select 1
    from pg_catalog.pg_class c
    join pg_catalog.pg_namespace n on n.oid = c.relnamespace
    where c.relkind = 'v'
      and n.nspname = 'csts'
      and c.relname = 'athlete_ranking'
  )
  into athlete_ranking_is_view;

  if athlete_ranking_is_view then
    execute 'drop view csts.athlete_ranking';
  end if;
end;
$$;

drop table if exists csts.athlete_primary_category;
drop table if exists csts.athlete_personal_ranking;

create table if not exists csts.couple (
  id integer primary key,
  couple_idt integer not null,
  man_idt integer not null,
  woman_idt integer not null,
  medical_checkup_expiration date,
  formed_at timestamptz not null
);

do $$
begin
  if not exists (
    select 1
    from pg_constraint
    where conrelid = 'csts.couple'::regclass
      and conname = 'couple_man_idt_fkey'
  ) then
    alter table csts.couple
      add constraint couple_man_idt_fkey
        foreign key (man_idt) references csts.athlete(idt) on delete cascade;
  end if;

  if not exists (
    select 1
    from pg_constraint
    where conrelid = 'csts.couple'::regclass
      and conname = 'couple_woman_idt_fkey'
  ) then
    alter table csts.couple
      add constraint couple_woman_idt_fkey
        foreign key (woman_idt) references csts.athlete(idt) on delete cascade;
  end if;
end;
$$;

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
  check (
    (athlete_idt is not null and couple_id is null)
    or (athlete_idt is null and couple_id is not null)
  )
);

alter table csts.competitor_ranking
  drop column if exists personal_class,
  drop column if exists personal_points,
  drop column if exists personal_domestic_finale_count,
  drop column if exists personal_foreign_finale_count,
  drop column if exists personal_approved;

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

create unique index if not exists couple_couple_idt_key
  on csts.couple (couple_idt);

create index if not exists couple_man_idx
  on csts.couple (man_idt);

create index if not exists couple_woman_idx
  on csts.couple (woman_idt);

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
