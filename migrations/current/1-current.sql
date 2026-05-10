DROP INDEX IF EXISTS crawler.frontier_process_pending_ok_pick_idx;
CREATE INDEX frontier_process_pending_ok_pick_idx ON crawler.frontier USING btree (discovered_at, last_fetched_at, id) WHERE ((process_status = 'pending'::crawler.process_status) AND (fetch_status = 'ok'::crawler.fetch_status));

do $$
begin
  if not exists (select * from pg_type where typcategory='E' and typname = 'person_license_kind') then
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
end if;

if not exists (select * from pg_type where typcategory='E' and typname = 'person_license_discipline') then
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
end if;

if not exists (select * from pg_type where typcategory='E' and typname = 'person_license_status') then
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
end if;
end
$$;

CREATE TABLE if not exists federated.person_license (
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
CREATE INDEX if not exists person_license_federation_person_idx
  ON federated.person_license (federation, person_id);

do $$
begin
  if to_regclass('federated.person') is null
     or to_regclass('federated.competitor') is null
     or to_regclass('federated.judge_score') is null then
    return;
  end if;

  alter table if exists federated.person_license
    drop constraint if exists person_license_person_id_fkey;
  alter table if exists federated.competitor_component
    drop constraint if exists competitor_component_person_id_fkey,
    drop constraint if exists competitor_component_competitor_id_fkey;
  alter table if exists federated.competitor_category_progress
    drop constraint if exists competitor_category_progress_competitor_id_fkey;
  alter table if exists federated.person_club_membership
    drop constraint if exists person_club_membership_person_id_fkey;
  alter table if exists federated.competitor_club_affiliation
    drop constraint if exists competitor_club_affiliation_competitor_id_fkey;
  alter table if exists federated.event_official
    drop constraint if exists event_official_person_id_fkey;
  alter table if exists federated.competition_official
    drop constraint if exists competition_official_person_id_fkey;
  alter table if exists federated.competition_round_judge
    drop constraint if exists competition_round_judge_person_judge_id_fkey;
  alter table if exists federated.competition_entry
    drop constraint if exists competition_entry_competitor_id_fkey;
  alter table if exists federated.competition_round_result
    drop constraint if exists competition_round_result_competitor_id_fkey;
  alter table if exists federated.competition_result
    drop constraint if exists competition_result_competitor_id_fkey;
  alter table if exists federated.ranklist_entry
    drop constraint if exists ranklist_entry_competitor_id_fkey;
  alter table if exists federated.judge_score
    drop constraint if exists judge_score_judge_person_id_fkey,
    drop constraint if exists judge_score_competitor_id_fkey,
    drop constraint if exists judge_score_federation_judge_person_id_fkey,
    drop constraint if exists judge_score_federation_competitor_id_fkey;

  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'federated'
      and table_name = 'person'
      and column_name = 'external_id'
      and data_type <> 'bigint'
  ) then
    if exists (
      select 1
      from federated.person
      where external_id !~ '^[0-9]+$'
    ) then
      raise exception 'Cannot convert federated.person.external_id to bigint: non-numeric values exist';
    end if;

    alter table federated.person drop constraint if exists person_pkey;
    alter table federated.person drop column id;
    alter table federated.person
      alter column external_id type bigint
      using external_id::bigint;
    alter table federated.person
      add column id text generated always as (federation || ':' || external_id::text) stored;
    alter table federated.person add constraint person_pkey primary key (id);
  end if;

  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'federated'
      and table_name = 'competitor'
      and column_name = 'external_id'
      and data_type <> 'bigint'
  ) then
    if exists (
      select 1
      from federated.competitor
      where external_id !~ '^[0-9]+$'
    ) then
      raise exception 'Cannot convert federated.competitor.external_id to bigint: non-numeric values exist';
    end if;

    alter table federated.competitor drop constraint if exists competitor_pkey;
    alter table federated.competitor drop column id;
    alter table federated.competitor
      alter column external_id type bigint
      using external_id::bigint;
    alter table federated.competitor
      add column id text generated always as (federation || ':' || external_id::text) stored;
    alter table federated.competitor add constraint competitor_pkey primary key (id);
  end if;

  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'federated'
      and table_name = 'judge_score'
      and column_name = 'judge_person_id'
      and data_type <> 'bigint'
  ) then
    drop index if exists federated.judge_score_federation_judge_person_id_event_date_idx;
    drop index if exists federated.judge_score_federation_competitor_id_event_date_idx;

    alter table federated.judge_score drop constraint if exists judge_score_pkey;
    alter table federated.judge_score
      alter column judge_person_id type bigint
      using nullif(split_part(judge_person_id, ':', 2), '')::bigint,
      alter column competitor_id type bigint
      using nullif(split_part(competitor_id, ':', 2), '')::bigint;
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.judge_score'::regclass
      and conname = 'judge_score_pkey'
  ) then
    alter table federated.judge_score
      add constraint judge_score_pkey
      primary key (round_id, dance_order, judge_person_id, competitor_id, component);
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.person_license'::regclass
      and conname = 'person_license_person_id_fkey'
  ) then
    alter table federated.person_license
      add constraint person_license_person_id_fkey
      foreign key (person_id) references federated.person(id) on delete cascade;
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.competitor_component'::regclass
      and conname = 'competitor_component_competitor_id_fkey'
  ) then
    alter table federated.competitor_component
      add constraint competitor_component_competitor_id_fkey
      foreign key (competitor_id) references federated.competitor(id) on delete cascade;
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.competitor_component'::regclass
      and conname = 'competitor_component_person_id_fkey'
  ) then
    alter table federated.competitor_component
      add constraint competitor_component_person_id_fkey
      foreign key (person_id) references federated.person(id);
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.competitor_category_progress'::regclass
      and conname = 'competitor_category_progress_competitor_id_fkey'
  ) then
    alter table federated.competitor_category_progress
      add constraint competitor_category_progress_competitor_id_fkey
      foreign key (competitor_id) references federated.competitor(id);
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.person_club_membership'::regclass
      and conname = 'person_club_membership_person_id_fkey'
  ) then
    alter table federated.person_club_membership
      add constraint person_club_membership_person_id_fkey
      foreign key (person_id) references federated.person(id);
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.competitor_club_affiliation'::regclass
      and conname = 'competitor_club_affiliation_competitor_id_fkey'
  ) then
    alter table federated.competitor_club_affiliation
      add constraint competitor_club_affiliation_competitor_id_fkey
      foreign key (competitor_id) references federated.competitor(id);
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.event_official'::regclass
      and conname = 'event_official_person_id_fkey'
  ) then
    alter table federated.event_official
      add constraint event_official_person_id_fkey
      foreign key (person_id) references federated.person(id);
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.competition_official'::regclass
      and conname = 'competition_official_person_id_fkey'
  ) then
    alter table federated.competition_official
      add constraint competition_official_person_id_fkey
      foreign key (person_id) references federated.person(id);
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.competition_round_judge'::regclass
      and conname = 'competition_round_judge_person_judge_id_fkey'
  ) then
    alter table federated.competition_round_judge
      add constraint competition_round_judge_person_judge_id_fkey
      foreign key (person_judge_id) references federated.person(id);
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.competition_entry'::regclass
      and conname = 'competition_entry_competitor_id_fkey'
  ) then
    alter table federated.competition_entry
      add constraint competition_entry_competitor_id_fkey
      foreign key (competitor_id) references federated.competitor(id);
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.competition_round_result'::regclass
      and conname = 'competition_round_result_competitor_id_fkey'
  ) then
    alter table federated.competition_round_result
      add constraint competition_round_result_competitor_id_fkey
      foreign key (competitor_id) references federated.competitor(id);
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.competition_result'::regclass
      and conname = 'competition_result_competitor_id_fkey'
  ) then
    alter table federated.competition_result
      add constraint competition_result_competitor_id_fkey
      foreign key (competitor_id) references federated.competitor(id);
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.ranklist_entry'::regclass
      and conname = 'ranklist_entry_competitor_id_fkey'
  ) then
    alter table federated.ranklist_entry
      add constraint ranklist_entry_competitor_id_fkey
      foreign key (competitor_id) references federated.competitor(id);
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.judge_score'::regclass
      and conname = 'judge_score_federation_judge_person_id_fkey'
  ) then
    alter table federated.judge_score
      add constraint judge_score_federation_judge_person_id_fkey
      foreign key (federation, judge_person_id)
      references federated.person(federation, external_id);
  end if;

  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.judge_score'::regclass
      and conname = 'judge_score_federation_competitor_id_fkey'
  ) then
    alter table federated.judge_score
      add constraint judge_score_federation_competitor_id_fkey
      foreign key (federation, competitor_id)
      references federated.competitor(federation, external_id);
  end if;
end
$$;

create index if not exists judge_score_federation_judge_person_id_event_date_idx
  on federated.judge_score (federation, judge_person_id, event_date);
create index if not exists judge_score_federation_competitor_id_event_date_idx
  on federated.judge_score (federation, competitor_id, event_date);

create or replace function public.csts_athlete(idt int) returns text as $$
  select canonical_name from federated.person where federation = 'csts' and external_id = idt;
$$ language sql stable;

create or replace function public.wdsf_athlete(min int) returns text as $$
  select canonical_name from federated.person where federation = 'wdsf' and external_id = min;
$$ language sql stable;

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
  and p.external_id = nullif(regexp_replace(in_person.csts_id, '\D', '', 'g'), '')::bigint;
$$ language sql stable;

comment on function public.person_csts_progress is '@simpleCollections only';
grant all on function public.csts_athlete to anonymous;
grant all on function public.wdsf_athlete to anonymous;
grant all on function public.person_csts_progress to anonymous;

CREATE TABLE if not exists federated.person_license (
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
CREATE INDEX if not exists person_license_federation_person_idx
  ON federated.person_license (federation, person_id);

do $$
begin
  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'federated'
      and table_name = 'person_license'
      and column_name = 'source_kind'
  ) then
    delete from federated.person_license
    where federation = 'szts'
      and source_kind = 'judgeIndex'
      and kind = 'trainer';

    alter table federated.person_license drop constraint if exists person_license_pkey;
    alter table federated.person_license drop column source_kind;
    alter table federated.person_license add primary key (person_id, kind, discipline);
  end if;
end
$$;

DROP INDEX if exists federated.person_license_source_scope_idx;
DROP INDEX if exists federated.person_license_federation_source_kind_person_id_idx;

do $$
begin
  if exists (
    select 1
    from pg_enum e
    join pg_type t on t.oid = e.enumtypid
    join pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'federated'
      and t.typname = 'person_license_discipline'
      and e.enumlabel = 'professional'
  ) or exists (
    select label
    from unnest(array[
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
    ]) desired(label)
    where not exists (
      select 1
      from pg_enum e
      join pg_type t on t.oid = e.enumtypid
      join pg_namespace n on n.oid = t.typnamespace
      where n.nspname = 'federated'
        and t.typname = 'person_license_discipline'
        and e.enumlabel = desired.label
    )
  ) then
    alter table federated.person_license alter column discipline drop default;
    alter table federated.person_license
      alter column discipline type text
      using discipline::text;

    delete from federated.person_license t
    using federated.person_license keep
    where t.discipline = 'professional'
      and keep.person_id = t.person_id
      and keep.kind = t.kind
      and keep.discipline = 'unknown';

    update federated.person_license
    set discipline = 'unknown'
    where discipline = 'professional';

    drop type federated.person_license_discipline;
    create type federated.person_license_discipline as enum (
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

    alter table federated.person_license
      alter column discipline type federated.person_license_discipline
      using discipline::federated.person_license_discipline,
      alter column discipline set default 'general';
  end if;
end
$$;
