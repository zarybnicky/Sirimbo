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
  source_kind    text NOT NULL,
  kind           federated.person_license_kind NOT NULL,
  discipline     federated.person_license_discipline NOT NULL DEFAULT 'general',
  grade          text,
  valid_until    date,
  status         federated.person_license_status NOT NULL DEFAULT 'unknown',
  created_at     timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (person_id, source_kind, kind, discipline)
);
CREATE INDEX if not exists person_license_source_scope_idx
  ON federated.person_license (federation, source_kind, person_id);

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
      and keep.source_kind = t.source_kind
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
