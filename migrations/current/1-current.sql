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
  'professional',
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
