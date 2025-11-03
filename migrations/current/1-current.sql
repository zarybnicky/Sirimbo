alter table if exists csts.ingest
  add column if not exists last_error text,
  add column if not exists checked_without_change integer not null default 0;

update csts.ingest
set checked_without_change = coalesce(checked_without_change, 0);

alter table if exists csts.athlete
  add column if not exists last_checked timestamptz default now();

update csts.athlete
set last_checked = coalesce(last_checked, fetched_at);
