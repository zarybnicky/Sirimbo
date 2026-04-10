alter table aktuality
  add column if not exists is_visible boolean default true not null;
--!include policies/aktuality.sql

alter table tenant_trainer
  alter column is_visible set not null,
  add column if not exists is_external boolean default false not null;

alter table cohort
  add column if not exists is_archived boolean default false not null;

update cohort set is_archived = is_visible where is_archived <> is_visible;
--!include policies/cohort.sql

alter table person
  add column if not exists note text default '' not null;
