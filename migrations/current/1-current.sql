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

do $$
begin
  create type federated.competitor_category_progress_input as (
    category_id bigint,
    points numeric(10,3),
    domestic_finale integer,
    foreign_finale integer
  );
exception
  when duplicate_object then null;
end;
$$;

create or replace function federated.replace_competitor_category_progress(
  in_federation    text,
  in_competitor_id bigint,
  in_entries       federated.competitor_category_progress_input[] default '{}'::federated.competitor_category_progress_input[]
)
  returns void
  language plpgsql
  set search_path = federated, pg_temp
as $$
begin
  delete from federated.competitor_category_progress
  where federation = in_federation
    and competitor_id = in_competitor_id;

  insert into federated.competitor_category_progress (
    federation,
    competitor_id,
    category_id,
    points,
    domestic_finale,
    foreign_finale
  )
  select
    in_federation,
    in_competitor_id,
    (entry).category_id,
    coalesce((entry).points, 0),
    coalesce((entry).domestic_finale, 0),
    coalesce((entry).foreign_finale, 0)
  from unnest(coalesce(in_entries, '{}'::federated.competitor_category_progress_input[])) as e(entry)
  where (entry).category_id is not null;
end;
$$;
