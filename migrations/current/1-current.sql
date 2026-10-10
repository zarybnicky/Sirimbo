alter table crawler.json_response
  add column if not exists observation_count bigint not null default 1
  check (observation_count > 0);

--!include functions/crawler_store_response.sql

alter table federated.judge_score
  drop column if exists competition_id,
  drop column if exists created_at,
  drop column if exists federation,
  drop column if exists event_date,
  drop column if exists event_id,
  drop column if exists category_id;

do $$
begin
  if not exists (
    select 1 from pg_constraint
    where conrelid = 'federated.judge_score'::regclass
      and conname = 'judge_score_round_id_fkey'
  ) then
    alter table federated.judge_score
      add constraint judge_score_round_id_fkey foreign key (round_id)
      references federated.competition_round(id) on update cascade on delete cascade
      not valid;
  end if;
end $$;

alter table federated.judge_score validate constraint judge_score_round_id_fkey;
