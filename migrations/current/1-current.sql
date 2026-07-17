--! include functions/event_instance_registration_last_attended.sql
--! include functions/event_instance_approx_price.sql
--! include functions/set_event_instance_registration.sql
--! include functions/set_lesson_demand.sql

drop function if exists federated.competition_sankey_links;

create or replace function public.current_claims() returns jsonb
    language sql stable security definer
as $$
  select to_jsonb(app_private.create_jwt_token(users))
    - 'exp'
    - 'is_member'
    - 'is_trainer'
    - 'is_admin'
  from users
  where id = nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$;

grant all on function public.current_claims() to anonymous;

comment on function competition_brief is '@omit';
comment on function competition_report is '@omit';

alter table crawler.frontier
  add column if not exists last_response_id bigint,
  add column if not exists last_successful_response_id bigint;

create index if not exists frontier_last_response_id_idx
  on crawler.frontier (last_response_id)
  where last_response_id is not null;

create index if not exists frontier_last_successful_response_id_idx
  on crawler.frontier (last_successful_response_id)
  where last_successful_response_id is not null;

do $$
begin
  if not exists (select 1 from pg_constraint where conrelid = 'crawler.frontier'::regclass and conname = 'frontier_last_response_id_fkey') then
    alter table crawler.frontier
      add constraint frontier_last_response_id_fkey
      foreign key (last_response_id)
      references crawler.json_response (id)
      on delete set null;
  end if;

  if not exists (select 1 from pg_constraint where conrelid = 'crawler.frontier'::regclass and conname = 'frontier_last_successful_response_id_fkey') then
    alter table crawler.frontier
      add constraint frontier_last_successful_response_id_fkey
      foreign key (last_successful_response_id)
      references crawler.json_response (id)
      on delete set null;
  end if;
end;
$$;

with latest_response as (
  select distinct on (frontier_id) frontier_id, id
  from crawler.json_response
  order by frontier_id, fetched_at desc, id desc
), latest_successful_response as (
  select distinct on (frontier_id) frontier_id, id
  from crawler.json_response
  where error is null
    and http_status between 200 and 399
  order by frontier_id, fetched_at desc, id desc
)
update crawler.frontier f
set
  last_response_id = latest_response.id,
  last_successful_response_id = latest_successful_response.id
from latest_response
left join latest_successful_response using (frontier_id)
where latest_response.frontier_id = f.id
  and (
    f.last_response_id is distinct from latest_response.id
    or f.last_successful_response_id is distinct from latest_successful_response.id
  );

create or replace view crawler.frontier_failure as
select
  f.id,
  f.federation,
  f.kind,
  f.key,
  f.fetch_status,
  f.process_status,
  f.error_count,
  f.next_fetch_at,
  case
    when f.fetch_status in ('error', 'transient') and f.process_status = 'error'
      then greatest(
        coalesce(jr.fetched_at, f.last_fetched_at, f.discovered_at),
        coalesce(f.last_process_error_at, f.last_fetched_at, f.discovered_at)
      )
    when f.process_status = 'error'
      then coalesce(f.last_process_error_at, f.last_fetched_at, f.discovered_at)
    else coalesce(jr.fetched_at, f.last_fetched_at, f.discovered_at)
  end as failed_at,
  jr.url,
  jr.http_status,
  jr.error as response_error,
  f.last_process_error as process_error,
  concat_ws(
    '+',
    case when f.fetch_status in ('error', 'transient') then 'fetch' end,
    case when f.process_status = 'error' then 'process' end
  ) as failure,
  coalesce(
    case when f.process_status = 'error' then f.last_process_error end,
    jr.error,
    f.last_process_error,
    ''
  ) as error_text
from crawler.frontier f
left join crawler.json_response jr on jr.id = f.last_response_id
where f.fetch_status in ('error', 'transient')
   or f.process_status = 'error';

drop index if exists crawler.json_response_frontier_fetched_desc_idx;

drop table if exists crawler.html_response;
drop table if exists crawler.html_response_cache;
