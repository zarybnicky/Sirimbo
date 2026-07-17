--! include functions/event_instance_registration_last_attended.sql
--! include functions/event_instance_approx_price.sql
--! include functions/index_advisor.sql
--! include functions/login.sql
--! include functions/register_using_invitation.sql
--! include functions/register_without_invitation.sql
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

create index if not exists json_response_content_hash_idx
  on crawler.json_response (content_hash);

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

-- Restore the verifier after historical migrations have finished replaying.
create or replace function public.verify_function(f regproc, relid regclass default 0)
returns void
language plpgsql volatile security invoker
set search_path = pg_catalog, public, pg_temp
as $$
declare
  issues text;
begin
  select string_agg(
    concat_ws(
      E'\n',
      format(
        '%s:%s:%s:%s: %s',
        issue.level,
        issue.sqlstate,
        coalesce(issue.lineno::text, '?'),
        issue.statement,
        issue.message
      ),
      'Query: ' || issue.query,
      'Detail: ' || issue.detail,
      'Hint: ' || issue.hint,
      'Context: ' || issue.context
    ),
    E'\n\n' order by issue.lineno nulls last, issue.level, issue.message
  ) into issues
  from plpgsql_check_function_tb(
    funcoid => f,
    relid => relid,
    fatal_errors => false,
    all_warnings => true
  ) issue;

  if issues is not null then
    raise exception 'Error when checking function %', f using detail = issues;
  end if;
end;
$$;

comment on function public.verify_function(regproc, regclass) is '@omit';
revoke execute on function public.verify_function(regproc, regclass) from public;

create or replace function app_private.drop_policies(tbl text) returns void language plpgsql as $$
declare
  target regclass := tbl::regclass;
  policy_name name;
begin
  for policy_name in
    select polname
    from pg_catalog.pg_policy
    where polrelid = target
  loop
    execute format('drop policy %I on %s', policy_name, target);
  end loop;
end;
$$;

create or replace function app_private.tg_users__encrypt_password() returns trigger language plpgsql as $$
declare
  v_salt varchar;
begin
  if length(new.u_pass) <> 40 then
    v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');
    new.u_pass := encode(digest(v_salt || new.u_pass || v_salt, 'sha1'), 'hex');
  end if;
  return new;
end;
$$;

create or replace function app_private.tg_users__trim_login() returns trigger language plpgsql as $$
begin
  new.u_login := trim(new.u_login);
  return new;
end;
$$;
