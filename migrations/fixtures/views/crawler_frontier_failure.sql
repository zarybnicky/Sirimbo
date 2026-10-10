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
        coalesce(jr.last_observed_at, jr.fetched_at, f.last_fetched_at, f.discovered_at),
        coalesce(f.last_process_error_at, f.last_fetched_at, f.discovered_at)
      )
    when f.process_status = 'error'
      then coalesce(f.last_process_error_at, f.last_fetched_at, f.discovered_at)
    else coalesce(jr.last_observed_at, jr.fetched_at, f.last_fetched_at, f.discovered_at)
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

revoke all on crawler.frontier_failure from public;
