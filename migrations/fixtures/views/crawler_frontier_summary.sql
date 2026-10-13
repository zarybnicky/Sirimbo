create or replace view crawler.frontier_summary as
select
  federation,
  kind,
  count(*)::int as total,
  count(*) filter (where fetch_status = 'pending')::int as fetch_pending,
  count(*) filter (where fetch_status = 'transient')::int as fetch_transient,
  count(*) filter (where fetch_status = 'error')::int as fetch_error,
  count(*) filter (where process_status = 'ok')::int as done,
  count(*) filter (where process_status = 'error')::int as process_error,
  count(*) filter (where process_status = 'pending' and fetch_status = 'ok')::int as process_ready,
  max(last_fetched_at) as latest,
  count(*) filter (where next_fetch_at <= now() and fetch_status in ('pending', 'transient'))::int as fetch_due,
  count(*) filter (where next_fetch_at <= now() and fetch_status = 'ok' and process_status = 'ok')::int as refetch_due
from crawler.frontier
group by federation, kind;

comment on view crawler.frontier_summary is '@name crawler_target_summary
@behavior +list -connection -insert -update -delete';
revoke all on crawler.frontier_summary from public;
grant select on crawler.frontier_summary to system_admin;
