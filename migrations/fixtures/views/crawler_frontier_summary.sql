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
comment on column crawler.frontier_summary.federation is '@notNull';
comment on column crawler.frontier_summary.kind is '@notNull';
comment on column crawler.frontier_summary.total is '@notNull';
comment on column crawler.frontier_summary.fetch_pending is '@notNull';
comment on column crawler.frontier_summary.fetch_transient is '@notNull';
comment on column crawler.frontier_summary.fetch_error is '@notNull';
comment on column crawler.frontier_summary.done is '@notNull';
comment on column crawler.frontier_summary.process_error is '@notNull';
comment on column crawler.frontier_summary.process_ready is '@notNull';
comment on column crawler.frontier_summary.fetch_due is '@notNull';
comment on column crawler.frontier_summary.refetch_due is '@notNull';
grant select on crawler.frontier_summary to system_admin;
