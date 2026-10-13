create or replace view crawler.frontier_failure_group as
select
  federation,
  kind,
  failure,
  http_status,
  left(regexp_replace(coalesce(nullif(error_text, ''), '-'), '\s+', ' ', 'g'), 180) as error_fingerprint,
  count(*)::int as count,
  max(failed_at) as failed_at
from crawler.frontier_failure
group by federation, kind, failure, http_status, error_fingerprint;

comment on view crawler.frontier_failure_group is '@name crawler_target_failure_group
@behavior +list -connection -insert -update -delete';
revoke all on crawler.frontier_failure_group from public;
grant select on crawler.frontier_failure_group to system_admin;
