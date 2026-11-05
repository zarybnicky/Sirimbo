create or replace view crawler.frontier_fetch_job AS
WITH jobs AS (
  SELECT
    j.id,
    j.key,
    substring(j.key FROM '^fetch:(.*):[0-9]+$') AS host,
    CASE
      WHEN (pj.payload->>'id') ~ '^[0-9]+$' THEN (pj.payload->>'id')::bigint
    END AS frontier_id,
    j.run_at,
    j.locked_at,
    j.attempts,
    j.max_attempts,
    j.last_error,
    j.updated_at
  FROM graphile_worker.jobs j
  JOIN graphile_worker._private_jobs pj ON pj.id = j.id
  WHERE j.task_identifier = 'frontier_fetch'
)
SELECT
  j.id AS job_id,
  j.key AS job_key,
  j.host,
  j.run_at,
  j.locked_at,
  j.attempts,
  j.max_attempts,
  j.last_error AS job_error,
  j.updated_at AS job_updated_at,
  f.id AS frontier_id,
  f.federation,
  f.kind,
  f.key AS frontier_key,
  f.fetch_status,
  f.process_status,
  f.last_process_error AS process_error,
  CASE
    WHEN j.attempts >= j.max_attempts THEN 'failed'
    WHEN j.locked_at IS NOT NULL THEN 'locked'
    ELSE 'queued'
  END AS state
FROM jobs j
JOIN crawler.frontier f ON f.id = j.frontier_id
WHERE j.frontier_id IS NOT NULL;


comment on view crawler.frontier_fetch_job is '@name crawler_fetch_job
@behavior -insert -update -delete -list';
grant select on crawler.frontier_fetch_job to system_admin;
