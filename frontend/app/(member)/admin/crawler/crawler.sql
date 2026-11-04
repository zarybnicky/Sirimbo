/* @name RefetchCrawlerErrors */
WITH updated AS (
  UPDATE crawler.frontier f
  SET fetch_status = 'pending',
      process_status = 'pending',
      last_process_error = NULL,
      last_process_error_at = NULL,
      error_count = 0,
      next_fetch_at = now()
  WHERE f.federation = :federation!
    AND f.kind = :kind!
    AND strpos(f.key, :keyContains!) > 0
    AND (f.fetch_status IN ('error', 'transient') OR f.process_status = 'error')
    AND (
      :failure::text IS NULL
      OR EXISTS (
        SELECT 1 FROM crawler.frontier_failure ff
        WHERE ff.id = f.id
          AND ff.failure = :failure
          AND ff.http_status IS NOT DISTINCT FROM :httpStatus::integer
          AND left(regexp_replace(coalesce(nullif(ff.error_text, ''), '-'), '\s+', ' ', 'g'), 180) = :errorFingerprint::text
      )
    )
  RETURNING f.id
)
SELECT count(*)::int AS "count!" FROM updated;

/* @name ReprocessCrawlerTargets */
WITH updated AS (
  UPDATE crawler.frontier f
  SET process_status = 'pending',
      last_process_error = NULL,
      last_process_error_at = NULL
  WHERE f.federation = :federation!
    AND f.kind = :kind!
    AND strpos(f.key, :keyContains!) > 0
    AND f.fetch_status = 'ok'
    AND f.process_status <> 'pending'
    AND (
      :failure::text IS NULL
      OR EXISTS (
        SELECT 1 FROM crawler.frontier_failure ff
        WHERE ff.id = f.id
          AND ff.failure = :failure
          AND ff.http_status IS NOT DISTINCT FROM :httpStatus::integer
          AND left(regexp_replace(coalesce(nullif(ff.error_text, ''), '-'), '\s+', ' ', 'g'), 180) = :errorFingerprint::text
      )
    )
  RETURNING f.id
)
SELECT count(*)::int AS "count!" FROM updated;

/* @name ScheduleCrawler */
SELECT (graphile_worker.add_job(
  identifier => 'frontier_schedule',
  payload => '{}'::json,
  run_at => now(),
  job_key => 'frontier_schedule',
  job_key_mode => 'replace'
)).id;

/* @name CompleteCrawlerJob */
-- complete_jobs() skips locked jobs; this action also removes those.
DELETE FROM graphile_worker._private_jobs
WHERE id IN (
  SELECT id FROM graphile_worker.jobs
  WHERE id = :jobId!::bigint
    AND task_identifier = 'frontier_fetch'
)
RETURNING id;
