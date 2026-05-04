--! Previous: sha1:c33857632e79aae14292a17d03319c0605ab6fcc
--! Hash: sha1:f24f4af377575b72add7fdf8b5082843b7deaee0

--! split: 1-current.sql
DROP VIEW IF EXISTS crawler.frontier_failure;
CREATE VIEW crawler.frontier_failure AS
SELECT
  f.id,
  f.federation,
  f.kind,
  f.key,
  f.fetch_status,
  f.process_status,
  f.error_count,
  f.next_fetch_at,
  CASE
    WHEN f.fetch_status IN ('error', 'transient') AND f.process_status = 'error'
      THEN greatest(
        coalesce(jr.fetched_at, f.last_fetched_at, f.discovered_at),
        coalesce(f.last_process_error_at, f.last_fetched_at, f.discovered_at)
      )
    WHEN f.process_status = 'error'
      THEN coalesce(f.last_process_error_at, f.last_fetched_at, f.discovered_at)
    ELSE coalesce(jr.fetched_at, f.last_fetched_at, f.discovered_at)
  END AS failed_at,
  jr.url,
  jr.http_status,
  jr.error AS response_error,
  f.last_process_error AS process_error,
  concat_ws(
    '+',
    CASE WHEN f.fetch_status IN ('error', 'transient') THEN 'fetch' END,
    CASE WHEN f.process_status = 'error' THEN 'process' END
  ) AS failure,
  coalesce(
    CASE WHEN f.process_status = 'error' THEN f.last_process_error END,
    jr.error,
    f.last_process_error,
    ''
  ) AS error_text
FROM crawler.frontier f
LEFT JOIN LATERAL (
  SELECT jr.*
  FROM crawler.json_response jr
  WHERE jr.frontier_id = f.id
  ORDER BY jr.fetched_at DESC
  LIMIT 1
) jr ON true
WHERE f.fetch_status IN ('error', 'transient')
   OR f.process_status = 'error';

CREATE OR REPLACE FUNCTION crawler.frontier_fetch_due(allow_refetch boolean)
RETURNS TABLE (
  id bigint,
  federation text,
  kind text,
  key text,
  discovered_at timestamptz,
  last_fetched_at timestamptz,
  next_fetch_at timestamptz,
  due_at timestamptz
) LANGUAGE sql STABLE AS $$
  SELECT
    f.id,
    f.federation,
    f.kind,
    f.key,
    f.discovered_at,
    f.last_fetched_at,
    f.next_fetch_at,
    coalesce(f.next_fetch_at, f.discovered_at) AS due_at
  FROM crawler.frontier f
  WHERE (f.next_fetch_at IS NULL OR f.next_fetch_at <= now())
    AND (
      f.fetch_status IN ('pending', 'transient')
      OR (
        coalesce(allow_refetch, false)
        AND f.fetch_status = 'ok'
        AND f.process_status = 'ok'
      )
    );
$$;

DROP VIEW IF EXISTS crawler.frontier_fetch_job;
CREATE VIEW crawler.frontier_fetch_job AS
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
    WHEN j.run_at <= now() THEN 'ready'
    ELSE 'delayed'
  END AS state
FROM jobs j
JOIN crawler.frontier f ON f.id = j.frontier_id
WHERE j.frontier_id IS NOT NULL;

WITH latest_response AS (
  SELECT DISTINCT ON (jr.frontier_id)
    jr.frontier_id,
    jr.http_status,
    jr.error
  FROM crawler.json_response jr
  ORDER BY jr.frontier_id, jr.fetched_at DESC
)
UPDATE crawler.frontier f
SET fetch_status = 'transient',
    process_status = 'pending',
    last_process_error = NULL,
    last_process_error_at = NULL,
    error_count = greatest(f.error_count, 1),
    next_fetch_at = now()
FROM latest_response lr
WHERE lr.frontier_id = f.id
  AND f.fetch_status IS DISTINCT FROM 'transient'::crawler.fetch_status
  AND (
    lr.http_status = 429
    OR lr.http_status >= 500
    OR (
      lr.http_status IS NULL
      AND coalesce(lr.error, '') ~* '(abort|timeout|timed out|econn|socket|network|terminated|fetch failed)'
    )
  );

UPDATE crawler.frontier
SET process_status = 'ok',
    last_process_error = NULL,
    last_process_error_at = NULL
WHERE fetch_status = 'gone'
  AND process_status = 'pending';

DROP INDEX IF EXISTS crawler.frontier_federation_kind_next_fetch_at_idx;
CREATE INDEX frontier_federation_kind_next_fetch_at_idx
  ON crawler.frontier (federation, kind, next_fetch_at)
  WHERE fetch_status IN ('pending', 'transient')
     OR (fetch_status = 'ok' AND process_status = 'ok');

DROP INDEX IF EXISTS crawler.frontier_process_pending_ok_gone_pick_idx;
CREATE INDEX IF NOT EXISTS frontier_process_pending_ok_pick_idx
  ON crawler.frontier (last_fetched_at, discovered_at, id)
  WHERE process_status = 'pending'
    AND fetch_status = 'ok';
