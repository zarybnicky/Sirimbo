DROP FUNCTION IF EXISTS public.event_instance_attendance_summary;

DROP FUNCTION IF EXISTS event_instances_for_range(only_type event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], only_mine boolean);
--!include functions/event_instances_for_range.sql

INSERT INTO crawler.json_response_cache (content)
SELECT to_jsonb(hc.content)
FROM crawler.html_response_cache AS hc
ON CONFLICT (content_hash) DO NOTHING;

INSERT INTO crawler.json_response (frontier_id, url, http_status, error, content_hash, fetched_at)
SELECT
  hr.frontier_id,
  hr.url,
  hr.http_status,
  hr.error,
  CASE
    WHEN hr.content_hash IS NOT NULL
    THEN encode(digest(to_jsonb(hc.content)::text, 'sha256'), 'hex')
  END AS content_hash,
  hr.fetched_at
FROM crawler.html_response hr
LEFT JOIN crawler.html_response_cache hc ON hr.content_hash = hc.content_hash
WHERE NOT EXISTS (SELECT 1 FROM crawler.json_response jr WHERE jr.frontier_id = hr.frontier_id AND jr.fetched_at = hr.fetched_at);

alter type crawler.fetch_status ADD VALUE IF NOT EXISTS 'transient';

ALTER TABLE crawler.frontier
  ADD COLUMN IF NOT EXISTS last_process_error text;

ALTER TABLE crawler.frontier
  ADD COLUMN IF NOT EXISTS last_process_error_at timestamptz;

UPDATE crawler.frontier
SET last_process_error_at = coalesce(last_fetched_at, discovered_at)
WHERE process_status = 'error'
  AND last_process_error_at IS NULL;

CREATE OR REPLACE VIEW crawler.frontier_failure AS
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

CREATE OR REPLACE FUNCTION crawler.failed_frontier_refetch_candidates(
  selected_federation text,
  selected_kind text,
  selected_key text DEFAULT NULL,
  selected_http_statuses int[] DEFAULT NULL,
  selected_error_contains text DEFAULT NULL,
  selected_limit int DEFAULT NULL
) RETURNS TABLE (
  id bigint,
  federation text,
  kind text,
  key text,
  fetch_status crawler.fetch_status,
  process_status crawler.process_status,
  error_count integer,
  next_fetch_at timestamptz,
  failed_at timestamptz,
  url text,
  http_status integer,
  error text
) LANGUAGE sql STABLE AS $$
  SELECT
    ff.id,
    ff.federation,
    ff.kind,
    ff.key,
    ff.fetch_status,
    ff.process_status,
    ff.error_count,
    ff.next_fetch_at,
    ff.failed_at,
    ff.url,
    ff.http_status,
    ff.error_text AS error
  FROM crawler.frontier_failure ff
  WHERE ff.federation = selected_federation
    AND ff.kind = selected_kind
    AND (selected_key IS NULL OR ff.key = selected_key)
    AND (selected_http_statuses IS NULL OR ff.http_status = ANY(selected_http_statuses))
    AND (
      selected_error_contains IS NULL
      OR ff.error_text ILIKE '%' || selected_error_contains || '%'
    )
  ORDER BY ff.failed_at DESC, ff.id DESC
  LIMIT coalesce(selected_limit, 2147483647);
$$;

CREATE OR REPLACE FUNCTION crawler.queue_frontier_refetch(selected_ids bigint[])
RETURNS TABLE (
  id bigint,
  federation text,
  kind text,
  key text,
  job_id bigint
) LANGUAGE sql VOLATILE AS $$
  WITH frontier AS (
    UPDATE crawler.frontier f
    SET fetch_status = 'pending',
        process_status = 'pending',
        last_process_error = NULL,
        last_process_error_at = NULL,
        error_count = 0,
        next_fetch_at = now()
    WHERE f.id = ANY(selected_ids)
    RETURNING f.id, f.federation, f.kind, f.key
  ), queued AS (
    SELECT
      (
        graphile_worker.add_job(
          identifier => 'frontier_schedule',
          payload => '{}'::json,
          run_at => now(),
          job_key => 'frontier_schedule',
          job_key_mode => 'replace'
        )
      ).id AS job_id
    WHERE EXISTS (SELECT 1 FROM frontier)
  )
  SELECT f.id, f.federation, f.kind, f.key, q.job_id
  FROM frontier f
  CROSS JOIN queued q;
$$;

CREATE OR REPLACE VIEW crawler.frontier_fetch_job AS
WITH jobs AS (
  SELECT
    j.id,
    j.key,
    substring(j.key FROM '^fetch:(.*):[0-9]+$') AS host,
    pj.payload,
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
  j.payload AS job_payload,
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
  f.error_count,
  f.next_fetch_at,
  jr.fetched_at AS response_fetched_at,
  jr.http_status AS response_http_status,
  jr.error AS response_error,
  jrc.content AS response_content,
  f.last_process_error AS process_error,
  CASE
    WHEN j.attempts >= j.max_attempts THEN 'failed'
    WHEN j.locked_at IS NOT NULL THEN 'locked'
    WHEN j.run_at <= now() THEN 'ready'
    ELSE 'delayed'
  END AS state
FROM jobs j
JOIN crawler.frontier f ON f.id = j.frontier_id
LEFT JOIN LATERAL (
  SELECT jr.*
  FROM crawler.json_response jr
  WHERE jr.frontier_id = f.id
  ORDER BY jr.fetched_at DESC
  LIMIT 1
) jr ON true
LEFT JOIN crawler.json_response_cache jrc ON jrc.content_hash = jr.content_hash
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
