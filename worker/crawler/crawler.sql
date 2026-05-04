/* @name GetFrontierForUpdate */
SELECT id, federation, kind, key, fetch_status, error_count, meta
FROM crawler.frontier
WHERE id = :id::bigint
FOR UPDATE;

/* @name GetDistinctFrontierKinds */
SELECT federation, kind
FROM crawler.frontier
GROUP BY federation, kind;

/* @name GetFrontierKindStatus */
WITH frontiers AS (
  SELECT
    f.*,
    row_number() OVER (
      PARTITION BY f.federation, f.kind
      ORDER BY
        CASE WHEN f.fetch_status = 'pending' THEN 0 ELSE 1 END,
        f.discovered_at DESC,
        f.id
    ) AS key_rank
  FROM crawler.frontier f
  WHERE (:federation::text IS NULL OR f.federation = :federation)
    AND (:kind::text IS NULL OR f.kind = :kind)
), response_counts AS (
  SELECT
    f.federation,
    f.kind,
    count(*)::int AS response_rows,
    max(r.fetched_at) AS latest_response
  FROM frontiers f
  JOIN crawler.json_response r ON r.frontier_id = f.id
  GROUP BY f.federation, f.kind
), due_status AS (
  SELECT
    df.federation,
    df.kind,
    count(*)::int AS fetch_due
  FROM crawler.frontier_fetch_due(:allowRefetch::boolean) df
  WHERE (:federation::text IS NULL OR df.federation = :federation)
    AND (:kind::text IS NULL OR df.kind = :kind)
  GROUP BY df.federation, df.kind
), status AS (
  SELECT
    federation,
    kind,
    count(*)::int AS total,
    coalesce(
      string_agg(key, ', ' ORDER BY key_rank) FILTER (WHERE key <> '' AND key_rank <= 3)
        || CASE WHEN count(*) FILTER (WHERE key <> '') > 3 THEN ', ...' ELSE '' END,
      '-'
    ) AS keys,
    count(*) FILTER (WHERE fetch_status = 'pending')::int AS fetch_pending,
    count(*) FILTER (WHERE fetch_status = 'transient')::int AS fetch_transient,
    count(*) FILTER (WHERE fetch_status = 'ok')::int AS fetch_ok,
    count(*) FILTER (WHERE fetch_status = 'gone')::int AS fetch_gone,
    count(*) FILTER (WHERE fetch_status = 'error')::int AS fetch_error,
    count(*) FILTER (WHERE process_status = 'pending')::int AS process_pending,
    count(*) FILTER (WHERE process_status = 'ok')::int AS process_ok,
    count(*) FILTER (WHERE process_status = 'error')::int AS process_error,
    max(last_fetched_at) AS latest_fetch
  FROM frontiers f
  GROUP BY federation, kind
)
SELECT
  s.*,
  coalesce(ds.fetch_due, 0)::int AS fetch_due,
  coalesce(rc.response_rows, 0)::int AS response_rows,
  coalesce(s.latest_fetch, rc.latest_response) AS latest
FROM status s
LEFT JOIN response_counts rc ON rc.federation = s.federation AND rc.kind = s.kind
LEFT JOIN due_status ds ON ds.federation = s.federation AND ds.kind = s.kind
ORDER BY s.federation, s.kind;

/* @name QueueCrawlerKick */
SELECT (
  graphile_worker.add_job(
    identifier => 'frontier_schedule',
    payload => '{}'::json,
    run_at => now(),
    job_key => 'frontier_schedule',
    job_key_mode => 'replace'
  )
).id AS job_id;

/* @name GetBacktestFrontierResponses */
SELECT f.id, jr.url, jr.http_status, jrc.content
FROM crawler.frontier f
JOIN LATERAL (
  SELECT jr.*
  FROM crawler.json_response jr
  WHERE jr.frontier_id = f.id
    AND jr.error IS NULL
    AND (jr.http_status IS NULL OR jr.http_status < 400)
  ORDER BY jr.fetched_at DESC
  LIMIT 1
  ) jr ON true
JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
WHERE f.federation = :federation
  AND f.kind = :kind;

/* @name GetLatestFrontierFailures */
SELECT
  id,
  federation,
  kind,
  key,
  fetch_status,
  process_status,
  error_count,
  next_fetch_at,
  failed_at,
  url,
  http_status,
  response_error,
  process_error,
  failure
FROM crawler.frontier_failure
WHERE (:federation::text IS NULL OR federation = :federation)
  AND (:kind::text IS NULL OR kind = :kind)
  AND (
    :excludeHttpStatuses::int[] IS NULL
    OR http_status IS NULL
    OR NOT (http_status = ANY(:excludeHttpStatuses::int[]))
  )
ORDER BY failed_at DESC, id DESC
LIMIT :limit;

/* @name GetFrontierFailureGroups */
WITH failures AS (
  SELECT
    id,
    federation,
    kind,
    key,
    failed_at,
    http_status,
    failure,
    coalesce(nullif(error_text, ''), '-') AS error_text
  FROM crawler.frontier_failure
  WHERE (:federation::text IS NULL OR federation = :federation)
    AND (:kind::text IS NULL OR kind = :kind)
    AND (
      :excludeHttpStatuses::int[] IS NULL
      OR http_status IS NULL
      OR NOT (http_status = ANY(:excludeHttpStatuses::int[]))
    )
), fingerprinted AS (
  SELECT
    *,
    left(regexp_replace(error_text, '\s+', ' ', 'g'), 180) AS error_fingerprint
  FROM failures
), ranked AS (
  SELECT
    *,
    row_number() OVER (
      PARTITION BY federation, kind, failure, http_status, error_fingerprint
      ORDER BY failed_at DESC, id DESC
    ) AS sample_rank
  FROM fingerprinted
)
SELECT
  federation,
  kind,
  failure,
  http_status,
  error_fingerprint,
  count(*)::int AS count,
  max(failed_at) AS latest_failed_at,
  (array_agg(id ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank = 1))[1]
    AS sample_id,
  (array_agg(key ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank = 1))[1]
    AS sample_key,
  string_agg(id::text || ':' || key, ', ' ORDER BY failed_at DESC, id DESC)
    FILTER (WHERE sample_rank <= 5) AS samples
FROM ranked
GROUP BY federation, kind, failure, http_status, error_fingerprint
ORDER BY count DESC, latest_failed_at DESC
LIMIT :limit;

/* @name GetCrawlerFetchJobSummary */
SELECT
  count(*)::int AS total,
  count(*) FILTER (WHERE state = 'ready')::int AS ready,
  count(*) FILTER (WHERE state = 'delayed')::int AS delayed,
  count(*) FILTER (WHERE state = 'locked')::int AS locked,
  count(*) FILTER (WHERE state = 'failed')::int AS failed,
  min(run_at) FILTER (WHERE state = 'ready') AS oldest_ready_at,
  min(run_at) FILTER (WHERE state = 'delayed') AS next_run_at,
  max(job_updated_at) AS latest_update_at,
  (array_agg(left(job_error, 300) ORDER BY job_updated_at DESC)
    FILTER (WHERE job_error IS NOT NULL))[1] AS latest_error
FROM crawler.frontier_fetch_job;

/* @name GetCrawlerControlJobStatus */
SELECT
  task_identifier,
  count(*)::int AS total,
  count(*) FILTER (
    WHERE locked_at IS NULL AND run_at <= now() AND attempts < max_attempts
  )::int AS ready,
  count(*) FILTER (
    WHERE locked_at IS NULL AND run_at > now() AND attempts < max_attempts
  )::int AS delayed,
  count(*) FILTER (WHERE locked_at IS NOT NULL)::int AS locked,
  count(*) FILTER (WHERE attempts >= max_attempts)::int AS failed,
  min(run_at) FILTER (
    WHERE locked_at IS NULL AND run_at <= now() AND attempts < max_attempts
  ) AS oldest_ready_at,
  min(run_at) FILTER (
    WHERE locked_at IS NULL AND run_at > now() AND attempts < max_attempts
  ) AS next_run_at,
  max(updated_at) AS latest_update_at,
  (array_agg(left(last_error, 300) ORDER BY updated_at DESC)
    FILTER (WHERE last_error IS NOT NULL))[1] AS latest_error
FROM graphile_worker.jobs
WHERE task_identifier IN ('frontier_schedule', 'frontier_process')
GROUP BY task_identifier
ORDER BY task_identifier;

/* @name GetCrawlerFrontierJobs */
SELECT
  job_id,
  run_at,
  locked_at,
  attempts,
  max_attempts,
  job_error,
  job_updated_at,
  frontier_id,
  federation,
  kind,
  frontier_key,
  fetch_status,
  process_status,
  response_http_status,
  response_error,
  CASE
    WHEN state = 'failed'
      OR job_error IS NOT NULL
      OR response_error IS NOT NULL
      OR process_error IS NOT NULL
      OR fetch_status IN ('error', 'transient')
      OR process_status = 'error'
      THEN response_content
  END AS response_content,
  process_error,
  state
FROM crawler.frontier_fetch_job
WHERE (
  :mode::text = 'all'
  OR (:mode::text = 'failed' AND state = 'failed')
  OR (:mode::text = 'active' AND state <> 'failed')
)
  AND (:federation::text IS NULL OR federation = :federation)
  AND (:kind::text IS NULL OR kind = :kind)
ORDER BY
  CASE state
    WHEN 'failed' THEN 0
    WHEN 'locked' THEN 1
    WHEN 'ready' THEN 2
    ELSE 3
  END,
  job_updated_at DESC,
  run_at,
  job_id DESC
LIMIT :limit;

/* @name GetCrawlerScheduleStatus */
WITH fetch_jobs AS (
  SELECT
    host,
    count(*) FILTER (WHERE state IN ('ready', 'delayed'))::int AS queued,
    count(*) FILTER (WHERE state = 'ready')::int AS ready,
    count(*) FILTER (WHERE state = 'delayed')::int AS delayed,
    count(*) FILTER (WHERE state = 'locked')::int AS locked,
    min(run_at) FILTER (WHERE state IN ('ready', 'delayed')) AS next_run_at,
    max(run_at) FILTER (WHERE state <> 'failed') AS queue_tail_at
  FROM crawler.frontier_fetch_job
  WHERE host IS NOT NULL
  GROUP BY host
)
SELECT
  COALESCE(r.host, fj.host) AS host,
  r.max_requests,
  r.per_interval::text AS per_interval,
  (extract(epoch from r.spacing) * 1000)::int AS spacing,
  r.next_available_at,
  COALESCE(fj.queued, 0)::int AS queued,
  COALESCE(fj.ready, 0)::int AS ready,
  COALESCE(fj.delayed, 0)::int AS delayed,
  COALESCE(fj.locked, 0)::int AS locked,
  fj.next_run_at,
  fj.queue_tail_at
FROM crawler.rate_limit_rule r
FULL JOIN fetch_jobs fj USING (host)
ORDER BY host;

/* @name GetCrawlerBacklogStatus */
WITH due_fetch AS (
  SELECT df.*
  FROM crawler.frontier_fetch_due(:allowRefetch::boolean) df
  WHERE (:federation::text IS NULL OR df.federation = :federation)
    AND (:kind::text IS NULL OR df.kind = :kind)
), due_status AS (
  SELECT
    federation,
    kind,
    count(*)::int AS fetch_due,
    min(due_at) AS oldest_due_at,
    (array_agg(id ORDER BY due_at, last_fetched_at NULLS FIRST, id))[1] AS sample_due_id,
    (array_agg(key ORDER BY due_at, last_fetched_at NULLS FIRST, id))[1] AS sample_due_key
  FROM due_fetch
  GROUP BY federation, kind
), process_status AS (
  SELECT
    f.federation,
    f.kind,
    count(*)::int AS process_ready,
    min(f.last_fetched_at) AS oldest_process_ready_at,
    (array_agg(f.id ORDER BY f.last_fetched_at NULLS FIRST, f.discovered_at, f.id))[1]
      AS sample_process_id
  FROM crawler.frontier f
  WHERE (:federation::text IS NULL OR f.federation = :federation)
    AND (:kind::text IS NULL OR f.kind = :kind)
    AND f.process_status = 'pending'
    AND f.fetch_status = 'ok'
  GROUP BY f.federation, f.kind
), fetch_jobs AS (
  SELECT
    federation,
    kind,
    count(*) FILTER (WHERE state IN ('ready', 'delayed'))::int AS queued_fetch,
    count(*) FILTER (WHERE state = 'ready')::int AS ready_fetch,
    count(*) FILTER (WHERE state = 'delayed')::int AS delayed_fetch,
    count(*) FILTER (WHERE state = 'locked')::int AS locked_fetch
  FROM crawler.frontier_fetch_job
  WHERE (:federation::text IS NULL OR federation = :federation)
    AND (:kind::text IS NULL OR kind = :kind)
  GROUP BY federation, kind
), kind_totals AS (
  SELECT
    f.federation,
    f.kind,
    count(*)::int AS total
  FROM crawler.frontier f
  WHERE (:federation::text IS NULL OR f.federation = :federation)
    AND (:kind::text IS NULL OR f.kind = :kind)
  GROUP BY f.federation, f.kind
)
SELECT
  kt.federation,
  kt.kind,
  kt.total,
  coalesce(ds.fetch_due, 0)::int AS fetch_due,
  ds.oldest_due_at,
  ds.sample_due_id,
  ds.sample_due_key,
  coalesce(ps.process_ready, 0)::int AS process_ready,
  ps.oldest_process_ready_at,
  ps.sample_process_id,
  coalesce(fj.queued_fetch, 0)::int AS queued_fetch,
  coalesce(fj.ready_fetch, 0)::int AS ready_fetch,
  coalesce(fj.delayed_fetch, 0)::int AS delayed_fetch,
  coalesce(fj.locked_fetch, 0)::int AS locked_fetch,
  (coalesce(fj.queued_fetch, 0) + coalesce(fj.locked_fetch, 0))::int
    AS scheduled_fetch,
  greatest(
    coalesce(ds.fetch_due, 0) - coalesce(fj.queued_fetch, 0) - coalesce(fj.locked_fetch, 0),
    0
  )::int AS unscheduled_fetch
FROM kind_totals kt
LEFT JOIN due_status ds USING (federation, kind)
LEFT JOIN process_status ps USING (federation, kind)
LEFT JOIN fetch_jobs fj USING (federation, kind)
ORDER BY kt.federation, kt.kind;

/* @name GetFrontierDetail */
SELECT
  f.id,
  f.federation,
  f.kind,
  f.key,
  f.discovered_at,
  f.last_fetched_at,
  f.fetch_status,
  f.process_status,
  f.error_count,
  f.next_fetch_at,
  f.last_process_error,
  f.meta,
  jr.url AS response_url,
  jr.fetched_at AS response_fetched_at,
  jr.http_status AS response_http_status,
  jr.error AS response_error,
  jr.content_hash AS response_content_hash,
  j.job_id,
  j.job_key,
  j.run_at AS job_run_at,
  j.attempts AS job_attempts,
  j.max_attempts AS job_max_attempts,
  j.job_error AS job_last_error,
  j.locked_at AS job_locked_at
FROM crawler.frontier f
LEFT JOIN LATERAL (
  SELECT jr.*
  FROM crawler.json_response jr
  WHERE jr.frontier_id = f.id
  ORDER BY jr.fetched_at DESC
  LIMIT 1
) jr ON true
LEFT JOIN LATERAL (
  SELECT j.*
  FROM crawler.frontier_fetch_job j
  WHERE j.frontier_id = f.id
  ORDER BY j.job_updated_at DESC
  LIMIT 1
) j ON true
WHERE (
    :id::bigint IS NOT NULL
    AND f.id = :id::bigint
  ) OR (
    :id::bigint IS NULL
    AND f.federation = :federation
    AND f.kind = :kind
    AND f.key = :key
  );

/* @name GetOutstandingJobCountForTask */
SELECT count(*)::int AS count
FROM graphile_worker.jobs
WHERE task_identifier = :task::text
  AND attempts < max_attempts;

/* @name GetFetchScheduleRules */
WITH job_stats AS (
  SELECT
    host,
    count(*) FILTER (WHERE state <> 'failed')::int AS outstanding,
    max(run_at) FILTER (WHERE state IN ('ready', 'delayed')) AS queue_tail_at
  FROM crawler.frontier_fetch_job
  WHERE host IS NOT NULL
  GROUP BY host
)
SELECT
  COALESCE(r.host, js.host) AS host,
  (extract(epoch from r.spacing) * 1000)::int as spacing,
  COALESCE(js.outstanding, 0)::int AS outstanding,
  GREATEST(js.queue_tail_at, now()) AS queue_tail_at
FROM crawler.rate_limit_rule r
FULL JOIN job_stats js USING (host);

/* @name GetPendingFetch */
WITH due AS (
  SELECT id, last_fetched_at
  FROM crawler.frontier_fetch_due(:allowRefetch::boolean)
), ranked AS (
  SELECT
    f.id, f.federation, f.kind, f.key, due.last_fetched_at,
    row_number() OVER (
      PARTITION BY f.federation, f.kind
      ORDER BY due.last_fetched_at NULLS FIRST
    ) AS rn
  FROM due
  JOIN crawler.frontier f ON f.id = due.id
)
SELECT id, federation, kind, key
FROM ranked
ORDER BY rn, last_fetched_at NULLS FIRST
LIMIT :capacity;

/* @name GetNextPendingProcess */
SELECT f.id, f.federation, f.kind, f.key, jr.url, jr.http_status, jr.error, jrc.content
FROM crawler.frontier f
JOIN LATERAL (
  SELECT jr.*
  FROM crawler.json_response jr
  WHERE jr.frontier_id = f.id
  ORDER BY jr.fetched_at DESC
  LIMIT 1
) jr ON true
JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
WHERE process_status = 'pending'
  AND fetch_status = 'ok'
ORDER BY last_fetched_at, discovered_at
FOR UPDATE SKIP LOCKED
LIMIT :limit;

/* @name ReserveRequest */
SELECT granted, allowed_at
  FROM crawler.reserve_request(:host::text);

/* @name MarkFrontierFetchError */
UPDATE crawler.frontier
SET last_fetched_at = now(),
    fetch_status    = 'error',
    error_count     = error_count + 1,
    next_fetch_at   = now() + least(
      interval '5 minutes',
      (power(2::numeric, error_count + 1) * 5) * interval '1 second'
    )
WHERE id = :id::bigint;

/* @name MarkFrontierTransient */
UPDATE crawler.frontier
SET last_fetched_at = now(),
    fetch_status    = 'transient',
    error_count     = error_count + 1,
    next_fetch_at   = now() + least(
      interval '30 minutes',
      (power(2::numeric, error_count + 1) * 5) * interval '1 second'
    )
WHERE id = :id::bigint;

/* @name MarkFrontierFetchSuccess */
UPDATE crawler.frontier
SET last_fetched_at = now(),
    fetch_status = :fetchStatus::crawler.fetch_status,
    process_status = CASE
      WHEN :fetchStatus::crawler.fetch_status = 'gone'::crawler.fetch_status
      THEN 'ok'::crawler.process_status
      ELSE 'pending'::crawler.process_status
    END,
    last_process_error = NULL,
    last_process_error_at = NULL,
    error_count = 0,
    next_fetch_at = now() + :revalidatePeriod::interval
WHERE id = :id::bigint;

/* @name MarkFrontiersProcessSuccess */
UPDATE crawler.frontier
SET process_status = 'ok',
    last_process_error = NULL,
    last_process_error_at = NULL
WHERE id = ANY(:ids::bigint[]);

/* @name MarkFrontierProcessError */
UPDATE crawler.frontier
SET process_status = 'error',
    last_process_error = :error,
    last_process_error_at = now()
WHERE id = :id::bigint;

/* @name RescheduleFrontier */
UPDATE crawler.frontier SET next_fetch_at = :nextRetryAt WHERE id = :id::bigint;

/* @name GetFrontierRefetchTarget */
SELECT id, federation, kind, key
FROM crawler.frontier
WHERE federation = :federation
  AND kind = :kind
  AND key = :key;

/* @name QueueFrontierRefetch */
SELECT id, federation, kind, key, job_id
FROM crawler.queue_frontier_refetch(ARRAY[:id::bigint]::bigint[]);

/* @name GetFailedFrontierRefetchTargets */
SELECT
  id,
  federation,
  kind,
  key,
  fetch_status,
  process_status,
  error_count,
  next_fetch_at,
  failed_at,
  url,
  http_status,
  error
FROM crawler.failed_frontier_refetch_candidates(
  :federation::text,
  :kind::text,
  :key::text,
  :httpStatuses::int[],
  :errorContains::text,
  :limit::int
)
ORDER BY failed_at DESC, id DESC;

/* @name QueueFailedFrontierRefetch */
WITH candidates AS (
  SELECT *
  FROM crawler.failed_frontier_refetch_candidates(
    :federation::text,
    :kind::text,
    :key::text,
    :httpStatuses::int[],
    :errorContains::text,
    :limit::int
  )
), queued AS (
  SELECT *
  FROM crawler.queue_frontier_refetch(ARRAY(SELECT id FROM candidates)::bigint[])
)
SELECT
  q.id,
  q.federation,
  q.kind,
  q.key,
  c.failed_at,
  c.http_status,
  c.error,
  q.job_id
FROM queued q
JOIN candidates c USING (id)
ORDER BY c.failed_at DESC, q.id DESC;

/* @name InsertResponse */
WITH payload AS (
  SELECT :content::text::jsonb AS content
), ins_cache AS (
  INSERT INTO crawler.json_response_cache (content)
    SELECT content
    FROM payload
    WHERE content IS NOT NULL
    ON CONFLICT (content_hash) DO NOTHING
)
INSERT INTO crawler.json_response (frontier_id, url, http_status, error, content_hash)
SELECT :id, :url, :httpStatus, :error,
       case when content IS NULL then NULL else encode(digest(content::text, 'sha256'), 'hex') end AS content_hash
FROM payload;

/* @name UpsertFrontier */
INSERT INTO crawler.frontier (federation, kind, key)
VALUES (:federation, :kind, :key)
ON CONFLICT (federation, kind, key) DO NOTHING;

/* @name UpsertFrontierKeys */
INSERT INTO crawler.frontier (federation, kind, key)
SELECT :federation, :kind, key
FROM unnest(:keys::text[]) as input(key)
ON CONFLICT DO NOTHING;
