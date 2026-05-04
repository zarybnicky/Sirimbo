/* @name GetFrontierForUpdate */
SELECT id, federation, kind, key, fetch_status, error_count, meta
FROM crawler.frontier
WHERE id = :id::bigint
FOR UPDATE;

/* @name GetCrawlerFrontierStatus */
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
), frontier_status AS (
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
    count(*) FILTER (WHERE fetch_status = 'error')::int AS fetch_error,
    count(*) FILTER (WHERE process_status = 'ok')::int AS done,
    count(*) FILTER (WHERE process_status = 'error')::int AS process_error,
    count(*) FILTER (WHERE process_status = 'pending' AND fetch_status = 'ok')::int
      AS process_ready,
    min(last_fetched_at) FILTER (WHERE process_status = 'pending' AND fetch_status = 'ok')
      AS oldest_process_ready_at,
    (array_agg(id ORDER BY last_fetched_at NULLS FIRST, discovered_at, id)
      FILTER (WHERE process_status = 'pending' AND fetch_status = 'ok'))[1]
      AS sample_process_id,
    max(last_fetched_at) AS latest
  FROM frontiers f
  GROUP BY federation, kind
), due_fetch AS (
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
), fetch_jobs AS (
  SELECT
    federation,
    kind,
    count(*) FILTER (WHERE state IN ('ready', 'delayed'))::int AS queued_fetch,
    count(*) FILTER (WHERE state = 'locked')::int AS locked_fetch
  FROM crawler.frontier_fetch_job
  WHERE (:federation::text IS NULL OR federation = :federation)
    AND (:kind::text IS NULL OR kind = :kind)
    AND state IN ('ready', 'delayed', 'locked')
  GROUP BY federation, kind
)
SELECT
  fs.federation AS "federation!",
  fs.kind AS "kind!",
  fs.total AS "total!",
  fs.keys AS "keys!",
  fs.fetch_pending AS "fetch_pending!",
  fs.fetch_transient AS "fetch_transient!",
  fs.fetch_error AS "fetch_error!",
  fs.done AS "done!",
  fs.process_error AS "process_error!",
  fs.latest,
  coalesce(ds.fetch_due, 0)::int AS "fetch_due!",
  ds.oldest_due_at,
  ds.sample_due_id,
  ds.sample_due_key,
  fs.process_ready AS "process_ready!",
  fs.oldest_process_ready_at,
  fs.sample_process_id,
  coalesce(fj.queued_fetch, 0)::int AS "queued_fetch!",
  coalesce(fj.locked_fetch, 0)::int AS "locked_fetch!",
  (coalesce(fj.queued_fetch, 0) + coalesce(fj.locked_fetch, 0))::int
    AS "scheduled_fetch!",
  greatest(
    coalesce(ds.fetch_due, 0) - coalesce(fj.queued_fetch, 0) - coalesce(fj.locked_fetch, 0),
    0
  )::int AS "unscheduled_fetch!"
FROM frontier_status fs
LEFT JOIN due_status ds USING (federation, kind)
LEFT JOIN fetch_jobs fj USING (federation, kind)
ORDER BY fs.federation, fs.kind;

/* @name QueueCrawlerKick */
SELECT (
  graphile_worker.add_job(
    identifier => 'frontier_schedule',
    payload => '{}'::json,
    run_at => now(),
    job_key => 'frontier_schedule',
    job_key_mode => 'replace'
  )
).id AS "job_id!";

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
  id AS "id!",
  federation AS "federation!",
  kind AS "kind!",
  key AS "key!",
  fetch_status AS "fetch_status!",
  process_status AS "process_status!",
  error_count AS "error_count!",
  next_fetch_at,
  failed_at AS "failed_at!",
  url,
  http_status,
  response_error,
  process_error,
  failure AS "failure!",
  error_text AS "error_text!"
FROM crawler.frontier_failure
WHERE (:federation::text IS NULL OR federation = :federation)
  AND (:kind::text IS NULL OR kind = :kind)
  AND (:key::text IS NULL OR key = :key)
  AND (
    :excludeHttpStatuses::int[] IS NULL
    OR http_status IS NULL
    OR NOT (http_status = ANY(:excludeHttpStatuses::int[]))
  )
  AND (:httpStatuses::int[] IS NULL OR http_status = ANY(:httpStatuses))
  AND (
    :errorContains::text IS NULL
    OR error_text ILIKE '%' || :errorContains || '%'
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
  federation AS "federation!",
  kind AS "kind!",
  failure AS "failure!",
  http_status,
  error_fingerprint AS "error_fingerprint!",
  count(*)::int AS "count!",
  max(failed_at) AS "latest_failed_at!",
  (array_agg(id ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank = 1))[1]
    AS "sample_id!",
  (array_agg(key ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank = 1))[1]
    AS "sample_key!",
  string_agg(id::text || ':' || key, ', ' ORDER BY failed_at DESC, id DESC)
    FILTER (WHERE sample_rank <= 5) AS "samples!"
FROM ranked
GROUP BY federation, kind, failure, http_status, error_fingerprint
ORDER BY count(*) DESC, max(failed_at) DESC
LIMIT :limit;

/* @name GetCrawlerWorkerJobStatus */
WITH jobs AS (
  SELECT
    'frontier_fetch' AS task_identifier,
    count(*) FILTER (WHERE state = 'ready')::int AS ready,
    count(*) FILTER (WHERE state = 'delayed')::int AS delayed,
    count(*) FILTER (WHERE state = 'locked')::int AS locked,
    count(*) FILTER (WHERE state = 'failed')::int AS failed,
    max(job_updated_at) FILTER (WHERE state = 'failed') AS latest_failed_at
  FROM crawler.frontier_fetch_job
  UNION ALL
  SELECT
    task_identifier,
    count(*) FILTER (
      WHERE locked_at IS NULL AND run_at <= now() AND attempts < max_attempts
    )::int AS ready,
    count(*) FILTER (
      WHERE locked_at IS NULL AND run_at > now() AND attempts < max_attempts
    )::int AS delayed,
    count(*) FILTER (WHERE locked_at IS NOT NULL)::int AS locked,
    count(*) FILTER (WHERE attempts >= max_attempts)::int AS failed,
    max(updated_at) FILTER (WHERE attempts >= max_attempts) AS latest_failed_at
  FROM graphile_worker.jobs
  WHERE task_identifier IN ('frontier_schedule', 'frontier_process')
  GROUP BY task_identifier
)
SELECT
  task_identifier AS "task_identifier!",
  ready AS "ready!",
  delayed AS "delayed!",
  locked AS "locked!",
  failed AS "failed!",
  latest_failed_at
FROM jobs
ORDER BY task_identifier;

/* @name GetCrawlerFrontierJobs */
WITH jobs AS (
  SELECT
    j.*,
    (
      j.state = 'failed'
      OR j.job_error IS NOT NULL
      OR j.process_error IS NOT NULL
      OR j.fetch_status IN ('error', 'transient')
      OR j.process_status = 'error'
    ) AS needs_detail
  FROM crawler.frontier_fetch_job j
  WHERE (
    :state::text IS NULL
    OR (:state::text = 'failed' AND j.state = 'failed')
    OR (:state::text = 'active' AND j.state <> 'failed')
  )
    AND (:federation::text IS NULL OR j.federation = :federation)
    AND (:kind::text IS NULL OR j.kind = :kind)
)
SELECT
  j.job_id AS "job_id!",
  j.run_at AS "run_at!",
  j.locked_at,
  j.attempts AS "attempts!",
  j.max_attempts AS "max_attempts!",
  j.job_error,
  j.job_updated_at AS "job_updated_at!",
  j.frontier_id AS "frontier_id!",
  j.federation AS "federation!",
  j.kind AS "kind!",
  j.frontier_key AS "frontier_key!",
  j.fetch_status AS "fetch_status!",
  j.process_status AS "process_status!",
  response.http_status AS response_http_status,
  response.error AS response_error,
  CASE
    WHEN j.needs_detail OR response.error IS NOT NULL THEN response.content
  END AS response_content,
  j.process_error,
  j.state AS "state!"
FROM jobs j
LEFT JOIN LATERAL (
  SELECT jr.http_status, jr.error, jrc.content
  FROM crawler.json_response jr
  LEFT JOIN crawler.json_response_cache jrc ON jrc.content_hash = jr.content_hash
  WHERE jr.frontier_id = j.frontier_id
    AND (j.needs_detail OR jr.error IS NOT NULL)
  ORDER BY jr.fetched_at DESC
  LIMIT 1
) response ON true
ORDER BY
  CASE j.state
    WHEN 'failed' THEN 0
    WHEN 'locked' THEN 1
    WHEN 'ready' THEN 2
    ELSE 3
  END,
  j.job_updated_at DESC,
  j.run_at,
  j.job_id DESC
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
    max(run_at) FILTER (WHERE state IN ('ready', 'delayed')) AS queue_tail_at
  FROM crawler.frontier_fetch_job
  WHERE host IS NOT NULL
  GROUP BY host
)
SELECT
  COALESCE(r.host, fj.host) AS "host!",
  r.max_requests AS "max_requests?",
  r.per_interval::text AS "per_interval?",
  (extract(epoch from r.spacing) * 1000)::int AS "spacing?",
  r.next_available_at AS "next_available_at?",
  COALESCE(fj.queued, 0)::int AS "queued!",
  COALESCE(fj.ready, 0)::int AS "ready!",
  COALESCE(fj.delayed, 0)::int AS "delayed!",
  COALESCE(fj.locked, 0)::int AS "locked!",
  fj.next_run_at,
  fj.queue_tail_at
FROM crawler.rate_limit_rule r
FULL JOIN fetch_jobs fj USING (host)
ORDER BY host;

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
  jr.url AS "response_url?",
  jr.fetched_at AS "response_fetched_at?",
  jr.http_status AS "response_http_status?",
  jr.error AS "response_error?",
  jr.content_hash AS "response_content_hash?",
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
SELECT count(*)::int AS "count!"
FROM graphile_worker.jobs
WHERE task_identifier = :task::text
  AND attempts < max_attempts;

/* @name GetPendingFetch */
WITH ranked AS (
  SELECT
    id,
    federation,
    kind,
    key,
    last_fetched_at,
    row_number() OVER (
      PARTITION BY federation, kind
      ORDER BY last_fetched_at NULLS FIRST, id
    ) AS rn
  FROM crawler.frontier_fetch_due(:allowRefetch::boolean)
)
SELECT
  id AS "id!",
  federation AS "federation!",
  kind AS "kind!",
  key AS "key!"
FROM ranked
ORDER BY rn, last_fetched_at NULLS FIRST, id
LIMIT :capacity;

/* @name GetNextPendingProcess */
SELECT
  f.id AS "id!",
  f.federation AS "federation!",
  f.kind AS "kind!",
  f.key AS "key!",
  jr.url,
  jr.http_status,
  jr.error,
  jrc.content
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
SELECT id AS "id!", federation AS "federation!", kind AS "kind!", key AS "key!"
FROM crawler.frontier
WHERE federation = :federation
  AND kind = :kind
  AND key = :key;

/* @name QueueFrontierRefetch */
WITH frontier AS (
  UPDATE crawler.frontier f
  SET fetch_status = 'pending',
      process_status = 'pending',
      last_process_error = NULL,
      last_process_error_at = NULL,
      error_count = 0,
      next_fetch_at = now()
  WHERE f.id = ANY(:ids::bigint[])
  RETURNING f.id, f.federation, f.kind, f.key
)
SELECT id AS "id!", federation AS "federation!", kind AS "kind!", key AS "key!"
FROM frontier;

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

/* @name UpsertFrontiers */
INSERT INTO crawler.frontier (federation, kind, key)
SELECT federation, kind, key
FROM unnest(
  :federations::text[],
  :kinds::text[],
  :keys::text[]
) AS input(federation, kind, key)
ON CONFLICT (federation, kind, key) DO NOTHING;

/* @name UpsertFrontierKeys */
INSERT INTO crawler.frontier (federation, kind, key)
SELECT :federation, :kind, key
FROM unnest(:keys::text[]) as input(key)
ON CONFLICT DO NOTHING;
