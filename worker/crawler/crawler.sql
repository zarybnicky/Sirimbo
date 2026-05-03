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
    count(*)::int AS responses,
    max(r.fetched_at) AS latest_response
  FROM frontiers f
  JOIN crawler.json_response r ON r.frontier_id = f.id
  GROUP BY f.federation, f.kind
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
    count(*) FILTER (WHERE next_fetch_at IS NULL OR next_fetch_at <= now())::int AS fetch_due,
    max(last_fetched_at) AS latest_fetch
  FROM frontiers f
  GROUP BY federation, kind
)
SELECT
  s.*,
  coalesce(rc.responses, 0)::int AS responses,
  coalesce(s.latest_fetch, rc.latest_response) AS latest
FROM status s
LEFT JOIN response_counts rc ON rc.federation = s.federation AND rc.kind = s.kind
ORDER BY s.federation, s.kind;

/* @name GetLatestFrontierResponses */
SELECT f.id, jr.url, jr.http_status, jrc.content
FROM crawler.frontier f
JOIN LATERAL (
  SELECT jr.*
  FROM crawler.json_response jr
  WHERE jr.frontier_id = f.id
  ORDER BY jr.fetched_at DESC
  LIMIT 1
  ) jr ON true
JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
WHERE f.federation = :federation AND f.kind = :kind AND f.fetch_status in ('ok', 'pending');

/* @name GetJobCountForTask */
SELECT count(*)::int AS count
FROM graphile_worker.jobs
WHERE task_identifier = :task::text;

/* @name GetFetchScheduleRules */
WITH job_stats AS (
  SELECT
    split_part(key, ':', 2) AS host,
    count(*)                AS queued,
    max(run_at)             AS last_run_at
  FROM graphile_worker.jobs
  WHERE task_identifier = 'frontier_fetch'
    AND key LIKE 'fetch:%'
    AND run_at >= now()
    AND locked_at IS NULL
  GROUP BY split_part(key, ':', 2)
)
SELECT
  COALESCE(r.host, js.host) AS host,
  (extract(epoch from r.spacing) * 1000)::int as spacing,
  COALESCE(js.queued, 0)::int AS queued,
  GREATEST(js.last_run_at, now()) AS last_run_at
FROM crawler.rate_limit_rule r
FULL JOIN job_stats js USING (host);

/* @name GetPendingFetch */
WITH eligible AS (
  SELECT id, federation, kind, key, last_fetched_at
  FROM crawler.frontier
  WHERE (next_fetch_at IS NULL OR next_fetch_at <= now())
    AND (fetch_status IN ('pending', 'transient')
       OR (:allowRefetch AND fetch_status = 'ok' AND process_status = 'ok'))
), ranked AS (
  SELECT
    id, federation, kind, key, last_fetched_at,
    row_number() OVER (
      PARTITION BY federation, kind
      ORDER BY last_fetched_at NULLS FIRST
    ) AS rn
  FROM eligible
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
  AND fetch_status IN ('ok', 'gone')
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
    fetch_status = :fetchStatus,
    process_status = 'pending',
    error_count = 0,
    next_fetch_at = now() + :revalidatePeriod::interval
WHERE id = :id::bigint;

/* @name MarkFrontiersProcessSuccess */
UPDATE crawler.frontier
SET process_status = 'ok'
WHERE id = ANY(:ids::bigint[]);

/* @name MarkFrontierProcessError */
UPDATE crawler.frontier SET process_status = 'error' WHERE id = :id::bigint;

/* @name RescheduleFrontier */
UPDATE crawler.frontier SET next_fetch_at = :nextRetryAt WHERE id = :id::bigint;

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
