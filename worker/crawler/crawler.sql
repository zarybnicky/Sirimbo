/* @name GetFrontierForUpdate */
SELECT id, federation, kind, key, fetch_status, error_count, meta
FROM crawler.frontier
WHERE id = :id::bigint
FOR UPDATE SKIP LOCKED;

/* @name GetLatestFrontierJsonResponse */
SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content
FROM crawler.frontier f
JOIN crawler.json_response jr on f.id = jr.frontier_id
JOIN crawler.json_response_cache jrc on jr.content_hash = jrc.content_hash
WHERE f.federation = :federation AND f.kind = :kind;

/* @name GetFrontierJsonResponseForUpdate */
SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content
FROM crawler.frontier f
JOIN crawler.json_response jr on f.id = jr.frontier_id
JOIN crawler.json_response_cache jrc on jr.content_hash = jrc.content_hash
WHERE f.id = :id::bigint
FOR UPDATE SKIP LOCKED;

/* @name GetFrontierHtmlResponseForUpdate */
SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content
FROM crawler.frontier f
JOIN crawler.html_response jr on f.id = jr.frontier_id
JOIN crawler.html_response_cache jrc on jr.content_hash = jrc.content_hash
WHERE f.id = :id::bigint
FOR UPDATE SKIP LOCKED;

/* @name GetJobCountForTask */
SELECT count(*)::int AS count
FROM graphile_worker.jobs
WHERE task_identifier = :task::text;

/* @name GetPendingFetch */
SELECT id
FROM crawler.frontier
WHERE (next_fetch_at IS NULL OR next_fetch_at <= now())
  AND (fetch_status = 'pending'
         OR (fetch_status = 'ok' AND process_status = 'ok'))
ORDER BY discovered_at
LIMIT :limit;

/* @name GetPendingProcess */
SELECT id
FROM crawler.frontier
WHERE process_status = 'pending'
  AND fetch_status IN ('ok', 'gone')
ORDER BY last_fetched_at NULLS FIRST, discovered_at
LIMIT :limit;

/* @name ReserveRequest */
SELECT granted, allowed_at
FROM crawler.reserve_request(:host::text, :prefixes::text[]);

/* @name MarkFrontierFetchError */
UPDATE crawler.frontier
SET last_fetched_at = now(),
    fetch_status    = 'error',
    error_count     = error_count + 1,
    next_fetch_at   = now() + least(interval '5 minutes', (2 ^ (error_count + 1)) * interval '5 second')
WHERE id = :id::bigint;

/* @name MarkFrontierFetchSuccess */
UPDATE crawler.frontier
SET last_fetched_at = now(),
    fetch_status = :fetchStatus,
    process_status = 'pending',
    error_count = 0,
    next_fetch_at = now() + :revalidatePeriod::interval
WHERE id = :id::bigint;

/* @name MarkFrontierProcessSuccess */
UPDATE crawler.frontier
SET process_status = 'ok'
WHERE id = :id::bigint;

/* @name MarkFrontierProcessError */
UPDATE crawler.frontier
SET process_status = 'error'
WHERE id = :id::bigint;

/* @name RescheduleFrontier */
UPDATE crawler.frontier
SET next_fetch_at = :nextRetryAt
WHERE id = :id::bigint;

/* @name InsertHtmlResponse */
WITH payload AS (
  SELECT :content AS content
), ins_cache AS (
  INSERT INTO crawler.html_response_cache (content)
    SELECT content
    FROM payload
    WHERE content IS NOT NULL
    ON CONFLICT (content_hash) DO NOTHING
)
INSERT INTO crawler.html_response (frontier_id, url, http_status, error, content_hash)
SELECT :id, :url,:httpStatus, :error,
  case when content IS NULL then NULL else encode(sha256(content::TEXT::BYTEA), 'hex') end AS content_hash
FROM payload;

/* @name InsertJsonResponse */
WITH payload AS (
  SELECT :content::jsonb AS content
), ins_cache AS (
  INSERT INTO crawler.json_response_cache (content)
    SELECT content
    FROM payload
    WHERE content IS NOT NULL
    ON CONFLICT (content_hash) DO NOTHING
)
INSERT INTO crawler.json_response (frontier_id, url, http_status, error, content_hash)
SELECT :id, :url, :httpStatus, :error,
       case when content IS NULL then NULL else encode(sha256(content::TEXT::BYTEA), 'hex') end AS content_hash
FROM payload;

/* @name InsertDiscoveredCstsMember */
with frontier as (
  INSERT INTO crawler.frontier (federation, kind, key)
    VALUES ('csts','member', :id)
    ON CONFLICT (federation, kind, key) DO NOTHING
    RETURNING key
)
UPDATE crawler.incremental_ranges
SET last_known = GREATEST(last_known, (SELECT key::bigint FROM frontier))
FROM frontier
WHERE federation = 'csts' AND kind = 'member_id';
