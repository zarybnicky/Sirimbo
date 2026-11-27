/* @name GetFrontierForUpdate */
SELECT id, federation, kind, key, fetch_status, error_count, meta, next_fetch_at
FROM crawler.frontier
WHERE id = :id::bigint
FOR UPDATE;

/* @name GetFrontierResponses */
SELECT
  f.id AS "id!",
  f.federation AS "federation!",
  f.kind AS "kind!",
  f.key AS "key!",
  f.reference_date::text AS "reference_date?",
  jr.url AS "url!",
  jr.http_status,
  coalesce(jr.last_observed_at, jr.fetched_at) AS "fetched_at!",
  jrc.content AS "content!"
FROM crawler.frontier f
JOIN crawler.json_response jr ON jr.id = f.last_successful_response_id
JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
WHERE (:federation::text IS NULL OR f.federation = :federation)
  AND (:kind::text IS NULL OR f.kind = :kind)
  AND (:key::text IS NULL OR f.key = :key)
  AND (:id::bigint IS NULL OR f.id = :id)
  AND (:allowErrors OR f.fetch_status = 'ok')
ORDER BY f.discovered_at, f.last_fetched_at, f.id
LIMIT :limit;

/* @name GetScheduleStatus */
WITH fetch_jobs AS (
  SELECT
    host,
    count(*) FILTER (WHERE state = 'queued')::int AS queued,
    count(*) FILTER (WHERE state = 'locked')::int AS locked,
    array_agg(run_at ORDER BY run_at) FILTER (WHERE state = 'queued') AS slots
  FROM crawler.frontier_fetch_job
  WHERE host IS NOT NULL
  GROUP BY host
)
SELECT
  COALESCE(r.host, fj.host) AS "host!",
  (extract(epoch from r.spacing) * 1000)::int AS "spacing?",
  r.next_available_at AS "next_available_at?",
  COALESCE(fj.queued, 0)::int AS "queued!",
  COALESCE(fj.locked, 0)::int AS "locked!",
  COALESCE(fj.slots, '{}'::timestamptz[]) AS "slots!"
FROM crawler.rate_limit_rule r
FULL JOIN fetch_jobs fj USING (host)
ORDER BY host;

/* @name QueuePendingProcess */
SELECT (
  graphile_worker.add_job(
    identifier => 'frontier_process',
    job_key => 'frontier_process',
    job_key_mode => 'preserve_run_at'
  )
).id AS "job_id!"
WHERE NOT EXISTS (
  SELECT FROM graphile_worker.jobs
  WHERE task_identifier = 'frontier_process' AND attempts < max_attempts
)
AND EXISTS (
  SELECT FROM crawler.frontier
  WHERE fetch_status = 'ok' AND process_status = 'pending'
);

/* @name GetPendingFetch */
WITH allowed_loaders AS (
  SELECT federation, kind
  FROM unnest(
    :loaderFederations::text[],
    :loaderKinds::text[]
  ) AS input(federation, kind)
), active_fetches AS MATERIALIZED (
  SELECT frontier_id
  FROM crawler.frontier_fetch_job
  WHERE state IN ('queued', 'locked')
), ranked AS (
  SELECT
    candidate.*,
    row_number() OVER (
      PARTITION BY candidate.federation, candidate.kind
      ORDER BY candidate.next_fetch_at, candidate.last_fetched_at NULLS FIRST, candidate.id
    ) AS rn
  FROM allowed_loaders loader
  CROSS JOIN LATERAL (
    SELECT
      f.id,
      f.federation,
      f.kind,
      f.key,
      f.next_fetch_at,
      f.last_fetched_at
    FROM crawler.frontier f
    WHERE f.federation = loader.federation
      AND f.kind = loader.kind
      AND f.next_fetch_at <= now()
      AND (
        f.fetch_status IN ('pending', 'transient')
        OR (
          coalesce(:allowRefetch::boolean, false)
          AND f.fetch_status = 'ok'
          AND f.process_status = 'ok'
        )
      )
      AND NOT EXISTS (
        SELECT 1
        FROM active_fetches active
        WHERE active.frontier_id = f.id
      )
    ORDER BY f.next_fetch_at, f.last_fetched_at NULLS FIRST, f.id
    LIMIT :capacity
  ) candidate
)
SELECT
  id AS "id!",
  federation AS "federation!",
  kind AS "kind!",
  key AS "key!"
FROM ranked
ORDER BY rn, random()
LIMIT :capacity;

/* @name GetNextPendingProcess */
WITH pending AS MATERIALIZED (
  SELECT f.id, f.federation, f.kind, f.key, f.reference_date::text, jr.url, jr.http_status, jr.error, jrc.content,
    c.id AS competition_id
  FROM crawler.frontier f
  JOIN crawler.json_response jr ON jr.id = f.last_successful_response_id
  JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
  LEFT JOIN federated.competition c
    ON c.federation = 'wdsf' AND c.external_id = CASE
      WHEN f.federation = 'wdsf' AND f.kind = 'participant'
      THEN jrc.content->>'competitionId'
    END
  WHERE process_status = 'pending' AND fetch_status = 'ok'
  ORDER BY discovered_at, last_fetched_at
  FOR UPDATE OF f SKIP LOCKED
  LIMIT :limit
), owned_competitions AS MATERIALIZED (
  SELECT c.id FROM federated.competition c
  WHERE c.id IN (SELECT competition_id FROM pending)
  ORDER BY c.id
  FOR NO KEY UPDATE SKIP LOCKED
)
SELECT id AS "id!", federation AS "federation!", kind AS "kind!", key AS "key!",
  reference_date, url, http_status, error, content,
  competition_id IS NULL OR competition_id IN (SELECT id FROM owned_competitions) AS "canProcess!"
FROM pending;

/* @name GetPendingProcessById */
SELECT
  f.federation AS "federation!",
  f.kind AS "kind!",
  f.key AS "key!",
  f.reference_date::text AS "reference_date?",
  jrc.content
FROM crawler.frontier f
JOIN crawler.json_response jr ON jr.id = f.last_successful_response_id
JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
WHERE f.id = :id::bigint
  AND process_status = 'pending'
  AND fetch_status = 'ok'
FOR UPDATE OF f SKIP LOCKED;

/* @name ReserveRequest */
SELECT granted, allowed_at
  FROM crawler.reserve_request(:host::text);

/* @name MarkFrontierFetchError */
UPDATE crawler.frontier
SET last_fetched_at = clock_timestamp(),
    fetch_status    = 'error',
    error_count     = error_count + 1,
    next_fetch_at   = now() + least(
      300::numeric,
      power(2::numeric, least(error_count, 9) + 1) * 5
    ) * interval '1 second'
WHERE id = :id::bigint;

/* @name MarkFrontierTransient */
UPDATE crawler.frontier
SET last_fetched_at = clock_timestamp(),
    fetch_status    = 'transient',
    error_count     = error_count + 1,
    next_fetch_at   = now() + least(
      1800::numeric,
      power(2::numeric, least(error_count, 9) + 1) * 5
    ) * interval '1 second'
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
    next_fetch_at = crawler.next_fetch_at(federation, kind, reference_date, now())
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

/* @name ReprocessWdsfParticipantIndex */
UPDATE crawler.frontier
SET process_status = 'pending'
WHERE federation = 'wdsf' AND kind = 'participantIndex' AND key = :competitionExternalId
  AND fetch_status = 'ok' AND process_status <> 'pending'
  AND (:referenceDate::date IS NULL OR reference_date IS DISTINCT FROM :referenceDate::date);

/* @name RescheduleFrontier */
UPDATE crawler.frontier SET next_fetch_at = :nextRetryAt WHERE id = :id::bigint;

/* @name InsertResponse */
SELECT crawler.store_response(
  :id::bigint, :url::text, :httpStatus::integer, :error::text,
  :content::text::jsonb, :fetchStatus::crawler.fetch_status
) AS "id!";

/* @name UpsertFrontiers */
WITH input AS (
  SELECT federation, kind, key, nullif(reference_date, '')::date AS reference_date
  FROM unnest(
    :federations::text[],
    :kinds::text[],
    :keys::text[],
    :referenceDates::text[]
  ) AS rows(federation, kind, key, reference_date)
)
INSERT INTO crawler.frontier AS f (federation, kind, key, reference_date)
SELECT federation, kind, key, reference_date
FROM input
WHERE NOT EXISTS (
  SELECT 1 FROM crawler.frontier existing
  WHERE (existing.federation, existing.kind, existing.key) = (input.federation, input.kind, input.key)
    AND existing.process_status <> 'error' AND existing.fetch_status <> 'gone'
    AND (input.reference_date IS NULL OR existing.reference_date IS NOT DISTINCT FROM input.reference_date)
)
ORDER BY federation, kind, key
ON CONFLICT (federation, kind, key) DO UPDATE
SET reference_date = coalesce(EXCLUDED.reference_date, f.reference_date),
    process_status = CASE WHEN f.process_status = 'error' OR f.fetch_status = 'gone'
      THEN 'pending'::crawler.process_status ELSE f.process_status END,
    fetch_status = CASE f.fetch_status WHEN 'gone' THEN 'pending' ELSE f.fetch_status END,
    next_fetch_at = CASE f.fetch_status WHEN 'gone' THEN now() ELSE f.next_fetch_at END
WHERE f.process_status = 'error'
   OR f.fetch_status = 'gone'
   OR (EXCLUDED.reference_date IS NOT NULL AND f.reference_date IS DISTINCT FROM EXCLUDED.reference_date);

/* @name SetFrontierReferenceDate */
UPDATE crawler.frontier SET reference_date = :referenceDate!::date
WHERE (federation, kind, key) = (:federation!, :kind!, :key!)
  AND reference_date IS DISTINCT FROM :referenceDate!::date;
