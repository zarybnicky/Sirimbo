/* @name CrawlerOverview */
WITH frontier_status AS (
  SELECT
    federation,
    kind,
    count(*)::int AS total,
    count(*) FILTER (WHERE fetch_status = 'pending')::int AS fetch_pending,
    count(*) FILTER (WHERE fetch_status = 'transient')::int AS fetch_transient,
    count(*) FILTER (WHERE fetch_status = 'error')::int AS fetch_error,
    count(*) FILTER (WHERE process_status = 'ok')::int AS done,
    count(*) FILTER (WHERE process_status = 'error')::int AS process_error,
    count(*) FILTER (WHERE process_status = 'pending' AND fetch_status = 'ok')::int
      AS process_ready,
    max(last_fetched_at) AS latest,
    count(*) FILTER (
      WHERE next_fetch_at <= now()
        AND (
          fetch_status IN ('pending', 'transient')
          OR (
            coalesce(:allowRefetch::boolean, false)
            AND fetch_status = 'ok'
            AND process_status = 'ok'
          )
        )
    )::int AS fetch_due
  FROM crawler.frontier
  WHERE (:federation::text IS NULL OR federation = :federation)
    AND (:kind::text IS NULL OR kind = :kind)
  GROUP BY federation, kind
)
SELECT fs.*,
  key_samples.keys, key_samples.ids
FROM frontier_status fs
LEFT JOIN LATERAL (
  SELECT array_agg(sample.key ORDER BY sample.key) AS keys,
    array_agg(sample.id ORDER BY sample.key) AS ids
  FROM (
    SELECT id, key FROM crawler.frontier f
    WHERE f.federation = fs.federation AND f.kind = fs.kind
    ORDER BY key LIMIT 3
  ) sample
) key_samples ON true
ORDER BY process_error DESC, fetch_error DESC, fetch_transient DESC,
  process_ready DESC, fetch_due DESC, fetch_pending DESC, federation, kind;

/* @name CrawlerFailures */
WITH fingerprinted AS (
  SELECT id, federation, kind, key, failed_at, http_status, failure,
    left(regexp_replace(coalesce(nullif(error_text, ''), '-'), '\s+', ' ', 'g'), 180) AS error_fingerprint
  FROM crawler.frontier_failure
  WHERE (:federation::text IS NULL OR federation = :federation)
    AND (:kind::text IS NULL OR kind = :kind)
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
  array_agg(key ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank <= 5) AS samples,
  array_agg(id ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank <= 5) AS sample_ids
FROM ranked
GROUP BY federation, kind, failure, http_status, error_fingerprint
ORDER BY count(*) DESC, max(failed_at) DESC;
