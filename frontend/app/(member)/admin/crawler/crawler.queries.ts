/** Types generated for queries found in "app/(member)/admin/crawler/crawler.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

/** 'CrawlerOverview' parameters type */
export interface ICrawlerOverviewParams {
  allowRefetch?: boolean | null | void;
  federation?: string | null | void;
  kind?: string | null | void;
}

/** 'CrawlerOverview' return type */
export interface ICrawlerOverviewResult {
  done: number | null;
  federation: string;
  fetch_due: number | null;
  fetch_error: number | null;
  fetch_pending: number | null;
  fetch_transient: number | null;
  key_count: number | null;
  keys: string | null;
  kind: string;
  latest: Date | null;
  process_error: number | null;
  process_ready: number | null;
  total: number | null;
}

/** 'CrawlerOverview' query type */
export interface ICrawlerOverviewQuery {
  params: ICrawlerOverviewParams;
  result: ICrawlerOverviewResult;
}

const crawlerOverviewIR: any = {"usedParamSet":{"allowRefetch":true,"federation":true,"kind":true},"params":[{"name":"allowRefetch","required":false,"transform":{"type":"scalar"},"locs":[{"a":816,"b":828}]},{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":1000,"b":1010},{"a":1042,"b":1052}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":1064,"b":1068},{"a":1094,"b":1098}]}],"statement":"WITH frontier_status AS (\n  SELECT\n    federation,\n    kind,\n    count(*)::int AS total,\n    count(*) FILTER (WHERE key <> '')::int AS key_count,\n    count(*) FILTER (WHERE fetch_status = 'pending')::int AS fetch_pending,\n    count(*) FILTER (WHERE fetch_status = 'transient')::int AS fetch_transient,\n    count(*) FILTER (WHERE fetch_status = 'error')::int AS fetch_error,\n    count(*) FILTER (WHERE process_status = 'ok')::int AS done,\n    count(*) FILTER (WHERE process_status = 'error')::int AS process_error,\n    count(*) FILTER (WHERE process_status = 'pending' AND fetch_status = 'ok')::int\n      AS process_ready,\n    max(last_fetched_at) AS latest,\n    count(*) FILTER (\n      WHERE next_fetch_at <= now()\n        AND (\n          fetch_status IN ('pending', 'transient')\n          OR (\n            coalesce(:allowRefetch::boolean, false)\n            AND fetch_status = 'ok'\n            AND process_status = 'ok'\n          )\n        )\n    )::int AS fetch_due\n  FROM crawler.frontier\n  WHERE (:federation::text IS NULL OR federation = :federation)\n    AND (:kind::text IS NULL OR kind = :kind)\n  GROUP BY federation, kind\n)\nSELECT fs.*,\n  coalesce(key_samples.keys, '-') || CASE WHEN fs.key_count > 3 THEN ', ...' ELSE '' END AS keys\nFROM frontier_status fs\nLEFT JOIN LATERAL (\n  SELECT string_agg(sample.key, ', ' ORDER BY sample.key) AS keys\n  FROM (\n    SELECT key FROM crawler.frontier f\n    WHERE f.federation = fs.federation AND f.kind = fs.kind AND key <> ''\n    ORDER BY key LIMIT 3\n  ) sample\n) key_samples ON true\nORDER BY process_error DESC, fetch_error DESC, fetch_transient DESC,\n  process_ready DESC, fetch_due DESC, fetch_pending DESC, federation, kind"};

/**
 * Query generated from SQL:
 * ```
 * WITH frontier_status AS (
 *   SELECT
 *     federation,
 *     kind,
 *     count(*)::int AS total,
 *     count(*) FILTER (WHERE key <> '')::int AS key_count,
 *     count(*) FILTER (WHERE fetch_status = 'pending')::int AS fetch_pending,
 *     count(*) FILTER (WHERE fetch_status = 'transient')::int AS fetch_transient,
 *     count(*) FILTER (WHERE fetch_status = 'error')::int AS fetch_error,
 *     count(*) FILTER (WHERE process_status = 'ok')::int AS done,
 *     count(*) FILTER (WHERE process_status = 'error')::int AS process_error,
 *     count(*) FILTER (WHERE process_status = 'pending' AND fetch_status = 'ok')::int
 *       AS process_ready,
 *     max(last_fetched_at) AS latest,
 *     count(*) FILTER (
 *       WHERE next_fetch_at <= now()
 *         AND (
 *           fetch_status IN ('pending', 'transient')
 *           OR (
 *             coalesce(:allowRefetch::boolean, false)
 *             AND fetch_status = 'ok'
 *             AND process_status = 'ok'
 *           )
 *         )
 *     )::int AS fetch_due
 *   FROM crawler.frontier
 *   WHERE (:federation::text IS NULL OR federation = :federation)
 *     AND (:kind::text IS NULL OR kind = :kind)
 *   GROUP BY federation, kind
 * )
 * SELECT fs.*,
 *   coalesce(key_samples.keys, '-') || CASE WHEN fs.key_count > 3 THEN ', ...' ELSE '' END AS keys
 * FROM frontier_status fs
 * LEFT JOIN LATERAL (
 *   SELECT string_agg(sample.key, ', ' ORDER BY sample.key) AS keys
 *   FROM (
 *     SELECT key FROM crawler.frontier f
 *     WHERE f.federation = fs.federation AND f.kind = fs.kind AND key <> ''
 *     ORDER BY key LIMIT 3
 *   ) sample
 * ) key_samples ON true
 * ORDER BY process_error DESC, fetch_error DESC, fetch_transient DESC,
 *   process_ready DESC, fetch_due DESC, fetch_pending DESC, federation, kind
 * ```
 */
export const crawlerOverview = new PreparedQuery<ICrawlerOverviewParams,ICrawlerOverviewResult>(crawlerOverviewIR);


/** 'CrawlerFailures' parameters type */
export interface ICrawlerFailuresParams {
  federation?: string | null | void;
  kind?: string | null | void;
}

/** 'CrawlerFailures' return type */
export interface ICrawlerFailuresResult {
  count: number | null;
  error_fingerprint: string | null;
  failure: string | null;
  federation: string | null;
  http_status: number | null;
  kind: string | null;
  samples: string | null;
}

/** 'CrawlerFailures' query type */
export interface ICrawlerFailuresQuery {
  params: ICrawlerFailuresParams;
  result: ICrawlerFailuresResult;
}

const crawlerFailuresIR: any = {"usedParamSet":{"federation":true,"kind":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":241,"b":251},{"a":283,"b":293}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":305,"b":309},{"a":335,"b":339}]}],"statement":"WITH fingerprinted AS (\n  SELECT id, federation, kind, key, failed_at, http_status, failure,\n    left(regexp_replace(coalesce(nullif(error_text, ''), '-'), '\\s+', ' ', 'g'), 180) AS error_fingerprint\n  FROM crawler.frontier_failure\n  WHERE (:federation::text IS NULL OR federation = :federation)\n    AND (:kind::text IS NULL OR kind = :kind)\n), ranked AS (\n  SELECT\n    *,\n    row_number() OVER (\n      PARTITION BY federation, kind, failure, http_status, error_fingerprint\n      ORDER BY failed_at DESC, id DESC\n    ) AS sample_rank\n  FROM fingerprinted\n)\nSELECT\n  federation,\n  kind,\n  failure,\n  http_status,\n  error_fingerprint,\n  count(*)::int AS count,\n  string_agg(key, ', ' ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank <= 5) AS samples\nFROM ranked\nGROUP BY federation, kind, failure, http_status, error_fingerprint\nORDER BY count(*) DESC, max(failed_at) DESC"};

/**
 * Query generated from SQL:
 * ```
 * WITH fingerprinted AS (
 *   SELECT id, federation, kind, key, failed_at, http_status, failure,
 *     left(regexp_replace(coalesce(nullif(error_text, ''), '-'), '\s+', ' ', 'g'), 180) AS error_fingerprint
 *   FROM crawler.frontier_failure
 *   WHERE (:federation::text IS NULL OR federation = :federation)
 *     AND (:kind::text IS NULL OR kind = :kind)
 * ), ranked AS (
 *   SELECT
 *     *,
 *     row_number() OVER (
 *       PARTITION BY federation, kind, failure, http_status, error_fingerprint
 *       ORDER BY failed_at DESC, id DESC
 *     ) AS sample_rank
 *   FROM fingerprinted
 * )
 * SELECT
 *   federation,
 *   kind,
 *   failure,
 *   http_status,
 *   error_fingerprint,
 *   count(*)::int AS count,
 *   string_agg(key, ', ' ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank <= 5) AS samples
 * FROM ranked
 * GROUP BY federation, kind, failure, http_status, error_fingerprint
 * ORDER BY count(*) DESC, max(failed_at) DESC
 * ```
 */
export const crawlerFailures = new PreparedQuery<ICrawlerFailuresParams,ICrawlerFailuresResult>(crawlerFailuresIR);


