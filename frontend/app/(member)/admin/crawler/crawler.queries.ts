/** Types generated for queries found in "app/(member)/admin/crawler/crawler.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type fetch_status = 'error' | 'gone' | 'ok' | 'pending' | 'transient';

export type process_status = 'error' | 'ok' | 'pending';

export type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

export type NumberOrString = number | string;

export type stringArray = (string)[];

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
  ids: stringArray | null;
  keys: stringArray | null;
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

const crawlerOverviewIR: any = {"usedParamSet":{"allowRefetch":true,"federation":true,"kind":true},"params":[{"name":"allowRefetch","required":false,"transform":{"type":"scalar"},"locs":[{"a":759,"b":771}]},{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":943,"b":953},{"a":985,"b":995}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":1007,"b":1011},{"a":1037,"b":1041}]}],"statement":"WITH frontier_status AS (\n  SELECT\n    federation,\n    kind,\n    count(*)::int AS total,\n    count(*) FILTER (WHERE fetch_status = 'pending')::int AS fetch_pending,\n    count(*) FILTER (WHERE fetch_status = 'transient')::int AS fetch_transient,\n    count(*) FILTER (WHERE fetch_status = 'error')::int AS fetch_error,\n    count(*) FILTER (WHERE process_status = 'ok')::int AS done,\n    count(*) FILTER (WHERE process_status = 'error')::int AS process_error,\n    count(*) FILTER (WHERE process_status = 'pending' AND fetch_status = 'ok')::int\n      AS process_ready,\n    max(last_fetched_at) AS latest,\n    count(*) FILTER (\n      WHERE next_fetch_at <= now()\n        AND (\n          fetch_status IN ('pending', 'transient')\n          OR (\n            coalesce(:allowRefetch::boolean, false)\n            AND fetch_status = 'ok'\n            AND process_status = 'ok'\n          )\n        )\n    )::int AS fetch_due\n  FROM crawler.frontier\n  WHERE (:federation::text IS NULL OR federation = :federation)\n    AND (:kind::text IS NULL OR kind = :kind)\n  GROUP BY federation, kind\n)\nSELECT fs.*,\n  key_samples.keys, key_samples.ids\nFROM frontier_status fs\nLEFT JOIN LATERAL (\n  SELECT array_agg(sample.key ORDER BY sample.key) AS keys,\n    array_agg(sample.id ORDER BY sample.key) AS ids\n  FROM (\n    SELECT id, key FROM crawler.frontier f\n    WHERE f.federation = fs.federation AND f.kind = fs.kind\n    ORDER BY key LIMIT 3\n  ) sample\n) key_samples ON true\nORDER BY process_error DESC, fetch_error DESC, fetch_transient DESC,\n  process_ready DESC, fetch_due DESC, fetch_pending DESC, federation, kind"};

/**
 * Query generated from SQL:
 * ```
 * WITH frontier_status AS (
 *   SELECT
 *     federation,
 *     kind,
 *     count(*)::int AS total,
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
 *   key_samples.keys, key_samples.ids
 * FROM frontier_status fs
 * LEFT JOIN LATERAL (
 *   SELECT array_agg(sample.key ORDER BY sample.key) AS keys,
 *     array_agg(sample.id ORDER BY sample.key) AS ids
 *   FROM (
 *     SELECT id, key FROM crawler.frontier f
 *     WHERE f.federation = fs.federation AND f.kind = fs.kind
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
  sample_ids: stringArray | null;
  samples: stringArray | null;
}

/** 'CrawlerFailures' query type */
export interface ICrawlerFailuresQuery {
  params: ICrawlerFailuresParams;
  result: ICrawlerFailuresResult;
}

const crawlerFailuresIR: any = {"usedParamSet":{"federation":true,"kind":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":241,"b":251},{"a":283,"b":293}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":305,"b":309},{"a":335,"b":339}]}],"statement":"WITH fingerprinted AS (\n  SELECT id, federation, kind, key, failed_at, http_status, failure,\n    left(regexp_replace(coalesce(nullif(error_text, ''), '-'), '\\s+', ' ', 'g'), 180) AS error_fingerprint\n  FROM crawler.frontier_failure\n  WHERE (:federation::text IS NULL OR federation = :federation)\n    AND (:kind::text IS NULL OR kind = :kind)\n), ranked AS (\n  SELECT\n    *,\n    row_number() OVER (\n      PARTITION BY federation, kind, failure, http_status, error_fingerprint\n      ORDER BY failed_at DESC, id DESC\n    ) AS sample_rank\n  FROM fingerprinted\n)\nSELECT\n  federation,\n  kind,\n  failure,\n  http_status,\n  error_fingerprint,\n  count(*)::int AS count,\n  array_agg(key ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank <= 5) AS samples,\n  array_agg(id ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank <= 5) AS sample_ids\nFROM ranked\nGROUP BY federation, kind, failure, http_status, error_fingerprint\nORDER BY count(*) DESC, max(failed_at) DESC"};

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
 *   array_agg(key ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank <= 5) AS samples,
 *   array_agg(id ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank <= 5) AS sample_ids
 * FROM ranked
 * GROUP BY federation, kind, failure, http_status, error_fingerprint
 * ORDER BY count(*) DESC, max(failed_at) DESC
 * ```
 */
export const crawlerFailures = new PreparedQuery<ICrawlerFailuresParams,ICrawlerFailuresResult>(crawlerFailuresIR);


/** 'CrawlerFrontier' parameters type */
export interface ICrawlerFrontierParams {
  id: NumberOrString;
}

/** 'CrawlerFrontier' return type */
export interface ICrawlerFrontierResult {
  discovered_at: Date;
  error_count: number;
  federation: string;
  fetch_status: fetch_status;
  id: string;
  key: string;
  kind: string;
  last_fetched_at: Date | null;
  last_process_error: string | null;
  last_process_error_at: Date | null;
  last_response_id: string | null;
  last_successful_response_id: string | null;
  next_fetch_at: Date;
  process_status: process_status;
}

/** 'CrawlerFrontier' query type */
export interface ICrawlerFrontierQuery {
  params: ICrawlerFrontierParams;
  result: ICrawlerFrontierResult;
}

const crawlerFrontierIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":250,"b":253}]}],"statement":"SELECT id, federation, kind, key, discovered_at, fetch_status, process_status,\n  last_fetched_at, next_fetch_at, error_count, last_process_error,\n  last_process_error_at, last_response_id, last_successful_response_id\nFROM crawler.frontier\nWHERE id = :id!"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id, federation, kind, key, discovered_at, fetch_status, process_status,
 *   last_fetched_at, next_fetch_at, error_count, last_process_error,
 *   last_process_error_at, last_response_id, last_successful_response_id
 * FROM crawler.frontier
 * WHERE id = :id!
 * ```
 */
export const crawlerFrontier = new PreparedQuery<ICrawlerFrontierParams,ICrawlerFrontierResult>(crawlerFrontierIR);


/** 'CrawlerResponses' parameters type */
export interface ICrawlerResponsesParams {
  id: NumberOrString;
  offset: NumberOrString;
}

/** 'CrawlerResponses' return type */
export interface ICrawlerResponsesResult {
  error: string | null;
  fetched_at: Date;
  has_content: boolean | null;
  http_status: number | null;
  id: string;
  last_observed_at: Date | null;
  observation_count: string;
  url: string;
  valid_from: Date | null;
  valid_until: Date | null;
}

/** 'CrawlerResponses' query type */
export interface ICrawlerResponsesQuery {
  params: ICrawlerResponsesParams;
  result: ICrawlerResponsesResult;
}

const crawlerResponsesIR: any = {"usedParamSet":{"id":true,"offset":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":243,"b":246}]},{"name":"offset","required":true,"transform":{"type":"scalar"},"locs":[{"a":298,"b":305}]}],"statement":"SELECT id, url, http_status, error, fetched_at, lower(sys_period) AS valid_from,\n  upper(sys_period) AS valid_until,\n  last_observed_at, observation_count, content_hash IS NOT NULL AS has_content\nFROM crawler.json_response\nWHERE frontier_id = :id!\nORDER BY fetched_at DESC, id DESC\nLIMIT 50 OFFSET :offset!"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id, url, http_status, error, fetched_at, lower(sys_period) AS valid_from,
 *   upper(sys_period) AS valid_until,
 *   last_observed_at, observation_count, content_hash IS NOT NULL AS has_content
 * FROM crawler.json_response
 * WHERE frontier_id = :id!
 * ORDER BY fetched_at DESC, id DESC
 * LIMIT 50 OFFSET :offset!
 * ```
 */
export const crawlerResponses = new PreparedQuery<ICrawlerResponsesParams,ICrawlerResponsesResult>(crawlerResponsesIR);


/** 'CrawlerResponseCount' parameters type */
export interface ICrawlerResponseCountParams {
  id: NumberOrString;
}

/** 'CrawlerResponseCount' return type */
export interface ICrawlerResponseCountResult {
  total: number | null;
}

/** 'CrawlerResponseCount' query type */
export interface ICrawlerResponseCountQuery {
  params: ICrawlerResponseCountParams;
  result: ICrawlerResponseCountResult;
}

const crawlerResponseCountIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":77,"b":80}]}],"statement":"SELECT count(*)::int AS total FROM crawler.json_response WHERE frontier_id = :id!"};

/**
 * Query generated from SQL:
 * ```
 * SELECT count(*)::int AS total FROM crawler.json_response WHERE frontier_id = :id!
 * ```
 */
export const crawlerResponseCount = new PreparedQuery<ICrawlerResponseCountParams,ICrawlerResponseCountResult>(crawlerResponseCountIR);


/** 'CrawlerResponseContent' parameters type */
export interface ICrawlerResponseContentParams {
  frontierId: NumberOrString;
  responseId: NumberOrString;
}

/** 'CrawlerResponseContent' return type */
export interface ICrawlerResponseContentResult {
  content: Json;
  error: string | null;
  federation: string;
  http_status: number | null;
  id: string;
  key: string;
  kind: string;
  url: string;
}

/** 'CrawlerResponseContent' query type */
export interface ICrawlerResponseContentQuery {
  params: ICrawlerResponseContentParams;
  result: ICrawlerResponseContentResult;
}

const crawlerResponseContentIR: any = {"usedParamSet":{"frontierId":true,"responseId":true},"params":[{"name":"frontierId","required":true,"transform":{"type":"scalar"},"locs":[{"a":257,"b":268}]},{"name":"responseId","required":true,"transform":{"type":"scalar"},"locs":[{"a":281,"b":292}]}],"statement":"SELECT r.id, r.url, r.http_status, r.error, c.content, f.federation, f.kind, f.key\nFROM crawler.json_response r\nJOIN crawler.frontier f ON f.id = r.frontier_id\nLEFT JOIN crawler.json_response_cache c ON c.content_hash = r.content_hash\nWHERE r.frontier_id = :frontierId! AND r.id = :responseId!"};

/**
 * Query generated from SQL:
 * ```
 * SELECT r.id, r.url, r.http_status, r.error, c.content, f.federation, f.kind, f.key
 * FROM crawler.json_response r
 * JOIN crawler.frontier f ON f.id = r.frontier_id
 * LEFT JOIN crawler.json_response_cache c ON c.content_hash = r.content_hash
 * WHERE r.frontier_id = :frontierId! AND r.id = :responseId!
 * ```
 */
export const crawlerResponseContent = new PreparedQuery<ICrawlerResponseContentParams,ICrawlerResponseContentResult>(crawlerResponseContentIR);


/** 'CrawlerFrontiers' parameters type */
export interface ICrawlerFrontiersParams {
  federation: string;
  key: string;
  kind: string;
  offset: NumberOrString;
}

/** 'CrawlerFrontiers' return type */
export interface ICrawlerFrontiersResult {
  error_count: number;
  fetch_status: fetch_status;
  id: string;
  key: string;
  last_fetched_at: Date | null;
  next_fetch_at: Date;
  process_status: process_status;
}

/** 'CrawlerFrontiers' query type */
export interface ICrawlerFrontiersQuery {
  params: ICrawlerFrontiersParams;
  result: ICrawlerFrontiersResult;
}

const crawlerFrontiersIR: any = {"usedParamSet":{"federation":true,"kind":true,"key":true,"offset":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":131,"b":142}]},{"name":"kind","required":true,"transform":{"type":"scalar"},"locs":[{"a":155,"b":160}]},{"name":"key","required":true,"transform":{"type":"scalar"},"locs":[{"a":169,"b":173},{"a":195,"b":199}]},{"name":"offset","required":true,"transform":{"type":"scalar"},"locs":[{"a":236,"b":243}]}],"statement":"SELECT id, key, fetch_status, process_status, error_count, last_fetched_at, next_fetch_at\nFROM crawler.frontier\nWHERE federation = :federation! AND kind = :kind!\n  AND (:key! = '' OR strpos(key, :key!) > 0)\nORDER BY key\nLIMIT 50 OFFSET :offset!"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id, key, fetch_status, process_status, error_count, last_fetched_at, next_fetch_at
 * FROM crawler.frontier
 * WHERE federation = :federation! AND kind = :kind!
 *   AND (:key! = '' OR strpos(key, :key!) > 0)
 * ORDER BY key
 * LIMIT 50 OFFSET :offset!
 * ```
 */
export const crawlerFrontiers = new PreparedQuery<ICrawlerFrontiersParams,ICrawlerFrontiersResult>(crawlerFrontiersIR);


/** 'CrawlerFrontierCount' parameters type */
export interface ICrawlerFrontierCountParams {
  federation: string;
  key: string;
  kind: string;
}

/** 'CrawlerFrontierCount' return type */
export interface ICrawlerFrontierCountResult {
  total: number | null;
}

/** 'CrawlerFrontierCount' query type */
export interface ICrawlerFrontierCountQuery {
  params: ICrawlerFrontierCountParams;
  result: ICrawlerFrontierCountResult;
}

const crawlerFrontierCountIR: any = {"usedParamSet":{"federation":true,"kind":true,"key":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":71,"b":82}]},{"name":"kind","required":true,"transform":{"type":"scalar"},"locs":[{"a":95,"b":100}]},{"name":"key","required":true,"transform":{"type":"scalar"},"locs":[{"a":109,"b":113},{"a":135,"b":139}]}],"statement":"SELECT count(*)::int AS total\nFROM crawler.frontier\nWHERE federation = :federation! AND kind = :kind!\n  AND (:key! = '' OR strpos(key, :key!) > 0)"};

/**
 * Query generated from SQL:
 * ```
 * SELECT count(*)::int AS total
 * FROM crawler.frontier
 * WHERE federation = :federation! AND kind = :kind!
 *   AND (:key! = '' OR strpos(key, :key!) > 0)
 * ```
 */
export const crawlerFrontierCount = new PreparedQuery<ICrawlerFrontierCountParams,ICrawlerFrontierCountResult>(crawlerFrontierCountIR);


