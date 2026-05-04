/** Types generated for queries found in "crawler/crawler.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type fetch_status = 'error' | 'gone' | 'ok' | 'pending' | 'transient';

export type process_status = 'error' | 'ok' | 'pending';

export type DateOrString = Date | string;

export type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

export type NumberOrString = number | string;

export type NumberOrStringArray = (NumberOrString)[];

export type numberArray = (number)[];

export type stringArray = (string)[];

/** 'GetFrontierForUpdate' parameters type */
export interface IGetFrontierForUpdateParams {
  id?: NumberOrString | null | void;
}

/** 'GetFrontierForUpdate' return type */
export interface IGetFrontierForUpdateResult {
  error_count: number;
  federation: string;
  fetch_status: fetch_status;
  id: string;
  key: string;
  kind: string;
  meta: Json;
}

/** 'GetFrontierForUpdate' query type */
export interface IGetFrontierForUpdateQuery {
  params: IGetFrontierForUpdateParams;
  result: IGetFrontierForUpdateResult;
}

const getFrontierForUpdateIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":99,"b":101}]}],"statement":"SELECT id, federation, kind, key, fetch_status, error_count, meta\nFROM crawler.frontier\nWHERE id = :id::bigint\nFOR UPDATE"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id, federation, kind, key, fetch_status, error_count, meta
 * FROM crawler.frontier
 * WHERE id = :id::bigint
 * FOR UPDATE
 * ```
 */
export const getFrontierForUpdate = new PreparedQuery<IGetFrontierForUpdateParams,IGetFrontierForUpdateResult>(getFrontierForUpdateIR);


/** 'GetDistinctFrontierKinds' parameters type */
export type IGetDistinctFrontierKindsParams = void;

/** 'GetDistinctFrontierKinds' return type */
export interface IGetDistinctFrontierKindsResult {
  federation: string;
  kind: string;
}

/** 'GetDistinctFrontierKinds' query type */
export interface IGetDistinctFrontierKindsQuery {
  params: IGetDistinctFrontierKindsParams;
  result: IGetDistinctFrontierKindsResult;
}

const getDistinctFrontierKindsIR: any = {"usedParamSet":{},"params":[],"statement":"SELECT federation, kind\nFROM crawler.frontier\nGROUP BY federation, kind"};

/**
 * Query generated from SQL:
 * ```
 * SELECT federation, kind
 * FROM crawler.frontier
 * GROUP BY federation, kind
 * ```
 */
export const getDistinctFrontierKinds = new PreparedQuery<IGetDistinctFrontierKindsParams,IGetDistinctFrontierKindsResult>(getDistinctFrontierKindsIR);


/** 'GetFrontierKindStatus' parameters type */
export interface IGetFrontierKindStatusParams {
  allowRefetch?: boolean | null | void;
  federation?: string | null | void;
  kind?: string | null | void;
}

/** 'GetFrontierKindStatus' return type */
export interface IGetFrontierKindStatusResult {
  federation: string;
  fetch_due: number | null;
  fetch_error: number | null;
  fetch_gone: number | null;
  fetch_ok: number | null;
  fetch_pending: number | null;
  fetch_transient: number | null;
  keys: string | null;
  kind: string;
  latest: Date | null;
  latest_fetch: Date | null;
  process_error: number | null;
  process_ok: number | null;
  process_pending: number | null;
  response_rows: number | null;
  total: number | null;
}

/** 'GetFrontierKindStatus' query type */
export interface IGetFrontierKindStatusQuery {
  params: IGetFrontierKindStatusParams;
  result: IGetFrontierKindStatusResult;
}

const getFrontierKindStatusIR: any = {"usedParamSet":{"federation":true,"kind":true,"allowRefetch":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":277,"b":287},{"a":321,"b":331},{"a":789,"b":799},{"a":834,"b":844}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":343,"b":347},{"a":375,"b":379},{"a":856,"b":860},{"a":889,"b":893}]},{"name":"allowRefetch","required":false,"transform":{"type":"scalar"},"locs":[{"a":753,"b":765}]}],"statement":"WITH frontiers AS (\n  SELECT\n    f.*,\n    row_number() OVER (\n      PARTITION BY f.federation, f.kind\n      ORDER BY\n        CASE WHEN f.fetch_status = 'pending' THEN 0 ELSE 1 END,\n        f.discovered_at DESC,\n        f.id\n    ) AS key_rank\n  FROM crawler.frontier f\n  WHERE (:federation::text IS NULL OR f.federation = :federation)\n    AND (:kind::text IS NULL OR f.kind = :kind)\n), response_counts AS (\n  SELECT\n    f.federation,\n    f.kind,\n    count(*)::int AS response_rows,\n    max(r.fetched_at) AS latest_response\n  FROM frontiers f\n  JOIN crawler.json_response r ON r.frontier_id = f.id\n  GROUP BY f.federation, f.kind\n), due_status AS (\n  SELECT\n    df.federation,\n    df.kind,\n    count(*)::int AS fetch_due\n  FROM crawler.frontier_fetch_due(:allowRefetch::boolean) df\n  WHERE (:federation::text IS NULL OR df.federation = :federation)\n    AND (:kind::text IS NULL OR df.kind = :kind)\n  GROUP BY df.federation, df.kind\n), status AS (\n  SELECT\n    federation,\n    kind,\n    count(*)::int AS total,\n    coalesce(\n      string_agg(key, ', ' ORDER BY key_rank) FILTER (WHERE key <> '' AND key_rank <= 3)\n        || CASE WHEN count(*) FILTER (WHERE key <> '') > 3 THEN ', ...' ELSE '' END,\n      '-'\n    ) AS keys,\n    count(*) FILTER (WHERE fetch_status = 'pending')::int AS fetch_pending,\n    count(*) FILTER (WHERE fetch_status = 'transient')::int AS fetch_transient,\n    count(*) FILTER (WHERE fetch_status = 'ok')::int AS fetch_ok,\n    count(*) FILTER (WHERE fetch_status = 'gone')::int AS fetch_gone,\n    count(*) FILTER (WHERE fetch_status = 'error')::int AS fetch_error,\n    count(*) FILTER (WHERE process_status = 'pending')::int AS process_pending,\n    count(*) FILTER (WHERE process_status = 'ok')::int AS process_ok,\n    count(*) FILTER (WHERE process_status = 'error')::int AS process_error,\n    max(last_fetched_at) AS latest_fetch\n  FROM frontiers f\n  GROUP BY federation, kind\n)\nSELECT\n  s.*,\n  coalesce(ds.fetch_due, 0)::int AS fetch_due,\n  coalesce(rc.response_rows, 0)::int AS response_rows,\n  coalesce(s.latest_fetch, rc.latest_response) AS latest\nFROM status s\nLEFT JOIN response_counts rc ON rc.federation = s.federation AND rc.kind = s.kind\nLEFT JOIN due_status ds ON ds.federation = s.federation AND ds.kind = s.kind\nORDER BY s.federation, s.kind"};

/**
 * Query generated from SQL:
 * ```
 * WITH frontiers AS (
 *   SELECT
 *     f.*,
 *     row_number() OVER (
 *       PARTITION BY f.federation, f.kind
 *       ORDER BY
 *         CASE WHEN f.fetch_status = 'pending' THEN 0 ELSE 1 END,
 *         f.discovered_at DESC,
 *         f.id
 *     ) AS key_rank
 *   FROM crawler.frontier f
 *   WHERE (:federation::text IS NULL OR f.federation = :federation)
 *     AND (:kind::text IS NULL OR f.kind = :kind)
 * ), response_counts AS (
 *   SELECT
 *     f.federation,
 *     f.kind,
 *     count(*)::int AS response_rows,
 *     max(r.fetched_at) AS latest_response
 *   FROM frontiers f
 *   JOIN crawler.json_response r ON r.frontier_id = f.id
 *   GROUP BY f.federation, f.kind
 * ), due_status AS (
 *   SELECT
 *     df.federation,
 *     df.kind,
 *     count(*)::int AS fetch_due
 *   FROM crawler.frontier_fetch_due(:allowRefetch::boolean) df
 *   WHERE (:federation::text IS NULL OR df.federation = :federation)
 *     AND (:kind::text IS NULL OR df.kind = :kind)
 *   GROUP BY df.federation, df.kind
 * ), status AS (
 *   SELECT
 *     federation,
 *     kind,
 *     count(*)::int AS total,
 *     coalesce(
 *       string_agg(key, ', ' ORDER BY key_rank) FILTER (WHERE key <> '' AND key_rank <= 3)
 *         || CASE WHEN count(*) FILTER (WHERE key <> '') > 3 THEN ', ...' ELSE '' END,
 *       '-'
 *     ) AS keys,
 *     count(*) FILTER (WHERE fetch_status = 'pending')::int AS fetch_pending,
 *     count(*) FILTER (WHERE fetch_status = 'transient')::int AS fetch_transient,
 *     count(*) FILTER (WHERE fetch_status = 'ok')::int AS fetch_ok,
 *     count(*) FILTER (WHERE fetch_status = 'gone')::int AS fetch_gone,
 *     count(*) FILTER (WHERE fetch_status = 'error')::int AS fetch_error,
 *     count(*) FILTER (WHERE process_status = 'pending')::int AS process_pending,
 *     count(*) FILTER (WHERE process_status = 'ok')::int AS process_ok,
 *     count(*) FILTER (WHERE process_status = 'error')::int AS process_error,
 *     max(last_fetched_at) AS latest_fetch
 *   FROM frontiers f
 *   GROUP BY federation, kind
 * )
 * SELECT
 *   s.*,
 *   coalesce(ds.fetch_due, 0)::int AS fetch_due,
 *   coalesce(rc.response_rows, 0)::int AS response_rows,
 *   coalesce(s.latest_fetch, rc.latest_response) AS latest
 * FROM status s
 * LEFT JOIN response_counts rc ON rc.federation = s.federation AND rc.kind = s.kind
 * LEFT JOIN due_status ds ON ds.federation = s.federation AND ds.kind = s.kind
 * ORDER BY s.federation, s.kind
 * ```
 */
export const getFrontierKindStatus = new PreparedQuery<IGetFrontierKindStatusParams,IGetFrontierKindStatusResult>(getFrontierKindStatusIR);


/** 'QueueCrawlerKick' parameters type */
export type IQueueCrawlerKickParams = void;

/** 'QueueCrawlerKick' return type */
export interface IQueueCrawlerKickResult {
  job_id: string | null;
}

/** 'QueueCrawlerKick' query type */
export interface IQueueCrawlerKickQuery {
  params: IQueueCrawlerKickParams;
  result: IQueueCrawlerKickResult;
}

const queueCrawlerKickIR: any = {"usedParamSet":{},"params":[],"statement":"SELECT (\n  graphile_worker.add_job(\n    identifier => 'frontier_schedule',\n    payload => '{}'::json,\n    run_at => now(),\n    job_key => 'frontier_schedule',\n    job_key_mode => 'replace'\n  )\n).id AS job_id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT (
 *   graphile_worker.add_job(
 *     identifier => 'frontier_schedule',
 *     payload => '{}'::json,
 *     run_at => now(),
 *     job_key => 'frontier_schedule',
 *     job_key_mode => 'replace'
 *   )
 * ).id AS job_id
 * ```
 */
export const queueCrawlerKick = new PreparedQuery<IQueueCrawlerKickParams,IQueueCrawlerKickResult>(queueCrawlerKickIR);


/** 'GetBacktestFrontierResponses' parameters type */
export interface IGetBacktestFrontierResponsesParams {
  federation?: string | null | void;
  kind?: string | null | void;
}

/** 'GetBacktestFrontierResponses' return type */
export interface IGetBacktestFrontierResponsesResult {
  content: Json;
  http_status: number | null;
  id: string;
  url: string;
}

/** 'GetBacktestFrontierResponses' query type */
export interface IGetBacktestFrontierResponsesQuery {
  params: IGetBacktestFrontierResponsesParams;
  result: IGetBacktestFrontierResponsesResult;
}

const getBacktestFrontierResponsesIR: any = {"usedParamSet":{"federation":true,"kind":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":397,"b":407}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":424,"b":428}]}],"statement":"SELECT f.id, jr.url, jr.http_status, jrc.content\nFROM crawler.frontier f\nJOIN LATERAL (\n  SELECT jr.*\n  FROM crawler.json_response jr\n  WHERE jr.frontier_id = f.id\n    AND jr.error IS NULL\n    AND (jr.http_status IS NULL OR jr.http_status < 400)\n  ORDER BY jr.fetched_at DESC\n  LIMIT 1\n  ) jr ON true\nJOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash\nWHERE f.federation = :federation\n  AND f.kind = :kind"};

/**
 * Query generated from SQL:
 * ```
 * SELECT f.id, jr.url, jr.http_status, jrc.content
 * FROM crawler.frontier f
 * JOIN LATERAL (
 *   SELECT jr.*
 *   FROM crawler.json_response jr
 *   WHERE jr.frontier_id = f.id
 *     AND jr.error IS NULL
 *     AND (jr.http_status IS NULL OR jr.http_status < 400)
 *   ORDER BY jr.fetched_at DESC
 *   LIMIT 1
 *   ) jr ON true
 * JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
 * WHERE f.federation = :federation
 *   AND f.kind = :kind
 * ```
 */
export const getBacktestFrontierResponses = new PreparedQuery<IGetBacktestFrontierResponsesParams,IGetBacktestFrontierResponsesResult>(getBacktestFrontierResponsesIR);


/** 'GetLatestFrontierFailures' parameters type */
export interface IGetLatestFrontierFailuresParams {
  excludeHttpStatuses?: numberArray | null | void;
  federation?: string | null | void;
  kind?: string | null | void;
  limit?: NumberOrString | null | void;
}

/** 'GetLatestFrontierFailures' return type */
export interface IGetLatestFrontierFailuresResult {
  error_count: number | null;
  failed_at: Date | null;
  failure: string | null;
  federation: string | null;
  fetch_status: fetch_status | null;
  http_status: number | null;
  id: string | null;
  key: string | null;
  kind: string | null;
  next_fetch_at: Date | null;
  process_error: string | null;
  process_status: process_status | null;
  response_error: string | null;
  url: string | null;
}

/** 'GetLatestFrontierFailures' query type */
export interface IGetLatestFrontierFailuresQuery {
  params: IGetLatestFrontierFailuresParams;
  result: IGetLatestFrontierFailuresResult;
}

const getLatestFrontierFailuresIR: any = {"usedParamSet":{"federation":true,"kind":true,"excludeHttpStatuses":true,"limit":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":225,"b":235},{"a":267,"b":277}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":287,"b":291},{"a":317,"b":321}]},{"name":"excludeHttpStatuses","required":false,"transform":{"type":"scalar"},"locs":[{"a":336,"b":355},{"a":429,"b":448}]},{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":502,"b":507}]}],"statement":"SELECT\n  id,\n  federation,\n  kind,\n  key,\n  fetch_status,\n  process_status,\n  error_count,\n  next_fetch_at,\n  failed_at,\n  url,\n  http_status,\n  response_error,\n  process_error,\n  failure\nFROM crawler.frontier_failure\nWHERE (:federation::text IS NULL OR federation = :federation)\n  AND (:kind::text IS NULL OR kind = :kind)\n  AND (\n    :excludeHttpStatuses::int[] IS NULL\n    OR http_status IS NULL\n    OR NOT (http_status = ANY(:excludeHttpStatuses::int[]))\n  )\nORDER BY failed_at DESC, id DESC\nLIMIT :limit"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   id,
 *   federation,
 *   kind,
 *   key,
 *   fetch_status,
 *   process_status,
 *   error_count,
 *   next_fetch_at,
 *   failed_at,
 *   url,
 *   http_status,
 *   response_error,
 *   process_error,
 *   failure
 * FROM crawler.frontier_failure
 * WHERE (:federation::text IS NULL OR federation = :federation)
 *   AND (:kind::text IS NULL OR kind = :kind)
 *   AND (
 *     :excludeHttpStatuses::int[] IS NULL
 *     OR http_status IS NULL
 *     OR NOT (http_status = ANY(:excludeHttpStatuses::int[]))
 *   )
 * ORDER BY failed_at DESC, id DESC
 * LIMIT :limit
 * ```
 */
export const getLatestFrontierFailures = new PreparedQuery<IGetLatestFrontierFailuresParams,IGetLatestFrontierFailuresResult>(getLatestFrontierFailuresIR);


/** 'GetFrontierFailureGroups' parameters type */
export interface IGetFrontierFailureGroupsParams {
  excludeHttpStatuses?: numberArray | null | void;
  federation?: string | null | void;
  kind?: string | null | void;
  limit?: NumberOrString | null | void;
}

/** 'GetFrontierFailureGroups' return type */
export interface IGetFrontierFailureGroupsResult {
  count: number | null;
  error_fingerprint: string | null;
  failure: string | null;
  federation: string | null;
  http_status: number | null;
  kind: string | null;
  latest_failed_at: Date | null;
  sample_id: string | null;
  sample_key: string | null;
  samples: string | null;
}

/** 'GetFrontierFailureGroups' query type */
export interface IGetFrontierFailureGroupsQuery {
  params: IGetFrontierFailureGroupsParams;
  result: IGetFrontierFailureGroupsResult;
}

const getFrontierFailureGroupsIR: any = {"usedParamSet":{"federation":true,"kind":true,"excludeHttpStatuses":true,"limit":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":213,"b":223},{"a":255,"b":265}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":277,"b":281},{"a":307,"b":311}]},{"name":"excludeHttpStatuses","required":false,"transform":{"type":"scalar"},"locs":[{"a":330,"b":349},{"a":427,"b":446}]},{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":1411,"b":1416}]}],"statement":"WITH failures AS (\n  SELECT\n    id,\n    federation,\n    kind,\n    key,\n    failed_at,\n    http_status,\n    failure,\n    coalesce(nullif(error_text, ''), '-') AS error_text\n  FROM crawler.frontier_failure\n  WHERE (:federation::text IS NULL OR federation = :federation)\n    AND (:kind::text IS NULL OR kind = :kind)\n    AND (\n      :excludeHttpStatuses::int[] IS NULL\n      OR http_status IS NULL\n      OR NOT (http_status = ANY(:excludeHttpStatuses::int[]))\n    )\n), fingerprinted AS (\n  SELECT\n    *,\n    left(regexp_replace(error_text, '\\s+', ' ', 'g'), 180) AS error_fingerprint\n  FROM failures\n), ranked AS (\n  SELECT\n    *,\n    row_number() OVER (\n      PARTITION BY federation, kind, failure, http_status, error_fingerprint\n      ORDER BY failed_at DESC, id DESC\n    ) AS sample_rank\n  FROM fingerprinted\n)\nSELECT\n  federation,\n  kind,\n  failure,\n  http_status,\n  error_fingerprint,\n  count(*)::int AS count,\n  max(failed_at) AS latest_failed_at,\n  (array_agg(id ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank = 1))[1]\n    AS sample_id,\n  (array_agg(key ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank = 1))[1]\n    AS sample_key,\n  string_agg(id::text || ':' || key, ', ' ORDER BY failed_at DESC, id DESC)\n    FILTER (WHERE sample_rank <= 5) AS samples\nFROM ranked\nGROUP BY federation, kind, failure, http_status, error_fingerprint\nORDER BY count DESC, latest_failed_at DESC\nLIMIT :limit"};

/**
 * Query generated from SQL:
 * ```
 * WITH failures AS (
 *   SELECT
 *     id,
 *     federation,
 *     kind,
 *     key,
 *     failed_at,
 *     http_status,
 *     failure,
 *     coalesce(nullif(error_text, ''), '-') AS error_text
 *   FROM crawler.frontier_failure
 *   WHERE (:federation::text IS NULL OR federation = :federation)
 *     AND (:kind::text IS NULL OR kind = :kind)
 *     AND (
 *       :excludeHttpStatuses::int[] IS NULL
 *       OR http_status IS NULL
 *       OR NOT (http_status = ANY(:excludeHttpStatuses::int[]))
 *     )
 * ), fingerprinted AS (
 *   SELECT
 *     *,
 *     left(regexp_replace(error_text, '\s+', ' ', 'g'), 180) AS error_fingerprint
 *   FROM failures
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
 *   max(failed_at) AS latest_failed_at,
 *   (array_agg(id ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank = 1))[1]
 *     AS sample_id,
 *   (array_agg(key ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank = 1))[1]
 *     AS sample_key,
 *   string_agg(id::text || ':' || key, ', ' ORDER BY failed_at DESC, id DESC)
 *     FILTER (WHERE sample_rank <= 5) AS samples
 * FROM ranked
 * GROUP BY federation, kind, failure, http_status, error_fingerprint
 * ORDER BY count DESC, latest_failed_at DESC
 * LIMIT :limit
 * ```
 */
export const getFrontierFailureGroups = new PreparedQuery<IGetFrontierFailureGroupsParams,IGetFrontierFailureGroupsResult>(getFrontierFailureGroupsIR);


/** 'GetCrawlerFetchJobSummary' parameters type */
export type IGetCrawlerFetchJobSummaryParams = void;

/** 'GetCrawlerFetchJobSummary' return type */
export interface IGetCrawlerFetchJobSummaryResult {
  delayed: number | null;
  failed: number | null;
  latest_error: string | null;
  latest_update_at: Date | null;
  locked: number | null;
  next_run_at: Date | null;
  oldest_ready_at: Date | null;
  ready: number | null;
  total: number | null;
}

/** 'GetCrawlerFetchJobSummary' query type */
export interface IGetCrawlerFetchJobSummaryQuery {
  params: IGetCrawlerFetchJobSummaryParams;
  result: IGetCrawlerFetchJobSummaryResult;
}

const getCrawlerFetchJobSummaryIR: any = {"usedParamSet":{},"params":[],"statement":"SELECT\n  count(*)::int AS total,\n  count(*) FILTER (WHERE state = 'ready')::int AS ready,\n  count(*) FILTER (WHERE state = 'delayed')::int AS delayed,\n  count(*) FILTER (WHERE state = 'locked')::int AS locked,\n  count(*) FILTER (WHERE state = 'failed')::int AS failed,\n  min(run_at) FILTER (WHERE state = 'ready') AS oldest_ready_at,\n  min(run_at) FILTER (WHERE state = 'delayed') AS next_run_at,\n  max(job_updated_at) AS latest_update_at,\n  (array_agg(left(job_error, 300) ORDER BY job_updated_at DESC)\n    FILTER (WHERE job_error IS NOT NULL))[1] AS latest_error\nFROM crawler.frontier_fetch_job"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   count(*)::int AS total,
 *   count(*) FILTER (WHERE state = 'ready')::int AS ready,
 *   count(*) FILTER (WHERE state = 'delayed')::int AS delayed,
 *   count(*) FILTER (WHERE state = 'locked')::int AS locked,
 *   count(*) FILTER (WHERE state = 'failed')::int AS failed,
 *   min(run_at) FILTER (WHERE state = 'ready') AS oldest_ready_at,
 *   min(run_at) FILTER (WHERE state = 'delayed') AS next_run_at,
 *   max(job_updated_at) AS latest_update_at,
 *   (array_agg(left(job_error, 300) ORDER BY job_updated_at DESC)
 *     FILTER (WHERE job_error IS NOT NULL))[1] AS latest_error
 * FROM crawler.frontier_fetch_job
 * ```
 */
export const getCrawlerFetchJobSummary = new PreparedQuery<IGetCrawlerFetchJobSummaryParams,IGetCrawlerFetchJobSummaryResult>(getCrawlerFetchJobSummaryIR);


/** 'GetCrawlerControlJobStatus' parameters type */
export type IGetCrawlerControlJobStatusParams = void;

/** 'GetCrawlerControlJobStatus' return type */
export interface IGetCrawlerControlJobStatusResult {
  delayed: number | null;
  failed: number | null;
  latest_error: string | null;
  latest_update_at: Date | null;
  locked: number | null;
  next_run_at: Date | null;
  oldest_ready_at: Date | null;
  ready: number | null;
  task_identifier: string | null;
  total: number | null;
}

/** 'GetCrawlerControlJobStatus' query type */
export interface IGetCrawlerControlJobStatusQuery {
  params: IGetCrawlerControlJobStatusParams;
  result: IGetCrawlerControlJobStatusResult;
}

const getCrawlerControlJobStatusIR: any = {"usedParamSet":{},"params":[],"statement":"SELECT\n  task_identifier,\n  count(*)::int AS total,\n  count(*) FILTER (\n    WHERE locked_at IS NULL AND run_at <= now() AND attempts < max_attempts\n  )::int AS ready,\n  count(*) FILTER (\n    WHERE locked_at IS NULL AND run_at > now() AND attempts < max_attempts\n  )::int AS delayed,\n  count(*) FILTER (WHERE locked_at IS NOT NULL)::int AS locked,\n  count(*) FILTER (WHERE attempts >= max_attempts)::int AS failed,\n  min(run_at) FILTER (\n    WHERE locked_at IS NULL AND run_at <= now() AND attempts < max_attempts\n  ) AS oldest_ready_at,\n  min(run_at) FILTER (\n    WHERE locked_at IS NULL AND run_at > now() AND attempts < max_attempts\n  ) AS next_run_at,\n  max(updated_at) AS latest_update_at,\n  (array_agg(left(last_error, 300) ORDER BY updated_at DESC)\n    FILTER (WHERE last_error IS NOT NULL))[1] AS latest_error\nFROM graphile_worker.jobs\nWHERE task_identifier IN ('frontier_schedule', 'frontier_process')\nGROUP BY task_identifier\nORDER BY task_identifier"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   task_identifier,
 *   count(*)::int AS total,
 *   count(*) FILTER (
 *     WHERE locked_at IS NULL AND run_at <= now() AND attempts < max_attempts
 *   )::int AS ready,
 *   count(*) FILTER (
 *     WHERE locked_at IS NULL AND run_at > now() AND attempts < max_attempts
 *   )::int AS delayed,
 *   count(*) FILTER (WHERE locked_at IS NOT NULL)::int AS locked,
 *   count(*) FILTER (WHERE attempts >= max_attempts)::int AS failed,
 *   min(run_at) FILTER (
 *     WHERE locked_at IS NULL AND run_at <= now() AND attempts < max_attempts
 *   ) AS oldest_ready_at,
 *   min(run_at) FILTER (
 *     WHERE locked_at IS NULL AND run_at > now() AND attempts < max_attempts
 *   ) AS next_run_at,
 *   max(updated_at) AS latest_update_at,
 *   (array_agg(left(last_error, 300) ORDER BY updated_at DESC)
 *     FILTER (WHERE last_error IS NOT NULL))[1] AS latest_error
 * FROM graphile_worker.jobs
 * WHERE task_identifier IN ('frontier_schedule', 'frontier_process')
 * GROUP BY task_identifier
 * ORDER BY task_identifier
 * ```
 */
export const getCrawlerControlJobStatus = new PreparedQuery<IGetCrawlerControlJobStatusParams,IGetCrawlerControlJobStatusResult>(getCrawlerControlJobStatusIR);


/** 'GetCrawlerFrontierJobs' parameters type */
export interface IGetCrawlerFrontierJobsParams {
  federation?: string | null | void;
  kind?: string | null | void;
  limit?: NumberOrString | null | void;
  mode?: string | null | void;
}

/** 'GetCrawlerFrontierJobs' return type */
export interface IGetCrawlerFrontierJobsResult {
  attempts: number | null;
  federation: string | null;
  fetch_status: fetch_status | null;
  frontier_id: string | null;
  frontier_key: string | null;
  job_error: string | null;
  job_id: string | null;
  job_updated_at: Date | null;
  kind: string | null;
  locked_at: Date | null;
  max_attempts: number | null;
  process_error: string | null;
  process_status: process_status | null;
  response_content: Json | null;
  response_error: string | null;
  response_http_status: number | null;
  run_at: Date | null;
  state: string | null;
}

/** 'GetCrawlerFrontierJobs' query type */
export interface IGetCrawlerFrontierJobsQuery {
  params: IGetCrawlerFrontierJobsParams;
  result: IGetCrawlerFrontierJobsResult;
}

const getCrawlerFrontierJobsIR: any = {"usedParamSet":{"mode":true,"federation":true,"kind":true,"limit":true},"params":[{"name":"mode","required":false,"transform":{"type":"scalar"},"locs":[{"a":567,"b":571},{"a":593,"b":597},{"a":644,"b":648}]},{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":699,"b":709},{"a":741,"b":751}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":761,"b":765},{"a":791,"b":795}]},{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":965,"b":970}]}],"statement":"SELECT\n  job_id,\n  run_at,\n  locked_at,\n  attempts,\n  max_attempts,\n  job_error,\n  job_updated_at,\n  frontier_id,\n  federation,\n  kind,\n  frontier_key,\n  fetch_status,\n  process_status,\n  response_http_status,\n  response_error,\n  CASE\n    WHEN state = 'failed'\n      OR job_error IS NOT NULL\n      OR response_error IS NOT NULL\n      OR process_error IS NOT NULL\n      OR fetch_status IN ('error', 'transient')\n      OR process_status = 'error'\n      THEN response_content\n  END AS response_content,\n  process_error,\n  state\nFROM crawler.frontier_fetch_job\nWHERE (\n  :mode::text = 'all'\n  OR (:mode::text = 'failed' AND state = 'failed')\n  OR (:mode::text = 'active' AND state <> 'failed')\n)\n  AND (:federation::text IS NULL OR federation = :federation)\n  AND (:kind::text IS NULL OR kind = :kind)\nORDER BY\n  CASE state\n    WHEN 'failed' THEN 0\n    WHEN 'locked' THEN 1\n    WHEN 'ready' THEN 2\n    ELSE 3\n  END,\n  job_updated_at DESC,\n  run_at,\n  job_id DESC\nLIMIT :limit"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   job_id,
 *   run_at,
 *   locked_at,
 *   attempts,
 *   max_attempts,
 *   job_error,
 *   job_updated_at,
 *   frontier_id,
 *   federation,
 *   kind,
 *   frontier_key,
 *   fetch_status,
 *   process_status,
 *   response_http_status,
 *   response_error,
 *   CASE
 *     WHEN state = 'failed'
 *       OR job_error IS NOT NULL
 *       OR response_error IS NOT NULL
 *       OR process_error IS NOT NULL
 *       OR fetch_status IN ('error', 'transient')
 *       OR process_status = 'error'
 *       THEN response_content
 *   END AS response_content,
 *   process_error,
 *   state
 * FROM crawler.frontier_fetch_job
 * WHERE (
 *   :mode::text = 'all'
 *   OR (:mode::text = 'failed' AND state = 'failed')
 *   OR (:mode::text = 'active' AND state <> 'failed')
 * )
 *   AND (:federation::text IS NULL OR federation = :federation)
 *   AND (:kind::text IS NULL OR kind = :kind)
 * ORDER BY
 *   CASE state
 *     WHEN 'failed' THEN 0
 *     WHEN 'locked' THEN 1
 *     WHEN 'ready' THEN 2
 *     ELSE 3
 *   END,
 *   job_updated_at DESC,
 *   run_at,
 *   job_id DESC
 * LIMIT :limit
 * ```
 */
export const getCrawlerFrontierJobs = new PreparedQuery<IGetCrawlerFrontierJobsParams,IGetCrawlerFrontierJobsResult>(getCrawlerFrontierJobsIR);


/** 'GetCrawlerScheduleStatus' parameters type */
export type IGetCrawlerScheduleStatusParams = void;

/** 'GetCrawlerScheduleStatus' return type */
export interface IGetCrawlerScheduleStatusResult {
  delayed: number | null;
  host: string | null;
  locked: number | null;
  max_requests: number;
  next_available_at: Date;
  next_run_at: Date | null;
  per_interval: string | null;
  queue_tail_at: Date | null;
  queued: number | null;
  ready: number | null;
  spacing: number | null;
}

/** 'GetCrawlerScheduleStatus' query type */
export interface IGetCrawlerScheduleStatusQuery {
  params: IGetCrawlerScheduleStatusParams;
  result: IGetCrawlerScheduleStatusResult;
}

const getCrawlerScheduleStatusIR: any = {"usedParamSet":{},"params":[],"statement":"WITH fetch_jobs AS (\n  SELECT\n    host,\n    count(*) FILTER (WHERE state IN ('ready', 'delayed'))::int AS queued,\n    count(*) FILTER (WHERE state = 'ready')::int AS ready,\n    count(*) FILTER (WHERE state = 'delayed')::int AS delayed,\n    count(*) FILTER (WHERE state = 'locked')::int AS locked,\n    min(run_at) FILTER (WHERE state IN ('ready', 'delayed')) AS next_run_at,\n    max(run_at) FILTER (WHERE state <> 'failed') AS queue_tail_at\n  FROM crawler.frontier_fetch_job\n  WHERE host IS NOT NULL\n  GROUP BY host\n)\nSELECT\n  COALESCE(r.host, fj.host) AS host,\n  r.max_requests,\n  r.per_interval::text AS per_interval,\n  (extract(epoch from r.spacing) * 1000)::int AS spacing,\n  r.next_available_at,\n  COALESCE(fj.queued, 0)::int AS queued,\n  COALESCE(fj.ready, 0)::int AS ready,\n  COALESCE(fj.delayed, 0)::int AS delayed,\n  COALESCE(fj.locked, 0)::int AS locked,\n  fj.next_run_at,\n  fj.queue_tail_at\nFROM crawler.rate_limit_rule r\nFULL JOIN fetch_jobs fj USING (host)\nORDER BY host"};

/**
 * Query generated from SQL:
 * ```
 * WITH fetch_jobs AS (
 *   SELECT
 *     host,
 *     count(*) FILTER (WHERE state IN ('ready', 'delayed'))::int AS queued,
 *     count(*) FILTER (WHERE state = 'ready')::int AS ready,
 *     count(*) FILTER (WHERE state = 'delayed')::int AS delayed,
 *     count(*) FILTER (WHERE state = 'locked')::int AS locked,
 *     min(run_at) FILTER (WHERE state IN ('ready', 'delayed')) AS next_run_at,
 *     max(run_at) FILTER (WHERE state <> 'failed') AS queue_tail_at
 *   FROM crawler.frontier_fetch_job
 *   WHERE host IS NOT NULL
 *   GROUP BY host
 * )
 * SELECT
 *   COALESCE(r.host, fj.host) AS host,
 *   r.max_requests,
 *   r.per_interval::text AS per_interval,
 *   (extract(epoch from r.spacing) * 1000)::int AS spacing,
 *   r.next_available_at,
 *   COALESCE(fj.queued, 0)::int AS queued,
 *   COALESCE(fj.ready, 0)::int AS ready,
 *   COALESCE(fj.delayed, 0)::int AS delayed,
 *   COALESCE(fj.locked, 0)::int AS locked,
 *   fj.next_run_at,
 *   fj.queue_tail_at
 * FROM crawler.rate_limit_rule r
 * FULL JOIN fetch_jobs fj USING (host)
 * ORDER BY host
 * ```
 */
export const getCrawlerScheduleStatus = new PreparedQuery<IGetCrawlerScheduleStatusParams,IGetCrawlerScheduleStatusResult>(getCrawlerScheduleStatusIR);


/** 'GetCrawlerBacklogStatus' parameters type */
export interface IGetCrawlerBacklogStatusParams {
  allowRefetch?: boolean | null | void;
  federation?: string | null | void;
  kind?: string | null | void;
}

/** 'GetCrawlerBacklogStatus' return type */
export interface IGetCrawlerBacklogStatusResult {
  delayed_fetch: number | null;
  federation: string;
  fetch_due: number | null;
  kind: string;
  locked_fetch: number | null;
  oldest_due_at: Date | null;
  oldest_process_ready_at: Date | null;
  process_ready: number | null;
  queued_fetch: number | null;
  ready_fetch: number | null;
  sample_due_id: string | null;
  sample_due_key: string | null;
  sample_process_id: string | null;
  scheduled_fetch: number | null;
  total: number | null;
  unscheduled_fetch: number | null;
}

/** 'GetCrawlerBacklogStatus' query type */
export interface IGetCrawlerBacklogStatusQuery {
  params: IGetCrawlerBacklogStatusParams;
  result: IGetCrawlerBacklogStatusResult;
}

const getCrawlerBacklogStatusIR: any = {"usedParamSet":{"allowRefetch":true,"federation":true,"kind":true},"params":[{"name":"allowRefetch","required":false,"transform":{"type":"scalar"},"locs":[{"a":68,"b":80}]},{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":104,"b":114},{"a":149,"b":159},{"a":859,"b":869},{"a":903,"b":913},{"a":1440,"b":1450},{"a":1482,"b":1492},{"a":1690,"b":1700},{"a":1734,"b":1744}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":171,"b":175},{"a":204,"b":208},{"a":925,"b":929},{"a":957,"b":961},{"a":1504,"b":1508},{"a":1534,"b":1538},{"a":1756,"b":1760},{"a":1788,"b":1792}]}],"statement":"WITH due_fetch AS (\n  SELECT df.*\n  FROM crawler.frontier_fetch_due(:allowRefetch::boolean) df\n  WHERE (:federation::text IS NULL OR df.federation = :federation)\n    AND (:kind::text IS NULL OR df.kind = :kind)\n), due_status AS (\n  SELECT\n    federation,\n    kind,\n    count(*)::int AS fetch_due,\n    min(due_at) AS oldest_due_at,\n    (array_agg(id ORDER BY due_at, last_fetched_at NULLS FIRST, id))[1] AS sample_due_id,\n    (array_agg(key ORDER BY due_at, last_fetched_at NULLS FIRST, id))[1] AS sample_due_key\n  FROM due_fetch\n  GROUP BY federation, kind\n), process_status AS (\n  SELECT\n    f.federation,\n    f.kind,\n    count(*)::int AS process_ready,\n    min(f.last_fetched_at) AS oldest_process_ready_at,\n    (array_agg(f.id ORDER BY f.last_fetched_at NULLS FIRST, f.discovered_at, f.id))[1]\n      AS sample_process_id\n  FROM crawler.frontier f\n  WHERE (:federation::text IS NULL OR f.federation = :federation)\n    AND (:kind::text IS NULL OR f.kind = :kind)\n    AND f.process_status = 'pending'\n    AND f.fetch_status = 'ok'\n  GROUP BY f.federation, f.kind\n), fetch_jobs AS (\n  SELECT\n    federation,\n    kind,\n    count(*) FILTER (WHERE state IN ('ready', 'delayed'))::int AS queued_fetch,\n    count(*) FILTER (WHERE state = 'ready')::int AS ready_fetch,\n    count(*) FILTER (WHERE state = 'delayed')::int AS delayed_fetch,\n    count(*) FILTER (WHERE state = 'locked')::int AS locked_fetch\n  FROM crawler.frontier_fetch_job\n  WHERE (:federation::text IS NULL OR federation = :federation)\n    AND (:kind::text IS NULL OR kind = :kind)\n  GROUP BY federation, kind\n), kind_totals AS (\n  SELECT\n    f.federation,\n    f.kind,\n    count(*)::int AS total\n  FROM crawler.frontier f\n  WHERE (:federation::text IS NULL OR f.federation = :federation)\n    AND (:kind::text IS NULL OR f.kind = :kind)\n  GROUP BY f.federation, f.kind\n)\nSELECT\n  kt.federation,\n  kt.kind,\n  kt.total,\n  coalesce(ds.fetch_due, 0)::int AS fetch_due,\n  ds.oldest_due_at,\n  ds.sample_due_id,\n  ds.sample_due_key,\n  coalesce(ps.process_ready, 0)::int AS process_ready,\n  ps.oldest_process_ready_at,\n  ps.sample_process_id,\n  coalesce(fj.queued_fetch, 0)::int AS queued_fetch,\n  coalesce(fj.ready_fetch, 0)::int AS ready_fetch,\n  coalesce(fj.delayed_fetch, 0)::int AS delayed_fetch,\n  coalesce(fj.locked_fetch, 0)::int AS locked_fetch,\n  (coalesce(fj.queued_fetch, 0) + coalesce(fj.locked_fetch, 0))::int\n    AS scheduled_fetch,\n  greatest(\n    coalesce(ds.fetch_due, 0) - coalesce(fj.queued_fetch, 0) - coalesce(fj.locked_fetch, 0),\n    0\n  )::int AS unscheduled_fetch\nFROM kind_totals kt\nLEFT JOIN due_status ds USING (federation, kind)\nLEFT JOIN process_status ps USING (federation, kind)\nLEFT JOIN fetch_jobs fj USING (federation, kind)\nORDER BY kt.federation, kt.kind"};

/**
 * Query generated from SQL:
 * ```
 * WITH due_fetch AS (
 *   SELECT df.*
 *   FROM crawler.frontier_fetch_due(:allowRefetch::boolean) df
 *   WHERE (:federation::text IS NULL OR df.federation = :federation)
 *     AND (:kind::text IS NULL OR df.kind = :kind)
 * ), due_status AS (
 *   SELECT
 *     federation,
 *     kind,
 *     count(*)::int AS fetch_due,
 *     min(due_at) AS oldest_due_at,
 *     (array_agg(id ORDER BY due_at, last_fetched_at NULLS FIRST, id))[1] AS sample_due_id,
 *     (array_agg(key ORDER BY due_at, last_fetched_at NULLS FIRST, id))[1] AS sample_due_key
 *   FROM due_fetch
 *   GROUP BY federation, kind
 * ), process_status AS (
 *   SELECT
 *     f.federation,
 *     f.kind,
 *     count(*)::int AS process_ready,
 *     min(f.last_fetched_at) AS oldest_process_ready_at,
 *     (array_agg(f.id ORDER BY f.last_fetched_at NULLS FIRST, f.discovered_at, f.id))[1]
 *       AS sample_process_id
 *   FROM crawler.frontier f
 *   WHERE (:federation::text IS NULL OR f.federation = :federation)
 *     AND (:kind::text IS NULL OR f.kind = :kind)
 *     AND f.process_status = 'pending'
 *     AND f.fetch_status = 'ok'
 *   GROUP BY f.federation, f.kind
 * ), fetch_jobs AS (
 *   SELECT
 *     federation,
 *     kind,
 *     count(*) FILTER (WHERE state IN ('ready', 'delayed'))::int AS queued_fetch,
 *     count(*) FILTER (WHERE state = 'ready')::int AS ready_fetch,
 *     count(*) FILTER (WHERE state = 'delayed')::int AS delayed_fetch,
 *     count(*) FILTER (WHERE state = 'locked')::int AS locked_fetch
 *   FROM crawler.frontier_fetch_job
 *   WHERE (:federation::text IS NULL OR federation = :federation)
 *     AND (:kind::text IS NULL OR kind = :kind)
 *   GROUP BY federation, kind
 * ), kind_totals AS (
 *   SELECT
 *     f.federation,
 *     f.kind,
 *     count(*)::int AS total
 *   FROM crawler.frontier f
 *   WHERE (:federation::text IS NULL OR f.federation = :federation)
 *     AND (:kind::text IS NULL OR f.kind = :kind)
 *   GROUP BY f.federation, f.kind
 * )
 * SELECT
 *   kt.federation,
 *   kt.kind,
 *   kt.total,
 *   coalesce(ds.fetch_due, 0)::int AS fetch_due,
 *   ds.oldest_due_at,
 *   ds.sample_due_id,
 *   ds.sample_due_key,
 *   coalesce(ps.process_ready, 0)::int AS process_ready,
 *   ps.oldest_process_ready_at,
 *   ps.sample_process_id,
 *   coalesce(fj.queued_fetch, 0)::int AS queued_fetch,
 *   coalesce(fj.ready_fetch, 0)::int AS ready_fetch,
 *   coalesce(fj.delayed_fetch, 0)::int AS delayed_fetch,
 *   coalesce(fj.locked_fetch, 0)::int AS locked_fetch,
 *   (coalesce(fj.queued_fetch, 0) + coalesce(fj.locked_fetch, 0))::int
 *     AS scheduled_fetch,
 *   greatest(
 *     coalesce(ds.fetch_due, 0) - coalesce(fj.queued_fetch, 0) - coalesce(fj.locked_fetch, 0),
 *     0
 *   )::int AS unscheduled_fetch
 * FROM kind_totals kt
 * LEFT JOIN due_status ds USING (federation, kind)
 * LEFT JOIN process_status ps USING (federation, kind)
 * LEFT JOIN fetch_jobs fj USING (federation, kind)
 * ORDER BY kt.federation, kt.kind
 * ```
 */
export const getCrawlerBacklogStatus = new PreparedQuery<IGetCrawlerBacklogStatusParams,IGetCrawlerBacklogStatusResult>(getCrawlerBacklogStatusIR);


/** 'GetFrontierDetail' parameters type */
export interface IGetFrontierDetailParams {
  federation?: string | null | void;
  id?: NumberOrString | null | void;
  key?: string | null | void;
  kind?: string | null | void;
}

/** 'GetFrontierDetail' return type */
export interface IGetFrontierDetailResult {
  discovered_at: Date;
  error_count: number;
  federation: string;
  fetch_status: fetch_status;
  id: string;
  job_attempts: number | null;
  job_id: string | null;
  job_key: string | null;
  job_last_error: string | null;
  job_locked_at: Date | null;
  job_max_attempts: number | null;
  job_run_at: Date | null;
  key: string;
  kind: string;
  last_fetched_at: Date | null;
  last_process_error: string | null;
  meta: Json;
  next_fetch_at: Date | null;
  process_status: process_status;
  response_content_hash: string | null;
  response_error: string | null;
  response_fetched_at: Date;
  response_http_status: number | null;
  response_url: string;
}

/** 'GetFrontierDetail' query type */
export interface IGetFrontierDetailQuery {
  params: IGetFrontierDetailParams;
  result: IGetFrontierDetailResult;
}

const getFrontierDetailIR: any = {"usedParamSet":{"id":true,"federation":true,"kind":true,"key":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":901,"b":903},{"a":940,"b":942},{"a":965,"b":967}]},{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":1008,"b":1018}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":1037,"b":1041}]},{"name":"key","required":false,"transform":{"type":"scalar"},"locs":[{"a":1059,"b":1062}]}],"statement":"SELECT\n  f.id,\n  f.federation,\n  f.kind,\n  f.key,\n  f.discovered_at,\n  f.last_fetched_at,\n  f.fetch_status,\n  f.process_status,\n  f.error_count,\n  f.next_fetch_at,\n  f.last_process_error,\n  f.meta,\n  jr.url AS response_url,\n  jr.fetched_at AS response_fetched_at,\n  jr.http_status AS response_http_status,\n  jr.error AS response_error,\n  jr.content_hash AS response_content_hash,\n  j.job_id,\n  j.job_key,\n  j.run_at AS job_run_at,\n  j.attempts AS job_attempts,\n  j.max_attempts AS job_max_attempts,\n  j.job_error AS job_last_error,\n  j.locked_at AS job_locked_at\nFROM crawler.frontier f\nLEFT JOIN LATERAL (\n  SELECT jr.*\n  FROM crawler.json_response jr\n  WHERE jr.frontier_id = f.id\n  ORDER BY jr.fetched_at DESC\n  LIMIT 1\n) jr ON true\nLEFT JOIN LATERAL (\n  SELECT j.*\n  FROM crawler.frontier_fetch_job j\n  WHERE j.frontier_id = f.id\n  ORDER BY j.job_updated_at DESC\n  LIMIT 1\n) j ON true\nWHERE (\n    :id::bigint IS NOT NULL\n    AND f.id = :id::bigint\n  ) OR (\n    :id::bigint IS NULL\n    AND f.federation = :federation\n    AND f.kind = :kind\n    AND f.key = :key\n  )"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   f.id,
 *   f.federation,
 *   f.kind,
 *   f.key,
 *   f.discovered_at,
 *   f.last_fetched_at,
 *   f.fetch_status,
 *   f.process_status,
 *   f.error_count,
 *   f.next_fetch_at,
 *   f.last_process_error,
 *   f.meta,
 *   jr.url AS response_url,
 *   jr.fetched_at AS response_fetched_at,
 *   jr.http_status AS response_http_status,
 *   jr.error AS response_error,
 *   jr.content_hash AS response_content_hash,
 *   j.job_id,
 *   j.job_key,
 *   j.run_at AS job_run_at,
 *   j.attempts AS job_attempts,
 *   j.max_attempts AS job_max_attempts,
 *   j.job_error AS job_last_error,
 *   j.locked_at AS job_locked_at
 * FROM crawler.frontier f
 * LEFT JOIN LATERAL (
 *   SELECT jr.*
 *   FROM crawler.json_response jr
 *   WHERE jr.frontier_id = f.id
 *   ORDER BY jr.fetched_at DESC
 *   LIMIT 1
 * ) jr ON true
 * LEFT JOIN LATERAL (
 *   SELECT j.*
 *   FROM crawler.frontier_fetch_job j
 *   WHERE j.frontier_id = f.id
 *   ORDER BY j.job_updated_at DESC
 *   LIMIT 1
 * ) j ON true
 * WHERE (
 *     :id::bigint IS NOT NULL
 *     AND f.id = :id::bigint
 *   ) OR (
 *     :id::bigint IS NULL
 *     AND f.federation = :federation
 *     AND f.kind = :kind
 *     AND f.key = :key
 *   )
 * ```
 */
export const getFrontierDetail = new PreparedQuery<IGetFrontierDetailParams,IGetFrontierDetailResult>(getFrontierDetailIR);


/** 'GetOutstandingJobCountForTask' parameters type */
export interface IGetOutstandingJobCountForTaskParams {
  task?: string | null | void;
}

/** 'GetOutstandingJobCountForTask' return type */
export interface IGetOutstandingJobCountForTaskResult {
  count: number | null;
}

/** 'GetOutstandingJobCountForTask' query type */
export interface IGetOutstandingJobCountForTaskQuery {
  params: IGetOutstandingJobCountForTaskParams;
  result: IGetOutstandingJobCountForTaskResult;
}

const getOutstandingJobCountForTaskIR: any = {"usedParamSet":{"task":true},"params":[{"name":"task","required":false,"transform":{"type":"scalar"},"locs":[{"a":80,"b":84}]}],"statement":"SELECT count(*)::int AS count\nFROM graphile_worker.jobs\nWHERE task_identifier = :task::text\n  AND attempts < max_attempts"};

/**
 * Query generated from SQL:
 * ```
 * SELECT count(*)::int AS count
 * FROM graphile_worker.jobs
 * WHERE task_identifier = :task::text
 *   AND attempts < max_attempts
 * ```
 */
export const getOutstandingJobCountForTask = new PreparedQuery<IGetOutstandingJobCountForTaskParams,IGetOutstandingJobCountForTaskResult>(getOutstandingJobCountForTaskIR);


/** 'GetFetchScheduleRules' parameters type */
export type IGetFetchScheduleRulesParams = void;

/** 'GetFetchScheduleRules' return type */
export interface IGetFetchScheduleRulesResult {
  host: string | null;
  outstanding: number | null;
  queue_tail_at: Date | null;
  spacing: number | null;
}

/** 'GetFetchScheduleRules' query type */
export interface IGetFetchScheduleRulesQuery {
  params: IGetFetchScheduleRulesParams;
  result: IGetFetchScheduleRulesResult;
}

const getFetchScheduleRulesIR: any = {"usedParamSet":{},"params":[],"statement":"WITH job_stats AS (\n  SELECT\n    host,\n    count(*) FILTER (WHERE state <> 'failed')::int AS outstanding,\n    max(run_at) FILTER (WHERE state IN ('ready', 'delayed')) AS queue_tail_at\n  FROM crawler.frontier_fetch_job\n  WHERE host IS NOT NULL\n  GROUP BY host\n)\nSELECT\n  COALESCE(r.host, js.host) AS host,\n  (extract(epoch from r.spacing) * 1000)::int as spacing,\n  COALESCE(js.outstanding, 0)::int AS outstanding,\n  GREATEST(js.queue_tail_at, now()) AS queue_tail_at\nFROM crawler.rate_limit_rule r\nFULL JOIN job_stats js USING (host)"};

/**
 * Query generated from SQL:
 * ```
 * WITH job_stats AS (
 *   SELECT
 *     host,
 *     count(*) FILTER (WHERE state <> 'failed')::int AS outstanding,
 *     max(run_at) FILTER (WHERE state IN ('ready', 'delayed')) AS queue_tail_at
 *   FROM crawler.frontier_fetch_job
 *   WHERE host IS NOT NULL
 *   GROUP BY host
 * )
 * SELECT
 *   COALESCE(r.host, js.host) AS host,
 *   (extract(epoch from r.spacing) * 1000)::int as spacing,
 *   COALESCE(js.outstanding, 0)::int AS outstanding,
 *   GREATEST(js.queue_tail_at, now()) AS queue_tail_at
 * FROM crawler.rate_limit_rule r
 * FULL JOIN job_stats js USING (host)
 * ```
 */
export const getFetchScheduleRules = new PreparedQuery<IGetFetchScheduleRulesParams,IGetFetchScheduleRulesResult>(getFetchScheduleRulesIR);


/** 'GetPendingFetch' parameters type */
export interface IGetPendingFetchParams {
  allowRefetch?: boolean | null | void;
  capacity?: NumberOrString | null | void;
}

/** 'GetPendingFetch' return type */
export interface IGetPendingFetchResult {
  federation: string;
  id: string;
  key: string;
  kind: string;
}

/** 'GetPendingFetch' query type */
export interface IGetPendingFetchQuery {
  params: IGetPendingFetchParams;
  result: IGetPendingFetchResult;
}

const getPendingFetchIR: any = {"usedParamSet":{"allowRefetch":true,"capacity":true},"params":[{"name":"allowRefetch","required":false,"transform":{"type":"scalar"},"locs":[{"a":77,"b":89}]},{"name":"capacity","required":false,"transform":{"type":"scalar"},"locs":[{"a":456,"b":464}]}],"statement":"WITH due AS (\n  SELECT id, last_fetched_at\n  FROM crawler.frontier_fetch_due(:allowRefetch::boolean)\n), ranked AS (\n  SELECT\n    f.id, f.federation, f.kind, f.key, due.last_fetched_at,\n    row_number() OVER (\n      PARTITION BY f.federation, f.kind\n      ORDER BY due.last_fetched_at NULLS FIRST\n    ) AS rn\n  FROM due\n  JOIN crawler.frontier f ON f.id = due.id\n)\nSELECT id, federation, kind, key\nFROM ranked\nORDER BY rn, last_fetched_at NULLS FIRST\nLIMIT :capacity"};

/**
 * Query generated from SQL:
 * ```
 * WITH due AS (
 *   SELECT id, last_fetched_at
 *   FROM crawler.frontier_fetch_due(:allowRefetch::boolean)
 * ), ranked AS (
 *   SELECT
 *     f.id, f.federation, f.kind, f.key, due.last_fetched_at,
 *     row_number() OVER (
 *       PARTITION BY f.federation, f.kind
 *       ORDER BY due.last_fetched_at NULLS FIRST
 *     ) AS rn
 *   FROM due
 *   JOIN crawler.frontier f ON f.id = due.id
 * )
 * SELECT id, federation, kind, key
 * FROM ranked
 * ORDER BY rn, last_fetched_at NULLS FIRST
 * LIMIT :capacity
 * ```
 */
export const getPendingFetch = new PreparedQuery<IGetPendingFetchParams,IGetPendingFetchResult>(getPendingFetchIR);


/** 'GetNextPendingProcess' parameters type */
export interface IGetNextPendingProcessParams {
  limit?: NumberOrString | null | void;
}

/** 'GetNextPendingProcess' return type */
export interface IGetNextPendingProcessResult {
  content: Json;
  error: string | null;
  federation: string;
  http_status: number | null;
  id: string;
  key: string;
  kind: string;
  url: string;
}

/** 'GetNextPendingProcess' query type */
export interface IGetNextPendingProcessQuery {
  params: IGetNextPendingProcessParams;
  result: IGetNextPendingProcessResult;
}

const getNextPendingProcessIR: any = {"usedParamSet":{"limit":true},"params":[{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":459,"b":464}]}],"statement":"SELECT f.id, f.federation, f.kind, f.key, jr.url, jr.http_status, jr.error, jrc.content\nFROM crawler.frontier f\nJOIN LATERAL (\n  SELECT jr.*\n  FROM crawler.json_response jr\n  WHERE jr.frontier_id = f.id\n  ORDER BY jr.fetched_at DESC\n  LIMIT 1\n) jr ON true\nJOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash\nWHERE process_status = 'pending'\n  AND fetch_status = 'ok'\nORDER BY last_fetched_at, discovered_at\nFOR UPDATE SKIP LOCKED\nLIMIT :limit"};

/**
 * Query generated from SQL:
 * ```
 * SELECT f.id, f.federation, f.kind, f.key, jr.url, jr.http_status, jr.error, jrc.content
 * FROM crawler.frontier f
 * JOIN LATERAL (
 *   SELECT jr.*
 *   FROM crawler.json_response jr
 *   WHERE jr.frontier_id = f.id
 *   ORDER BY jr.fetched_at DESC
 *   LIMIT 1
 * ) jr ON true
 * JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
 * WHERE process_status = 'pending'
 *   AND fetch_status = 'ok'
 * ORDER BY last_fetched_at, discovered_at
 * FOR UPDATE SKIP LOCKED
 * LIMIT :limit
 * ```
 */
export const getNextPendingProcess = new PreparedQuery<IGetNextPendingProcessParams,IGetNextPendingProcessResult>(getNextPendingProcessIR);


/** 'ReserveRequest' parameters type */
export interface IReserveRequestParams {
  host?: string | null | void;
}

/** 'ReserveRequest' return type */
export interface IReserveRequestResult {
  allowed_at: Date | null;
  granted: boolean | null;
}

/** 'ReserveRequest' query type */
export interface IReserveRequestQuery {
  params: IReserveRequestParams;
  result: IReserveRequestResult;
}

const reserveRequestIR: any = {"usedParamSet":{"host":true},"params":[{"name":"host","required":false,"transform":{"type":"scalar"},"locs":[{"a":58,"b":62}]}],"statement":"SELECT granted, allowed_at\n  FROM crawler.reserve_request(:host::text)"};

/**
 * Query generated from SQL:
 * ```
 * SELECT granted, allowed_at
 *   FROM crawler.reserve_request(:host::text)
 * ```
 */
export const reserveRequest = new PreparedQuery<IReserveRequestParams,IReserveRequestResult>(reserveRequestIR);


/** 'MarkFrontierFetchError' parameters type */
export interface IMarkFrontierFetchErrorParams {
  id?: NumberOrString | null | void;
}

/** 'MarkFrontierFetchError' return type */
export type IMarkFrontierFetchErrorResult = void;

/** 'MarkFrontierFetchError' query type */
export interface IMarkFrontierFetchErrorQuery {
  params: IMarkFrontierFetchErrorParams;
  result: IMarkFrontierFetchErrorResult;
}

const markFrontierFetchErrorIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":274,"b":276}]}],"statement":"UPDATE crawler.frontier\nSET last_fetched_at = now(),\n    fetch_status    = 'error',\n    error_count     = error_count + 1,\n    next_fetch_at   = now() + least(\n      interval '5 minutes',\n      (power(2::numeric, error_count + 1) * 5) * interval '1 second'\n    )\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET last_fetched_at = now(),
 *     fetch_status    = 'error',
 *     error_count     = error_count + 1,
 *     next_fetch_at   = now() + least(
 *       interval '5 minutes',
 *       (power(2::numeric, error_count + 1) * 5) * interval '1 second'
 *     )
 * WHERE id = :id::bigint
 * ```
 */
export const markFrontierFetchError = new PreparedQuery<IMarkFrontierFetchErrorParams,IMarkFrontierFetchErrorResult>(markFrontierFetchErrorIR);


/** 'MarkFrontierTransient' parameters type */
export interface IMarkFrontierTransientParams {
  id?: NumberOrString | null | void;
}

/** 'MarkFrontierTransient' return type */
export type IMarkFrontierTransientResult = void;

/** 'MarkFrontierTransient' query type */
export interface IMarkFrontierTransientQuery {
  params: IMarkFrontierTransientParams;
  result: IMarkFrontierTransientResult;
}

const markFrontierTransientIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":279,"b":281}]}],"statement":"UPDATE crawler.frontier\nSET last_fetched_at = now(),\n    fetch_status    = 'transient',\n    error_count     = error_count + 1,\n    next_fetch_at   = now() + least(\n      interval '30 minutes',\n      (power(2::numeric, error_count + 1) * 5) * interval '1 second'\n    )\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET last_fetched_at = now(),
 *     fetch_status    = 'transient',
 *     error_count     = error_count + 1,
 *     next_fetch_at   = now() + least(
 *       interval '30 minutes',
 *       (power(2::numeric, error_count + 1) * 5) * interval '1 second'
 *     )
 * WHERE id = :id::bigint
 * ```
 */
export const markFrontierTransient = new PreparedQuery<IMarkFrontierTransientParams,IMarkFrontierTransientResult>(markFrontierTransientIR);


/** 'MarkFrontierFetchSuccess' parameters type */
export interface IMarkFrontierFetchSuccessParams {
  fetchStatus?: fetch_status | null | void;
  id?: NumberOrString | null | void;
  revalidatePeriod?: DateOrString | null | void;
}

/** 'MarkFrontierFetchSuccess' return type */
export type IMarkFrontierFetchSuccessResult = void;

/** 'MarkFrontierFetchSuccess' query type */
export interface IMarkFrontierFetchSuccessQuery {
  params: IMarkFrontierFetchSuccessParams;
  result: IMarkFrontierFetchSuccessResult;
}

const markFrontierFetchSuccessIR: any = {"usedParamSet":{"fetchStatus":true,"revalidatePeriod":true,"id":true},"params":[{"name":"fetchStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":72,"b":83},{"a":145,"b":156}]},{"name":"revalidatePeriod","required":false,"transform":{"type":"scalar"},"locs":[{"a":419,"b":435}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":458,"b":460}]}],"statement":"UPDATE crawler.frontier\nSET last_fetched_at = now(),\n    fetch_status = :fetchStatus::crawler.fetch_status,\n    process_status = CASE\n      WHEN :fetchStatus::crawler.fetch_status = 'gone'::crawler.fetch_status\n      THEN 'ok'::crawler.process_status\n      ELSE 'pending'::crawler.process_status\n    END,\n    last_process_error = NULL,\n    last_process_error_at = NULL,\n    error_count = 0,\n    next_fetch_at = now() + :revalidatePeriod::interval\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET last_fetched_at = now(),
 *     fetch_status = :fetchStatus::crawler.fetch_status,
 *     process_status = CASE
 *       WHEN :fetchStatus::crawler.fetch_status = 'gone'::crawler.fetch_status
 *       THEN 'ok'::crawler.process_status
 *       ELSE 'pending'::crawler.process_status
 *     END,
 *     last_process_error = NULL,
 *     last_process_error_at = NULL,
 *     error_count = 0,
 *     next_fetch_at = now() + :revalidatePeriod::interval
 * WHERE id = :id::bigint
 * ```
 */
export const markFrontierFetchSuccess = new PreparedQuery<IMarkFrontierFetchSuccessParams,IMarkFrontierFetchSuccessResult>(markFrontierFetchSuccessIR);


/** 'MarkFrontiersProcessSuccess' parameters type */
export interface IMarkFrontiersProcessSuccessParams {
  ids?: NumberOrStringArray | null | void;
}

/** 'MarkFrontiersProcessSuccess' return type */
export type IMarkFrontiersProcessSuccessResult = void;

/** 'MarkFrontiersProcessSuccess' query type */
export interface IMarkFrontiersProcessSuccessQuery {
  params: IMarkFrontiersProcessSuccessParams;
  result: IMarkFrontiersProcessSuccessResult;
}

const markFrontiersProcessSuccessIR: any = {"usedParamSet":{"ids":true},"params":[{"name":"ids","required":false,"transform":{"type":"scalar"},"locs":[{"a":130,"b":133}]}],"statement":"UPDATE crawler.frontier\nSET process_status = 'ok',\n    last_process_error = NULL,\n    last_process_error_at = NULL\nWHERE id = ANY(:ids::bigint[])"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET process_status = 'ok',
 *     last_process_error = NULL,
 *     last_process_error_at = NULL
 * WHERE id = ANY(:ids::bigint[])
 * ```
 */
export const markFrontiersProcessSuccess = new PreparedQuery<IMarkFrontiersProcessSuccessParams,IMarkFrontiersProcessSuccessResult>(markFrontiersProcessSuccessIR);


/** 'MarkFrontierProcessError' parameters type */
export interface IMarkFrontierProcessErrorParams {
  error?: string | null | void;
  id?: NumberOrString | null | void;
}

/** 'MarkFrontierProcessError' return type */
export type IMarkFrontierProcessErrorResult = void;

/** 'MarkFrontierProcessError' query type */
export interface IMarkFrontierProcessErrorQuery {
  params: IMarkFrontierProcessErrorParams;
  result: IMarkFrontierProcessErrorResult;
}

const markFrontierProcessErrorIR: any = {"usedParamSet":{"error":true,"id":true},"params":[{"name":"error","required":false,"transform":{"type":"scalar"},"locs":[{"a":79,"b":84}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":132,"b":134}]}],"statement":"UPDATE crawler.frontier\nSET process_status = 'error',\n    last_process_error = :error,\n    last_process_error_at = now()\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET process_status = 'error',
 *     last_process_error = :error,
 *     last_process_error_at = now()
 * WHERE id = :id::bigint
 * ```
 */
export const markFrontierProcessError = new PreparedQuery<IMarkFrontierProcessErrorParams,IMarkFrontierProcessErrorResult>(markFrontierProcessErrorIR);


/** 'RescheduleFrontier' parameters type */
export interface IRescheduleFrontierParams {
  id?: NumberOrString | null | void;
  nextRetryAt?: DateOrString | null | void;
}

/** 'RescheduleFrontier' return type */
export type IRescheduleFrontierResult = void;

/** 'RescheduleFrontier' query type */
export interface IRescheduleFrontierQuery {
  params: IRescheduleFrontierParams;
  result: IRescheduleFrontierResult;
}

const rescheduleFrontierIR: any = {"usedParamSet":{"nextRetryAt":true,"id":true},"params":[{"name":"nextRetryAt","required":false,"transform":{"type":"scalar"},"locs":[{"a":44,"b":55}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":68,"b":70}]}],"statement":"UPDATE crawler.frontier SET next_fetch_at = :nextRetryAt WHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier SET next_fetch_at = :nextRetryAt WHERE id = :id::bigint
 * ```
 */
export const rescheduleFrontier = new PreparedQuery<IRescheduleFrontierParams,IRescheduleFrontierResult>(rescheduleFrontierIR);


/** 'GetFrontierRefetchTarget' parameters type */
export interface IGetFrontierRefetchTargetParams {
  federation?: string | null | void;
  key?: string | null | void;
  kind?: string | null | void;
}

/** 'GetFrontierRefetchTarget' return type */
export interface IGetFrontierRefetchTargetResult {
  federation: string;
  id: string;
  key: string;
  kind: string;
}

/** 'GetFrontierRefetchTarget' query type */
export interface IGetFrontierRefetchTargetQuery {
  params: IGetFrontierRefetchTargetParams;
  result: IGetFrontierRefetchTargetResult;
}

const getFrontierRefetchTargetIR: any = {"usedParamSet":{"federation":true,"kind":true,"key":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":74,"b":84}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":99,"b":103}]},{"name":"key","required":false,"transform":{"type":"scalar"},"locs":[{"a":117,"b":120}]}],"statement":"SELECT id, federation, kind, key\nFROM crawler.frontier\nWHERE federation = :federation\n  AND kind = :kind\n  AND key = :key"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id, federation, kind, key
 * FROM crawler.frontier
 * WHERE federation = :federation
 *   AND kind = :kind
 *   AND key = :key
 * ```
 */
export const getFrontierRefetchTarget = new PreparedQuery<IGetFrontierRefetchTargetParams,IGetFrontierRefetchTargetResult>(getFrontierRefetchTargetIR);


/** 'QueueFrontierRefetch' parameters type */
export interface IQueueFrontierRefetchParams {
  id?: NumberOrString | null | void;
}

/** 'QueueFrontierRefetch' return type */
export interface IQueueFrontierRefetchResult {
  federation: string | null;
  id: string | null;
  job_id: string | null;
  key: string | null;
  kind: string | null;
}

/** 'QueueFrontierRefetch' query type */
export interface IQueueFrontierRefetchQuery {
  params: IQueueFrontierRefetchParams;
  result: IQueueFrontierRefetchResult;
}

const queueFrontierRefetchIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":83,"b":85}]}],"statement":"SELECT id, federation, kind, key, job_id\nFROM crawler.queue_frontier_refetch(ARRAY[:id::bigint]::bigint[])"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id, federation, kind, key, job_id
 * FROM crawler.queue_frontier_refetch(ARRAY[:id::bigint]::bigint[])
 * ```
 */
export const queueFrontierRefetch = new PreparedQuery<IQueueFrontierRefetchParams,IQueueFrontierRefetchResult>(queueFrontierRefetchIR);


/** 'GetFailedFrontierRefetchTargets' parameters type */
export interface IGetFailedFrontierRefetchTargetsParams {
  errorContains?: string | null | void;
  federation?: string | null | void;
  httpStatuses?: numberArray | null | void;
  key?: string | null | void;
  kind?: string | null | void;
  limit?: number | null | void;
}

/** 'GetFailedFrontierRefetchTargets' return type */
export interface IGetFailedFrontierRefetchTargetsResult {
  error: string | null;
  error_count: number | null;
  failed_at: Date | null;
  federation: string | null;
  fetch_status: fetch_status | null;
  http_status: number | null;
  id: string | null;
  key: string | null;
  kind: string | null;
  next_fetch_at: Date | null;
  process_status: process_status | null;
  url: string | null;
}

/** 'GetFailedFrontierRefetchTargets' query type */
export interface IGetFailedFrontierRefetchTargetsQuery {
  params: IGetFailedFrontierRefetchTargetsParams;
  result: IGetFailedFrontierRefetchTargetsResult;
}

const getFailedFrontierRefetchTargetsIR: any = {"usedParamSet":{"federation":true,"kind":true,"key":true,"httpStatuses":true,"errorContains":true,"limit":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":202,"b":212}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":223,"b":227}]},{"name":"key","required":false,"transform":{"type":"scalar"},"locs":[{"a":238,"b":241}]},{"name":"httpStatuses","required":false,"transform":{"type":"scalar"},"locs":[{"a":252,"b":264}]},{"name":"errorContains","required":false,"transform":{"type":"scalar"},"locs":[{"a":276,"b":289}]},{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":300,"b":305}]}],"statement":"SELECT\n  id,\n  federation,\n  kind,\n  key,\n  fetch_status,\n  process_status,\n  error_count,\n  next_fetch_at,\n  failed_at,\n  url,\n  http_status,\n  error\nFROM crawler.failed_frontier_refetch_candidates(\n  :federation::text,\n  :kind::text,\n  :key::text,\n  :httpStatuses::int[],\n  :errorContains::text,\n  :limit::int\n)\nORDER BY failed_at DESC, id DESC"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   id,
 *   federation,
 *   kind,
 *   key,
 *   fetch_status,
 *   process_status,
 *   error_count,
 *   next_fetch_at,
 *   failed_at,
 *   url,
 *   http_status,
 *   error
 * FROM crawler.failed_frontier_refetch_candidates(
 *   :federation::text,
 *   :kind::text,
 *   :key::text,
 *   :httpStatuses::int[],
 *   :errorContains::text,
 *   :limit::int
 * )
 * ORDER BY failed_at DESC, id DESC
 * ```
 */
export const getFailedFrontierRefetchTargets = new PreparedQuery<IGetFailedFrontierRefetchTargetsParams,IGetFailedFrontierRefetchTargetsResult>(getFailedFrontierRefetchTargetsIR);


/** 'QueueFailedFrontierRefetch' parameters type */
export interface IQueueFailedFrontierRefetchParams {
  errorContains?: string | null | void;
  federation?: string | null | void;
  httpStatuses?: numberArray | null | void;
  key?: string | null | void;
  kind?: string | null | void;
  limit?: number | null | void;
}

/** 'QueueFailedFrontierRefetch' return type */
export interface IQueueFailedFrontierRefetchResult {
  error: string | null;
  failed_at: Date | null;
  federation: string | null;
  http_status: number | null;
  id: string | null;
  job_id: string | null;
  key: string | null;
  kind: string | null;
}

/** 'QueueFailedFrontierRefetch' query type */
export interface IQueueFailedFrontierRefetchQuery {
  params: IQueueFailedFrontierRefetchParams;
  result: IQueueFailedFrontierRefetchResult;
}

const queueFailedFrontierRefetchIR: any = {"usedParamSet":{"federation":true,"kind":true,"key":true,"httpStatuses":true,"errorContains":true,"limit":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":87,"b":97}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":110,"b":114}]},{"name":"key","required":false,"transform":{"type":"scalar"},"locs":[{"a":127,"b":130}]},{"name":"httpStatuses","required":false,"transform":{"type":"scalar"},"locs":[{"a":143,"b":155}]},{"name":"errorContains","required":false,"transform":{"type":"scalar"},"locs":[{"a":169,"b":182}]},{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":195,"b":200}]}],"statement":"WITH candidates AS (\n  SELECT *\n  FROM crawler.failed_frontier_refetch_candidates(\n    :federation::text,\n    :kind::text,\n    :key::text,\n    :httpStatuses::int[],\n    :errorContains::text,\n    :limit::int\n  )\n), queued AS (\n  SELECT *\n  FROM crawler.queue_frontier_refetch(ARRAY(SELECT id FROM candidates)::bigint[])\n)\nSELECT\n  q.id,\n  q.federation,\n  q.kind,\n  q.key,\n  c.failed_at,\n  c.http_status,\n  c.error,\n  q.job_id\nFROM queued q\nJOIN candidates c USING (id)\nORDER BY c.failed_at DESC, q.id DESC"};

/**
 * Query generated from SQL:
 * ```
 * WITH candidates AS (
 *   SELECT *
 *   FROM crawler.failed_frontier_refetch_candidates(
 *     :federation::text,
 *     :kind::text,
 *     :key::text,
 *     :httpStatuses::int[],
 *     :errorContains::text,
 *     :limit::int
 *   )
 * ), queued AS (
 *   SELECT *
 *   FROM crawler.queue_frontier_refetch(ARRAY(SELECT id FROM candidates)::bigint[])
 * )
 * SELECT
 *   q.id,
 *   q.federation,
 *   q.kind,
 *   q.key,
 *   c.failed_at,
 *   c.http_status,
 *   c.error,
 *   q.job_id
 * FROM queued q
 * JOIN candidates c USING (id)
 * ORDER BY c.failed_at DESC, q.id DESC
 * ```
 */
export const queueFailedFrontierRefetch = new PreparedQuery<IQueueFailedFrontierRefetchParams,IQueueFailedFrontierRefetchResult>(queueFailedFrontierRefetchIR);


/** 'InsertResponse' parameters type */
export interface IInsertResponseParams {
  content?: string | null | void;
  error?: string | null | void;
  httpStatus?: number | null | void;
  id?: NumberOrString | null | void;
  url?: string | null | void;
}

/** 'InsertResponse' return type */
export type IInsertResponseResult = void;

/** 'InsertResponse' query type */
export interface IInsertResponseQuery {
  params: IInsertResponseParams;
  result: IInsertResponseResult;
}

const insertResponseIR: any = {"usedParamSet":{"content":true,"id":true,"url":true,"httpStatus":true,"error":true},"params":[{"name":"content","required":false,"transform":{"type":"scalar"},"locs":[{"a":27,"b":34}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":334,"b":336}]},{"name":"url","required":false,"transform":{"type":"scalar"},"locs":[{"a":339,"b":342}]},{"name":"httpStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":345,"b":355}]},{"name":"error","required":false,"transform":{"type":"scalar"},"locs":[{"a":358,"b":363}]}],"statement":"WITH payload AS (\n  SELECT :content::text::jsonb AS content\n), ins_cache AS (\n  INSERT INTO crawler.json_response_cache (content)\n    SELECT content\n    FROM payload\n    WHERE content IS NOT NULL\n    ON CONFLICT (content_hash) DO NOTHING\n)\nINSERT INTO crawler.json_response (frontier_id, url, http_status, error, content_hash)\nSELECT :id, :url, :httpStatus, :error,\n       case when content IS NULL then NULL else encode(digest(content::text, 'sha256'), 'hex') end AS content_hash\nFROM payload"};

/**
 * Query generated from SQL:
 * ```
 * WITH payload AS (
 *   SELECT :content::text::jsonb AS content
 * ), ins_cache AS (
 *   INSERT INTO crawler.json_response_cache (content)
 *     SELECT content
 *     FROM payload
 *     WHERE content IS NOT NULL
 *     ON CONFLICT (content_hash) DO NOTHING
 * )
 * INSERT INTO crawler.json_response (frontier_id, url, http_status, error, content_hash)
 * SELECT :id, :url, :httpStatus, :error,
 *        case when content IS NULL then NULL else encode(digest(content::text, 'sha256'), 'hex') end AS content_hash
 * FROM payload
 * ```
 */
export const insertResponse = new PreparedQuery<IInsertResponseParams,IInsertResponseResult>(insertResponseIR);


/** 'UpsertFrontier' parameters type */
export interface IUpsertFrontierParams {
  federation?: string | null | void;
  key?: string | null | void;
  kind?: string | null | void;
}

/** 'UpsertFrontier' return type */
export type IUpsertFrontierResult = void;

/** 'UpsertFrontier' query type */
export interface IUpsertFrontierQuery {
  params: IUpsertFrontierParams;
  result: IUpsertFrontierResult;
}

const upsertFrontierIR: any = {"usedParamSet":{"federation":true,"kind":true,"key":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":61,"b":71}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":74,"b":78}]},{"name":"key","required":false,"transform":{"type":"scalar"},"locs":[{"a":81,"b":84}]}],"statement":"INSERT INTO crawler.frontier (federation, kind, key)\nVALUES (:federation, :kind, :key)\nON CONFLICT (federation, kind, key) DO NOTHING"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO crawler.frontier (federation, kind, key)
 * VALUES (:federation, :kind, :key)
 * ON CONFLICT (federation, kind, key) DO NOTHING
 * ```
 */
export const upsertFrontier = new PreparedQuery<IUpsertFrontierParams,IUpsertFrontierResult>(upsertFrontierIR);


/** 'UpsertFrontierKeys' parameters type */
export interface IUpsertFrontierKeysParams {
  federation?: string | null | void;
  keys?: stringArray | null | void;
  kind?: string | null | void;
}

/** 'UpsertFrontierKeys' return type */
export type IUpsertFrontierKeysResult = void;

/** 'UpsertFrontierKeys' query type */
export interface IUpsertFrontierKeysQuery {
  params: IUpsertFrontierKeysParams;
  result: IUpsertFrontierKeysResult;
}

const upsertFrontierKeysIR: any = {"usedParamSet":{"federation":true,"kind":true,"keys":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":60,"b":70}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":73,"b":77}]},{"name":"keys","required":false,"transform":{"type":"scalar"},"locs":[{"a":96,"b":100}]}],"statement":"INSERT INTO crawler.frontier (federation, kind, key)\nSELECT :federation, :kind, key\nFROM unnest(:keys::text[]) as input(key)\nON CONFLICT DO NOTHING"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO crawler.frontier (federation, kind, key)
 * SELECT :federation, :kind, key
 * FROM unnest(:keys::text[]) as input(key)
 * ON CONFLICT DO NOTHING
 * ```
 */
export const upsertFrontierKeys = new PreparedQuery<IUpsertFrontierKeysParams,IUpsertFrontierKeysResult>(upsertFrontierKeysIR);


