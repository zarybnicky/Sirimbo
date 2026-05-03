/** Types generated for queries found in "crawler/crawler.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type fetch_status = 'error' | 'gone' | 'ok' | 'pending' | 'transient';

export type DateOrString = Date | string;

export type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

export type NumberOrString = number | string;

export type NumberOrStringArray = (NumberOrString)[];

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
  responses: number | null;
  total: number | null;
}

/** 'GetFrontierKindStatus' query type */
export interface IGetFrontierKindStatusQuery {
  params: IGetFrontierKindStatusParams;
  result: IGetFrontierKindStatusResult;
}

const getFrontierKindStatusIR: any = {"usedParamSet":{"federation":true,"kind":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":277,"b":287},{"a":321,"b":331}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":343,"b":347},{"a":375,"b":379}]}],"statement":"WITH frontiers AS (\n  SELECT\n    f.*,\n    row_number() OVER (\n      PARTITION BY f.federation, f.kind\n      ORDER BY\n        CASE WHEN f.fetch_status = 'pending' THEN 0 ELSE 1 END,\n        f.discovered_at DESC,\n        f.id\n    ) AS key_rank\n  FROM crawler.frontier f\n  WHERE (:federation::text IS NULL OR f.federation = :federation)\n    AND (:kind::text IS NULL OR f.kind = :kind)\n), response_counts AS (\n  SELECT\n    f.federation,\n    f.kind,\n    count(*)::int AS responses,\n    max(r.fetched_at) AS latest_response\n  FROM frontiers f\n  JOIN crawler.json_response r ON r.frontier_id = f.id\n  GROUP BY f.federation, f.kind\n), status AS (\n  SELECT\n    federation,\n    kind,\n    count(*)::int AS total,\n    coalesce(\n      string_agg(key, ', ' ORDER BY key_rank) FILTER (WHERE key <> '' AND key_rank <= 3)\n        || CASE WHEN count(*) FILTER (WHERE key <> '') > 3 THEN ', ...' ELSE '' END,\n      '-'\n    ) AS keys,\n    count(*) FILTER (WHERE fetch_status = 'pending')::int AS fetch_pending,\n    count(*) FILTER (WHERE fetch_status = 'transient')::int AS fetch_transient,\n    count(*) FILTER (WHERE fetch_status = 'ok')::int AS fetch_ok,\n    count(*) FILTER (WHERE fetch_status = 'gone')::int AS fetch_gone,\n    count(*) FILTER (WHERE fetch_status = 'error')::int AS fetch_error,\n    count(*) FILTER (WHERE process_status = 'pending')::int AS process_pending,\n    count(*) FILTER (WHERE process_status = 'ok')::int AS process_ok,\n    count(*) FILTER (WHERE process_status = 'error')::int AS process_error,\n    count(*) FILTER (WHERE next_fetch_at IS NULL OR next_fetch_at <= now())::int AS fetch_due,\n    max(last_fetched_at) AS latest_fetch\n  FROM frontiers f\n  GROUP BY federation, kind\n)\nSELECT\n  s.*,\n  coalesce(rc.responses, 0)::int AS responses,\n  coalesce(s.latest_fetch, rc.latest_response) AS latest\nFROM status s\nLEFT JOIN response_counts rc ON rc.federation = s.federation AND rc.kind = s.kind\nORDER BY s.federation, s.kind"};

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
 *     count(*)::int AS responses,
 *     max(r.fetched_at) AS latest_response
 *   FROM frontiers f
 *   JOIN crawler.json_response r ON r.frontier_id = f.id
 *   GROUP BY f.federation, f.kind
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
 *     count(*) FILTER (WHERE next_fetch_at IS NULL OR next_fetch_at <= now())::int AS fetch_due,
 *     max(last_fetched_at) AS latest_fetch
 *   FROM frontiers f
 *   GROUP BY federation, kind
 * )
 * SELECT
 *   s.*,
 *   coalesce(rc.responses, 0)::int AS responses,
 *   coalesce(s.latest_fetch, rc.latest_response) AS latest
 * FROM status s
 * LEFT JOIN response_counts rc ON rc.federation = s.federation AND rc.kind = s.kind
 * ORDER BY s.federation, s.kind
 * ```
 */
export const getFrontierKindStatus = new PreparedQuery<IGetFrontierKindStatusParams,IGetFrontierKindStatusResult>(getFrontierKindStatusIR);


/** 'GetLatestFrontierResponses' parameters type */
export interface IGetLatestFrontierResponsesParams {
  federation?: string | null | void;
  kind?: string | null | void;
}

/** 'GetLatestFrontierResponses' return type */
export interface IGetLatestFrontierResponsesResult {
  content: Json;
  http_status: number | null;
  id: string;
  url: string;
}

/** 'GetLatestFrontierResponses' query type */
export interface IGetLatestFrontierResponsesQuery {
  params: IGetLatestFrontierResponsesParams;
  result: IGetLatestFrontierResponsesResult;
}

const getLatestFrontierResponsesIR: any = {"usedParamSet":{"federation":true,"kind":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":315,"b":325}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":340,"b":344}]}],"statement":"SELECT f.id, jr.url, jr.http_status, jrc.content\nFROM crawler.frontier f\nJOIN LATERAL (\n  SELECT jr.*\n  FROM crawler.json_response jr\n  WHERE jr.frontier_id = f.id\n  ORDER BY jr.fetched_at DESC\n  LIMIT 1\n  ) jr ON true\nJOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash\nWHERE f.federation = :federation AND f.kind = :kind AND f.fetch_status in ('ok', 'pending')"};

/**
 * Query generated from SQL:
 * ```
 * SELECT f.id, jr.url, jr.http_status, jrc.content
 * FROM crawler.frontier f
 * JOIN LATERAL (
 *   SELECT jr.*
 *   FROM crawler.json_response jr
 *   WHERE jr.frontier_id = f.id
 *   ORDER BY jr.fetched_at DESC
 *   LIMIT 1
 *   ) jr ON true
 * JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
 * WHERE f.federation = :federation AND f.kind = :kind AND f.fetch_status in ('ok', 'pending')
 * ```
 */
export const getLatestFrontierResponses = new PreparedQuery<IGetLatestFrontierResponsesParams,IGetLatestFrontierResponsesResult>(getLatestFrontierResponsesIR);


/** 'GetJobCountForTask' parameters type */
export interface IGetJobCountForTaskParams {
  task?: string | null | void;
}

/** 'GetJobCountForTask' return type */
export interface IGetJobCountForTaskResult {
  count: number | null;
}

/** 'GetJobCountForTask' query type */
export interface IGetJobCountForTaskQuery {
  params: IGetJobCountForTaskParams;
  result: IGetJobCountForTaskResult;
}

const getJobCountForTaskIR: any = {"usedParamSet":{"task":true},"params":[{"name":"task","required":false,"transform":{"type":"scalar"},"locs":[{"a":80,"b":84}]}],"statement":"SELECT count(*)::int AS count\nFROM graphile_worker.jobs\nWHERE task_identifier = :task::text"};

/**
 * Query generated from SQL:
 * ```
 * SELECT count(*)::int AS count
 * FROM graphile_worker.jobs
 * WHERE task_identifier = :task::text
 * ```
 */
export const getJobCountForTask = new PreparedQuery<IGetJobCountForTaskParams,IGetJobCountForTaskResult>(getJobCountForTaskIR);


/** 'GetFetchScheduleRules' parameters type */
export type IGetFetchScheduleRulesParams = void;

/** 'GetFetchScheduleRules' return type */
export interface IGetFetchScheduleRulesResult {
  host: string | null;
  last_run_at: Date | null;
  queued: number | null;
  spacing: number | null;
}

/** 'GetFetchScheduleRules' query type */
export interface IGetFetchScheduleRulesQuery {
  params: IGetFetchScheduleRulesParams;
  result: IGetFetchScheduleRulesResult;
}

const getFetchScheduleRulesIR: any = {"usedParamSet":{},"params":[],"statement":"WITH job_stats AS (\n  SELECT\n    split_part(key, ':', 2) AS host,\n    count(*)                AS queued,\n    max(run_at)             AS last_run_at\n  FROM graphile_worker.jobs\n  WHERE task_identifier = 'frontier_fetch'\n    AND key LIKE 'fetch:%'\n    AND run_at >= now()\n    AND locked_at IS NULL\n  GROUP BY split_part(key, ':', 2)\n)\nSELECT\n  COALESCE(r.host, js.host) AS host,\n  (extract(epoch from r.spacing) * 1000)::int as spacing,\n  COALESCE(js.queued, 0)::int AS queued,\n  GREATEST(js.last_run_at, now()) AS last_run_at\nFROM crawler.rate_limit_rule r\nFULL JOIN job_stats js USING (host)"};

/**
 * Query generated from SQL:
 * ```
 * WITH job_stats AS (
 *   SELECT
 *     split_part(key, ':', 2) AS host,
 *     count(*)                AS queued,
 *     max(run_at)             AS last_run_at
 *   FROM graphile_worker.jobs
 *   WHERE task_identifier = 'frontier_fetch'
 *     AND key LIKE 'fetch:%'
 *     AND run_at >= now()
 *     AND locked_at IS NULL
 *   GROUP BY split_part(key, ':', 2)
 * )
 * SELECT
 *   COALESCE(r.host, js.host) AS host,
 *   (extract(epoch from r.spacing) * 1000)::int as spacing,
 *   COALESCE(js.queued, 0)::int AS queued,
 *   GREATEST(js.last_run_at, now()) AS last_run_at
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

const getPendingFetchIR: any = {"usedParamSet":{"allowRefetch":true,"capacity":true},"params":[{"name":"allowRefetch","required":false,"transform":{"type":"scalar"},"locs":[{"a":214,"b":226}]},{"name":"capacity","required":false,"transform":{"type":"scalar"},"locs":[{"a":577,"b":585}]}],"statement":"WITH eligible AS (\n  SELECT id, federation, kind, key, last_fetched_at\n  FROM crawler.frontier\n  WHERE (next_fetch_at IS NULL OR next_fetch_at <= now())\n    AND (fetch_status IN ('pending', 'transient')\n       OR (:allowRefetch AND fetch_status = 'ok' AND process_status = 'ok'))\n), ranked AS (\n  SELECT\n    id, federation, kind, key, last_fetched_at,\n    row_number() OVER (\n      PARTITION BY federation, kind\n      ORDER BY last_fetched_at NULLS FIRST\n    ) AS rn\n  FROM eligible\n)\nSELECT id, federation, kind, key\nFROM ranked\nORDER BY rn, last_fetched_at NULLS FIRST\nLIMIT :capacity"};

/**
 * Query generated from SQL:
 * ```
 * WITH eligible AS (
 *   SELECT id, federation, kind, key, last_fetched_at
 *   FROM crawler.frontier
 *   WHERE (next_fetch_at IS NULL OR next_fetch_at <= now())
 *     AND (fetch_status IN ('pending', 'transient')
 *        OR (:allowRefetch AND fetch_status = 'ok' AND process_status = 'ok'))
 * ), ranked AS (
 *   SELECT
 *     id, federation, kind, key, last_fetched_at,
 *     row_number() OVER (
 *       PARTITION BY federation, kind
 *       ORDER BY last_fetched_at NULLS FIRST
 *     ) AS rn
 *   FROM eligible
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

const getNextPendingProcessIR: any = {"usedParamSet":{"limit":true},"params":[{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":470,"b":475}]}],"statement":"SELECT f.id, f.federation, f.kind, f.key, jr.url, jr.http_status, jr.error, jrc.content\nFROM crawler.frontier f\nJOIN LATERAL (\n  SELECT jr.*\n  FROM crawler.json_response jr\n  WHERE jr.frontier_id = f.id\n  ORDER BY jr.fetched_at DESC\n  LIMIT 1\n) jr ON true\nJOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash\nWHERE process_status = 'pending'\n  AND fetch_status IN ('ok', 'gone')\nORDER BY last_fetched_at, discovered_at\nFOR UPDATE SKIP LOCKED\nLIMIT :limit"};

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
 *   AND fetch_status IN ('ok', 'gone')
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

const markFrontierFetchSuccessIR: any = {"usedParamSet":{"fetchStatus":true,"revalidatePeriod":true,"id":true},"params":[{"name":"fetchStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":72,"b":83}]},{"name":"revalidatePeriod","required":false,"transform":{"type":"scalar"},"locs":[{"a":167,"b":183}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":206,"b":208}]}],"statement":"UPDATE crawler.frontier\nSET last_fetched_at = now(),\n    fetch_status = :fetchStatus,\n    process_status = 'pending',\n    error_count = 0,\n    next_fetch_at = now() + :revalidatePeriod::interval\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET last_fetched_at = now(),
 *     fetch_status = :fetchStatus,
 *     process_status = 'pending',
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

const markFrontiersProcessSuccessIR: any = {"usedParamSet":{"ids":true},"params":[{"name":"ids","required":false,"transform":{"type":"scalar"},"locs":[{"a":65,"b":68}]}],"statement":"UPDATE crawler.frontier\nSET process_status = 'ok'\nWHERE id = ANY(:ids::bigint[])"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET process_status = 'ok'
 * WHERE id = ANY(:ids::bigint[])
 * ```
 */
export const markFrontiersProcessSuccess = new PreparedQuery<IMarkFrontiersProcessSuccessParams,IMarkFrontiersProcessSuccessResult>(markFrontiersProcessSuccessIR);


/** 'MarkFrontierProcessError' parameters type */
export interface IMarkFrontierProcessErrorParams {
  id?: NumberOrString | null | void;
}

/** 'MarkFrontierProcessError' return type */
export type IMarkFrontierProcessErrorResult = void;

/** 'MarkFrontierProcessError' query type */
export interface IMarkFrontierProcessErrorQuery {
  params: IMarkFrontierProcessErrorParams;
  result: IMarkFrontierProcessErrorResult;
}

const markFrontierProcessErrorIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":64,"b":66}]}],"statement":"UPDATE crawler.frontier SET process_status = 'error' WHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier SET process_status = 'error' WHERE id = :id::bigint
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


