/** Types generated for queries found in "crawler/crawler.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type fetch_status = 'error' | 'gone' | 'ok' | 'pending';

export type process_status = 'error' | 'ok' | 'pending';

export type DateOrString = Date | string;

export type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

export type NumberOrString = number | string;

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


/** 'GetLatestFrontierJsonResponses' parameters type */
export interface IGetLatestFrontierJsonResponsesParams {
  federation?: string | null | void;
  kind?: string | null | void;
}

/** 'GetLatestFrontierJsonResponses' return type */
export interface IGetLatestFrontierJsonResponsesResult {
  content: Json;
  error: string | null;
  http_status: number | null;
  id: string;
  url: string;
}

/** 'GetLatestFrontierJsonResponses' query type */
export interface IGetLatestFrontierJsonResponsesQuery {
  params: IGetLatestFrontierJsonResponsesParams;
  result: IGetLatestFrontierJsonResponsesResult;
}

const getLatestFrontierJsonResponsesIR: any = {"usedParamSet":{"federation":true,"kind":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":325,"b":335}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":350,"b":354}]}],"statement":"SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content\nFROM crawler.frontier f\nJOIN LATERAL (\n  SELECT jr.*\n  FROM crawler.json_response jr\n  WHERE jr.frontier_id = f.id\n  ORDER BY jr.fetched_at DESC\n  LIMIT 1\n  ) jr ON true\nJOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash\nWHERE f.federation = :federation AND f.kind = :kind"};

/**
 * Query generated from SQL:
 * ```
 * SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content
 * FROM crawler.frontier f
 * JOIN LATERAL (
 *   SELECT jr.*
 *   FROM crawler.json_response jr
 *   WHERE jr.frontier_id = f.id
 *   ORDER BY jr.fetched_at DESC
 *   LIMIT 1
 *   ) jr ON true
 * JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
 * WHERE f.federation = :federation AND f.kind = :kind
 * ```
 */
export const getLatestFrontierJsonResponses = new PreparedQuery<IGetLatestFrontierJsonResponsesParams,IGetLatestFrontierJsonResponsesResult>(getLatestFrontierJsonResponsesIR);


/** 'GetFrontierJsonResponse' parameters type */
export interface IGetFrontierJsonResponseParams {
  id?: NumberOrString | null | void;
}

/** 'GetFrontierJsonResponse' return type */
export interface IGetFrontierJsonResponseResult {
  content: Json;
  error: string | null;
  http_status: number | null;
  id: string;
  url: string;
}

/** 'GetFrontierJsonResponse' query type */
export interface IGetFrontierJsonResponseQuery {
  params: IGetFrontierJsonResponseParams;
  result: IGetFrontierJsonResponseResult;
}

const getFrontierJsonResponseIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":315,"b":317}]}],"statement":"SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content\nFROM crawler.frontier f\nJOIN LATERAL (\n  SELECT jr.*\n  FROM crawler.json_response jr\n  WHERE jr.frontier_id = f.id\n  ORDER BY jr.fetched_at DESC\n  LIMIT 1\n) jr ON true\nJOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash\nWHERE f.id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content
 * FROM crawler.frontier f
 * JOIN LATERAL (
 *   SELECT jr.*
 *   FROM crawler.json_response jr
 *   WHERE jr.frontier_id = f.id
 *   ORDER BY jr.fetched_at DESC
 *   LIMIT 1
 * ) jr ON true
 * JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
 * WHERE f.id = :id::bigint
 * ```
 */
export const getFrontierJsonResponse = new PreparedQuery<IGetFrontierJsonResponseParams,IGetFrontierJsonResponseResult>(getFrontierJsonResponseIR);


/** 'GetFrontierHtmlResponse' parameters type */
export interface IGetFrontierHtmlResponseParams {
  id?: NumberOrString | null | void;
}

/** 'GetFrontierHtmlResponse' return type */
export interface IGetFrontierHtmlResponseResult {
  content: string;
  error: string | null;
  http_status: number | null;
  id: string;
  url: string;
}

/** 'GetFrontierHtmlResponse' query type */
export interface IGetFrontierHtmlResponseQuery {
  params: IGetFrontierHtmlResponseParams;
  result: IGetFrontierHtmlResponseResult;
}

const getFrontierHtmlResponseIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":315,"b":317}]}],"statement":"SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content\nFROM crawler.frontier f\nJOIN LATERAL (\n  SELECT jr.*\n  FROM crawler.html_response jr\n  WHERE jr.frontier_id = f.id\n  ORDER BY jr.fetched_at DESC\n  LIMIT 1\n) jr ON true\nJOIN crawler.html_response_cache jrc ON jr.content_hash = jrc.content_hash\nWHERE f.id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content
 * FROM crawler.frontier f
 * JOIN LATERAL (
 *   SELECT jr.*
 *   FROM crawler.html_response jr
 *   WHERE jr.frontier_id = f.id
 *   ORDER BY jr.fetched_at DESC
 *   LIMIT 1
 * ) jr ON true
 * JOIN crawler.html_response_cache jrc ON jr.content_hash = jrc.content_hash
 * WHERE f.id = :id::bigint
 * ```
 */
export const getFrontierHtmlResponse = new PreparedQuery<IGetFrontierHtmlResponseParams,IGetFrontierHtmlResponseResult>(getFrontierHtmlResponseIR);


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

const getPendingFetchIR: any = {"usedParamSet":{"allowRefetch":true,"capacity":true},"params":[{"name":"allowRefetch","required":false,"transform":{"type":"scalar"},"locs":[{"a":198,"b":210}]},{"name":"capacity","required":false,"transform":{"type":"scalar"},"locs":[{"a":561,"b":569}]}],"statement":"WITH eligible AS (\n  SELECT id, federation, kind, key, last_fetched_at\n  FROM crawler.frontier\n  WHERE (next_fetch_at IS NULL OR next_fetch_at <= now())\n    AND (fetch_status = 'pending'\n       OR (:allowRefetch AND fetch_status = 'ok' AND process_status = 'ok'))\n), ranked AS (\n  SELECT\n    id, federation, kind, key, last_fetched_at,\n    row_number() OVER (\n      PARTITION BY federation, kind\n      ORDER BY last_fetched_at NULLS FIRST\n    ) AS rn\n  FROM eligible\n)\nSELECT id, federation, kind, key\nFROM ranked\nORDER BY rn, last_fetched_at NULLS FIRST\nLIMIT :capacity"};

/**
 * Query generated from SQL:
 * ```
 * WITH eligible AS (
 *   SELECT id, federation, kind, key, last_fetched_at
 *   FROM crawler.frontier
 *   WHERE (next_fetch_at IS NULL OR next_fetch_at <= now())
 *     AND (fetch_status = 'pending'
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
export type IGetNextPendingProcessParams = void;

/** 'GetNextPendingProcess' return type */
export interface IGetNextPendingProcessResult {
  discovered_at: Date;
  error_count: number;
  federation: string;
  fetch_status: fetch_status;
  id: string;
  key: string;
  kind: string;
  last_fetched_at: Date | null;
  meta: Json;
  next_fetch_at: Date | null;
  process_status: process_status;
}

/** 'GetNextPendingProcess' query type */
export interface IGetNextPendingProcessQuery {
  params: IGetNextPendingProcessParams;
  result: IGetNextPendingProcessResult;
}

const getNextPendingProcessIR: any = {"usedParamSet":{},"params":[],"statement":"SELECT *\nFROM crawler.frontier\nWHERE process_status = 'pending'\n  AND fetch_status IN ('ok', 'gone')\nORDER BY last_fetched_at, discovered_at\nFOR UPDATE SKIP LOCKED\nLIMIT 1"};

/**
 * Query generated from SQL:
 * ```
 * SELECT *
 * FROM crawler.frontier
 * WHERE process_status = 'pending'
 *   AND fetch_status IN ('ok', 'gone')
 * ORDER BY last_fetched_at, discovered_at
 * FOR UPDATE SKIP LOCKED
 * LIMIT 1
 * ```
 */
export const getNextPendingProcess = new PreparedQuery<IGetNextPendingProcessParams,IGetNextPendingProcessResult>(getNextPendingProcessIR);


/** 'CountPendingProcess' parameters type */
export type ICountPendingProcessParams = void;

/** 'CountPendingProcess' return type */
export interface ICountPendingProcessResult {
  count: number | null;
}

/** 'CountPendingProcess' query type */
export interface ICountPendingProcessQuery {
  params: ICountPendingProcessParams;
  result: ICountPendingProcessResult;
}

const countPendingProcessIR: any = {"usedParamSet":{},"params":[],"statement":"SELECT count(*)::int\nFROM crawler.frontier\nWHERE process_status = 'pending'\n  AND fetch_status IN ('ok', 'gone')"};

/**
 * Query generated from SQL:
 * ```
 * SELECT count(*)::int
 * FROM crawler.frontier
 * WHERE process_status = 'pending'
 *   AND fetch_status IN ('ok', 'gone')
 * ```
 */
export const countPendingProcess = new PreparedQuery<ICountPendingProcessParams,ICountPendingProcessResult>(countPendingProcessIR);


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


/** 'MarkFrontierProcessSuccess' parameters type */
export interface IMarkFrontierProcessSuccessParams {
  id?: NumberOrString | null | void;
}

/** 'MarkFrontierProcessSuccess' return type */
export type IMarkFrontierProcessSuccessResult = void;

/** 'MarkFrontierProcessSuccess' query type */
export interface IMarkFrontierProcessSuccessQuery {
  params: IMarkFrontierProcessSuccessParams;
  result: IMarkFrontierProcessSuccessResult;
}

const markFrontierProcessSuccessIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":61,"b":63}]}],"statement":"UPDATE crawler.frontier SET process_status = 'ok' WHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier SET process_status = 'ok' WHERE id = :id::bigint
 * ```
 */
export const markFrontierProcessSuccess = new PreparedQuery<IMarkFrontierProcessSuccessParams,IMarkFrontierProcessSuccessResult>(markFrontierProcessSuccessIR);


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


/** 'InsertHtmlResponse' parameters type */
export interface IInsertHtmlResponseParams {
  content?: string | null | void;
  error?: string | null | void;
  httpStatus?: number | null | void;
  id?: NumberOrString | null | void;
  url?: string | null | void;
}

/** 'InsertHtmlResponse' return type */
export type IInsertHtmlResponseResult = void;

/** 'InsertHtmlResponse' query type */
export interface IInsertHtmlResponseQuery {
  params: IInsertHtmlResponseParams;
  result: IInsertHtmlResponseResult;
}

const insertHtmlResponseIR: any = {"usedParamSet":{"content":true,"id":true,"url":true,"httpStatus":true,"error":true},"params":[{"name":"content","required":false,"transform":{"type":"scalar"},"locs":[{"a":27,"b":34}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":321,"b":323}]},{"name":"url","required":false,"transform":{"type":"scalar"},"locs":[{"a":326,"b":329}]},{"name":"httpStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":331,"b":341}]},{"name":"error","required":false,"transform":{"type":"scalar"},"locs":[{"a":344,"b":349}]}],"statement":"WITH payload AS (\n  SELECT :content AS content\n), ins_cache AS (\n  INSERT INTO crawler.html_response_cache (content)\n    SELECT content\n    FROM payload\n    WHERE content IS NOT NULL\n    ON CONFLICT (content_hash) DO NOTHING\n)\nINSERT INTO crawler.html_response (frontier_id, url, http_status, error, content_hash)\nSELECT :id, :url,:httpStatus, :error,\n  case when content IS NULL then NULL else encode(digest(content, 'sha256'), 'hex') end AS content_hash\nFROM payload"};

/**
 * Query generated from SQL:
 * ```
 * WITH payload AS (
 *   SELECT :content AS content
 * ), ins_cache AS (
 *   INSERT INTO crawler.html_response_cache (content)
 *     SELECT content
 *     FROM payload
 *     WHERE content IS NOT NULL
 *     ON CONFLICT (content_hash) DO NOTHING
 * )
 * INSERT INTO crawler.html_response (frontier_id, url, http_status, error, content_hash)
 * SELECT :id, :url,:httpStatus, :error,
 *   case when content IS NULL then NULL else encode(digest(content, 'sha256'), 'hex') end AS content_hash
 * FROM payload
 * ```
 */
export const insertHtmlResponse = new PreparedQuery<IInsertHtmlResponseParams,IInsertHtmlResponseResult>(insertHtmlResponseIR);


/** 'InsertJsonResponse' parameters type */
export interface IInsertJsonResponseParams {
  content?: string | null | void;
  error?: string | null | void;
  httpStatus?: number | null | void;
  id?: NumberOrString | null | void;
  url?: string | null | void;
}

/** 'InsertJsonResponse' return type */
export type IInsertJsonResponseResult = void;

/** 'InsertJsonResponse' query type */
export interface IInsertJsonResponseQuery {
  params: IInsertJsonResponseParams;
  result: IInsertJsonResponseResult;
}

const insertJsonResponseIR: any = {"usedParamSet":{"content":true,"id":true,"url":true,"httpStatus":true,"error":true},"params":[{"name":"content","required":false,"transform":{"type":"scalar"},"locs":[{"a":27,"b":34}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":334,"b":336}]},{"name":"url","required":false,"transform":{"type":"scalar"},"locs":[{"a":339,"b":342}]},{"name":"httpStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":345,"b":355}]},{"name":"error","required":false,"transform":{"type":"scalar"},"locs":[{"a":358,"b":363}]}],"statement":"WITH payload AS (\n  SELECT :content::text::jsonb AS content\n), ins_cache AS (\n  INSERT INTO crawler.json_response_cache (content)\n    SELECT content\n    FROM payload\n    WHERE content IS NOT NULL\n    ON CONFLICT (content_hash) DO NOTHING\n)\nINSERT INTO crawler.json_response (frontier_id, url, http_status, error, content_hash)\nSELECT :id, :url, :httpStatus, :error,\n       case when content IS NULL then NULL else encode(digest(content::text, 'sha256'), 'hex') end AS content_hash\nFROM payload"};

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
export const insertJsonResponse = new PreparedQuery<IInsertJsonResponseParams,IInsertJsonResponseResult>(insertJsonResponseIR);


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


/** 'UpsertFrontiers' parameters type */
export interface IUpsertFrontiersParams {
  federation?: string | null | void;
  keys?: stringArray | null | void;
  kind?: string | null | void;
}

/** 'UpsertFrontiers' return type */
export type IUpsertFrontiersResult = void;

/** 'UpsertFrontiers' query type */
export interface IUpsertFrontiersQuery {
  params: IUpsertFrontiersParams;
  result: IUpsertFrontiersResult;
}

const upsertFrontiersIR: any = {"usedParamSet":{"federation":true,"kind":true,"keys":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":60,"b":70}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":73,"b":77}]},{"name":"keys","required":false,"transform":{"type":"scalar"},"locs":[{"a":96,"b":100}]}],"statement":"INSERT INTO crawler.frontier (federation, kind, key)\nSELECT :federation, :kind, key\nFROM unnest(:keys::text[]) AS t(key)\nON CONFLICT (federation, kind, key) DO NOTHING"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO crawler.frontier (federation, kind, key)
 * SELECT :federation, :kind, key
 * FROM unnest(:keys::text[]) AS t(key)
 * ON CONFLICT (federation, kind, key) DO NOTHING
 * ```
 */
export const upsertFrontiers = new PreparedQuery<IUpsertFrontiersParams,IUpsertFrontiersResult>(upsertFrontiersIR);


/** 'GetFrontierKeyBounds' parameters type */
export interface IGetFrontierKeyBoundsParams {
  federation?: string | null | void;
  kind?: string | null | void;
}

/** 'GetFrontierKeyBounds' return type */
export interface IGetFrontierKeyBoundsResult {
  max_key: string | null;
  min_key: string | null;
}

/** 'GetFrontierKeyBounds' query type */
export interface IGetFrontierKeyBoundsQuery {
  params: IGetFrontierKeyBoundsParams;
  result: IGetFrontierKeyBoundsResult;
}

const getFrontierKeyBoundsIR: any = {"usedParamSet":{"federation":true,"kind":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":117,"b":127}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":142,"b":146}]}],"statement":"SELECT\n  min(key COLLATE \"C\") AS min_key,\n  max(key COLLATE \"C\") AS max_key\nFROM crawler.frontier\nWHERE federation = :federation\n  AND kind = :kind"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   min(key COLLATE "C") AS min_key,
 *   max(key COLLATE "C") AS max_key
 * FROM crawler.frontier
 * WHERE federation = :federation
 *   AND kind = :kind
 * ```
 */
export const getFrontierKeyBounds = new PreparedQuery<IGetFrontierKeyBoundsParams,IGetFrontierKeyBoundsResult>(getFrontierKeyBoundsIR);


/** 'GetExistingFrontierKeys' parameters type */
export interface IGetExistingFrontierKeysParams {
  federation?: string | null | void;
  keys?: stringArray | null | void;
  kind?: string | null | void;
}

/** 'GetExistingFrontierKeys' return type */
export interface IGetExistingFrontierKeysResult {
  key: string;
}

/** 'GetExistingFrontierKeys' query type */
export interface IGetExistingFrontierKeysQuery {
  params: IGetExistingFrontierKeysParams;
  result: IGetExistingFrontierKeysResult;
}

const getExistingFrontierKeysIR: any = {"usedParamSet":{"federation":true,"kind":true,"keys":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":52,"b":62}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":77,"b":81}]},{"name":"keys","required":false,"transform":{"type":"scalar"},"locs":[{"a":99,"b":103}]}],"statement":"SELECT key\nFROM crawler.frontier\nWHERE federation = :federation\n  AND kind = :kind\n  AND key = ANY(:keys::text[])"};

/**
 * Query generated from SQL:
 * ```
 * SELECT key
 * FROM crawler.frontier
 * WHERE federation = :federation
 *   AND kind = :kind
 *   AND key = ANY(:keys::text[])
 * ```
 */
export const getExistingFrontierKeys = new PreparedQuery<IGetExistingFrontierKeysParams,IGetExistingFrontierKeysResult>(getExistingFrontierKeysIR);


