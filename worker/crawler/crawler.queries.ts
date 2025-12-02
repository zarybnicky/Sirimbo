/** Types generated for queries found in "crawler/crawler.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type fetch_status = 'error' | 'gone' | 'ok' | 'pending';

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

const getFrontierForUpdateIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":99,"b":101}]}],"statement":"SELECT id, federation, kind, key, fetch_status, error_count, meta\nFROM crawler.frontier\nWHERE id = :id::bigint\nFOR UPDATE SKIP LOCKED"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id, federation, kind, key, fetch_status, error_count, meta
 * FROM crawler.frontier
 * WHERE id = :id::bigint
 * FOR UPDATE SKIP LOCKED
 * ```
 */
export const getFrontierForUpdate = new PreparedQuery<IGetFrontierForUpdateParams,IGetFrontierForUpdateResult>(getFrontierForUpdateIR);


/** 'GetLatestFrontierJsonResponse' parameters type */
export interface IGetLatestFrontierJsonResponseParams {
  federation?: string | null | void;
  kind?: string | null | void;
}

/** 'GetLatestFrontierJsonResponse' return type */
export interface IGetLatestFrontierJsonResponseResult {
  content: Json | null;
  error: string | null;
  http_status: number | null;
  id: string;
  url: string;
}

/** 'GetLatestFrontierJsonResponse' query type */
export interface IGetLatestFrontierJsonResponseQuery {
  params: IGetLatestFrontierJsonResponseParams;
  result: IGetLatestFrontierJsonResponseResult;
}

const getLatestFrontierJsonResponseIR: any = {"usedParamSet":{"federation":true,"kind":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":234,"b":244}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":259,"b":263}]}],"statement":"SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content\nFROM crawler.frontier f\nJOIN crawler.json_response jr on f.id = jr.frontier_id\nJOIN crawler.json_response_cache jrc on jr.content_hash = jrc.content_hash\nWHERE f.federation = :federation AND f.kind = :kind"};

/**
 * Query generated from SQL:
 * ```
 * SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content
 * FROM crawler.frontier f
 * JOIN crawler.json_response jr on f.id = jr.frontier_id
 * JOIN crawler.json_response_cache jrc on jr.content_hash = jrc.content_hash
 * WHERE f.federation = :federation AND f.kind = :kind
 * ```
 */
export const getLatestFrontierJsonResponse = new PreparedQuery<IGetLatestFrontierJsonResponseParams,IGetLatestFrontierJsonResponseResult>(getLatestFrontierJsonResponseIR);


/** 'GetFrontierJsonResponseForUpdate' parameters type */
export interface IGetFrontierJsonResponseForUpdateParams {
  id?: NumberOrString | null | void;
}

/** 'GetFrontierJsonResponseForUpdate' return type */
export interface IGetFrontierJsonResponseForUpdateResult {
  content: Json | null;
  error: string | null;
  http_status: number | null;
  id: string;
  url: string;
}

/** 'GetFrontierJsonResponseForUpdate' query type */
export interface IGetFrontierJsonResponseForUpdateQuery {
  params: IGetFrontierJsonResponseForUpdateParams;
  result: IGetFrontierJsonResponseForUpdateResult;
}

const getFrontierJsonResponseForUpdateIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":226,"b":228}]}],"statement":"SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content\nFROM crawler.frontier f\nJOIN crawler.json_response jr on f.id = jr.frontier_id\nJOIN crawler.json_response_cache jrc on jr.content_hash = jrc.content_hash\nWHERE f.id = :id::bigint\nFOR UPDATE SKIP LOCKED"};

/**
 * Query generated from SQL:
 * ```
 * SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content
 * FROM crawler.frontier f
 * JOIN crawler.json_response jr on f.id = jr.frontier_id
 * JOIN crawler.json_response_cache jrc on jr.content_hash = jrc.content_hash
 * WHERE f.id = :id::bigint
 * FOR UPDATE SKIP LOCKED
 * ```
 */
export const getFrontierJsonResponseForUpdate = new PreparedQuery<IGetFrontierJsonResponseForUpdateParams,IGetFrontierJsonResponseForUpdateResult>(getFrontierJsonResponseForUpdateIR);


/** 'GetFrontierHtmlResponseForUpdate' parameters type */
export interface IGetFrontierHtmlResponseForUpdateParams {
  id?: NumberOrString | null | void;
}

/** 'GetFrontierHtmlResponseForUpdate' return type */
export interface IGetFrontierHtmlResponseForUpdateResult {
  content: string | null;
  error: string | null;
  http_status: number | null;
  id: string;
  url: string;
}

/** 'GetFrontierHtmlResponseForUpdate' query type */
export interface IGetFrontierHtmlResponseForUpdateQuery {
  params: IGetFrontierHtmlResponseForUpdateParams;
  result: IGetFrontierHtmlResponseForUpdateResult;
}

const getFrontierHtmlResponseForUpdateIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":226,"b":228}]}],"statement":"SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content\nFROM crawler.frontier f\nJOIN crawler.html_response jr on f.id = jr.frontier_id\nJOIN crawler.html_response_cache jrc on jr.content_hash = jrc.content_hash\nWHERE f.id = :id::bigint\nFOR UPDATE SKIP LOCKED"};

/**
 * Query generated from SQL:
 * ```
 * SELECT f.id, jr.url, jr.http_status, jr.error, jrc.content
 * FROM crawler.frontier f
 * JOIN crawler.html_response jr on f.id = jr.frontier_id
 * JOIN crawler.html_response_cache jrc on jr.content_hash = jrc.content_hash
 * WHERE f.id = :id::bigint
 * FOR UPDATE SKIP LOCKED
 * ```
 */
export const getFrontierHtmlResponseForUpdate = new PreparedQuery<IGetFrontierHtmlResponseForUpdateParams,IGetFrontierHtmlResponseForUpdateResult>(getFrontierHtmlResponseForUpdateIR);


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


/** 'GetPendingFetch' parameters type */
export interface IGetPendingFetchParams {
  limit?: NumberOrString | null | void;
}

/** 'GetPendingFetch' return type */
export interface IGetPendingFetchResult {
  id: string;
}

/** 'GetPendingFetch' query type */
export interface IGetPendingFetchQuery {
  params: IGetPendingFetchParams;
  result: IGetPendingFetchResult;
}

const getPendingFetchIR: any = {"usedParamSet":{"limit":true},"params":[{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":210,"b":215}]}],"statement":"SELECT id\nFROM crawler.frontier\nWHERE (next_fetch_at IS NULL OR next_fetch_at <= now())\n  AND (fetch_status = 'pending'\n         OR (fetch_status = 'ok' AND process_status = 'ok'))\nORDER BY discovered_at\nLIMIT :limit"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id
 * FROM crawler.frontier
 * WHERE (next_fetch_at IS NULL OR next_fetch_at <= now())
 *   AND (fetch_status = 'pending'
 *          OR (fetch_status = 'ok' AND process_status = 'ok'))
 * ORDER BY discovered_at
 * LIMIT :limit
 * ```
 */
export const getPendingFetch = new PreparedQuery<IGetPendingFetchParams,IGetPendingFetchResult>(getPendingFetchIR);


/** 'GetPendingProcess' parameters type */
export interface IGetPendingProcessParams {
  limit?: NumberOrString | null | void;
}

/** 'GetPendingProcess' return type */
export interface IGetPendingProcessResult {
  id: string;
}

/** 'GetPendingProcess' query type */
export interface IGetPendingProcessQuery {
  params: IGetPendingProcessParams;
  result: IGetPendingProcessResult;
}

const getPendingProcessIR: any = {"usedParamSet":{"limit":true},"params":[{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":160,"b":165}]}],"statement":"SELECT id\nFROM crawler.frontier\nWHERE process_status = 'pending'\n  AND fetch_status IN ('ok', 'gone')\nORDER BY last_fetched_at NULLS FIRST, discovered_at\nLIMIT :limit"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id
 * FROM crawler.frontier
 * WHERE process_status = 'pending'
 *   AND fetch_status IN ('ok', 'gone')
 * ORDER BY last_fetched_at NULLS FIRST, discovered_at
 * LIMIT :limit
 * ```
 */
export const getPendingProcess = new PreparedQuery<IGetPendingProcessParams,IGetPendingProcessResult>(getPendingProcessIR);


/** 'ReserveRequest' parameters type */
export interface IReserveRequestParams {
  host?: string | null | void;
  prefixes?: stringArray | null | void;
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

const reserveRequestIR: any = {"usedParamSet":{"host":true,"prefixes":true},"params":[{"name":"host","required":false,"transform":{"type":"scalar"},"locs":[{"a":56,"b":60}]},{"name":"prefixes","required":false,"transform":{"type":"scalar"},"locs":[{"a":69,"b":77}]}],"statement":"SELECT granted, allowed_at\nFROM crawler.reserve_request(:host::text, :prefixes::text[])"};

/**
 * Query generated from SQL:
 * ```
 * SELECT granted, allowed_at
 * FROM crawler.reserve_request(:host::text, :prefixes::text[])
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

const markFrontierFetchErrorIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":239,"b":241}]}],"statement":"UPDATE crawler.frontier\nSET last_fetched_at = now(),\n    fetch_status    = 'error',\n    error_count     = error_count + 1,\n    next_fetch_at   = now() + least(interval '5 minutes', (2 ^ (error_count + 1)) * interval '5 second')\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET last_fetched_at = now(),
 *     fetch_status    = 'error',
 *     error_count     = error_count + 1,
 *     next_fetch_at   = now() + least(interval '5 minutes', (2 ^ (error_count + 1)) * interval '5 second')
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

const markFrontierProcessSuccessIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":61,"b":63}]}],"statement":"UPDATE crawler.frontier\nSET process_status = 'ok'\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET process_status = 'ok'
 * WHERE id = :id::bigint
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

const markFrontierProcessErrorIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":64,"b":66}]}],"statement":"UPDATE crawler.frontier\nSET process_status = 'error'\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET process_status = 'error'
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

const rescheduleFrontierIR: any = {"usedParamSet":{"nextRetryAt":true,"id":true},"params":[{"name":"nextRetryAt","required":false,"transform":{"type":"scalar"},"locs":[{"a":44,"b":55}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":68,"b":70}]}],"statement":"UPDATE crawler.frontier\nSET next_fetch_at = :nextRetryAt\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET next_fetch_at = :nextRetryAt
 * WHERE id = :id::bigint
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

const insertHtmlResponseIR: any = {"usedParamSet":{"content":true,"id":true,"url":true,"httpStatus":true,"error":true},"params":[{"name":"content","required":false,"transform":{"type":"scalar"},"locs":[{"a":27,"b":34}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":321,"b":323}]},{"name":"url","required":false,"transform":{"type":"scalar"},"locs":[{"a":326,"b":329}]},{"name":"httpStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":331,"b":341}]},{"name":"error","required":false,"transform":{"type":"scalar"},"locs":[{"a":344,"b":349}]}],"statement":"WITH payload AS (\n  SELECT :content AS content\n), ins_cache AS (\n  INSERT INTO crawler.html_response_cache (content)\n    SELECT content\n    FROM payload\n    WHERE content IS NOT NULL\n    ON CONFLICT (content_hash) DO NOTHING\n)\nINSERT INTO crawler.html_response (frontier_id, url, http_status, error, content_hash)\nSELECT :id, :url,:httpStatus, :error,\n  case when content IS NULL then NULL else encode(sha256(content::TEXT::BYTEA), 'hex') end AS content_hash\nFROM payload"};

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
 *   case when content IS NULL then NULL else encode(sha256(content::TEXT::BYTEA), 'hex') end AS content_hash
 * FROM payload
 * ```
 */
export const insertHtmlResponse = new PreparedQuery<IInsertHtmlResponseParams,IInsertHtmlResponseResult>(insertHtmlResponseIR);


/** 'InsertJsonResponse' parameters type */
export interface IInsertJsonResponseParams {
  content?: Json | null | void;
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

const insertJsonResponseIR: any = {"usedParamSet":{"content":true,"id":true,"url":true,"httpStatus":true,"error":true},"params":[{"name":"content","required":false,"transform":{"type":"scalar"},"locs":[{"a":27,"b":34}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":328,"b":330}]},{"name":"url","required":false,"transform":{"type":"scalar"},"locs":[{"a":333,"b":336}]},{"name":"httpStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":339,"b":349}]},{"name":"error","required":false,"transform":{"type":"scalar"},"locs":[{"a":352,"b":357}]}],"statement":"WITH payload AS (\n  SELECT :content::jsonb AS content\n), ins_cache AS (\n  INSERT INTO crawler.json_response_cache (content)\n    SELECT content\n    FROM payload\n    WHERE content IS NOT NULL\n    ON CONFLICT (content_hash) DO NOTHING\n)\nINSERT INTO crawler.json_response (frontier_id, url, http_status, error, content_hash)\nSELECT :id, :url, :httpStatus, :error,\n       case when content IS NULL then NULL else encode(sha256(content::TEXT::BYTEA), 'hex') end AS content_hash\nFROM payload"};

/**
 * Query generated from SQL:
 * ```
 * WITH payload AS (
 *   SELECT :content::jsonb AS content
 * ), ins_cache AS (
 *   INSERT INTO crawler.json_response_cache (content)
 *     SELECT content
 *     FROM payload
 *     WHERE content IS NOT NULL
 *     ON CONFLICT (content_hash) DO NOTHING
 * )
 * INSERT INTO crawler.json_response (frontier_id, url, http_status, error, content_hash)
 * SELECT :id, :url, :httpStatus, :error,
 *        case when content IS NULL then NULL else encode(sha256(content::TEXT::BYTEA), 'hex') end AS content_hash
 * FROM payload
 * ```
 */
export const insertJsonResponse = new PreparedQuery<IInsertJsonResponseParams,IInsertJsonResponseResult>(insertJsonResponseIR);


/** 'InsertDiscoveredCstsMember' parameters type */
export interface IInsertDiscoveredCstsMemberParams {
  id?: string | null | void;
}

/** 'InsertDiscoveredCstsMember' return type */
export type IInsertDiscoveredCstsMemberResult = void;

/** 'InsertDiscoveredCstsMember' query type */
export interface IInsertDiscoveredCstsMemberQuery {
  params: IInsertDiscoveredCstsMemberParams;
  result: IInsertDiscoveredCstsMemberResult;
}

const insertDiscoveredCstsMemberIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":103,"b":105}]}],"statement":"with frontier as (\n  INSERT INTO crawler.frontier (federation, kind, key)\n    VALUES ('csts','member', :id)\n    ON CONFLICT (federation, kind, key) DO NOTHING\n    RETURNING key\n)\nUPDATE crawler.incremental_ranges\nSET last_known = GREATEST(last_known, (SELECT key::bigint FROM frontier))\nFROM frontier\nWHERE federation = 'csts' AND kind = 'member_id'"};

/**
 * Query generated from SQL:
 * ```
 * with frontier as (
 *   INSERT INTO crawler.frontier (federation, kind, key)
 *     VALUES ('csts','member', :id)
 *     ON CONFLICT (federation, kind, key) DO NOTHING
 *     RETURNING key
 * )
 * UPDATE crawler.incremental_ranges
 * SET last_known = GREATEST(last_known, (SELECT key::bigint FROM frontier))
 * FROM frontier
 * WHERE federation = 'csts' AND kind = 'member_id'
 * ```
 */
export const insertDiscoveredCstsMember = new PreparedQuery<IInsertDiscoveredCstsMemberParams,IInsertDiscoveredCstsMemberResult>(insertDiscoveredCstsMemberIR);


