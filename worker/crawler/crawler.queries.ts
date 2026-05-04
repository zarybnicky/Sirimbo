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


/** 'GetCrawlerFrontierStatus' parameters type */
export interface IGetCrawlerFrontierStatusParams {
  allowRefetch?: boolean | null | void;
  federation?: string | null | void;
  kind?: string | null | void;
}

/** 'GetCrawlerFrontierStatus' return type */
export interface IGetCrawlerFrontierStatusResult {
  done: number;
  federation: string;
  fetch_due: number;
  fetch_error: number;
  fetch_pending: number;
  fetch_transient: number;
  keys: string;
  kind: string;
  latest: Date | null;
  locked_fetch: number;
  oldest_due_at: Date | null;
  oldest_process_ready_at: Date | null;
  process_error: number;
  process_ready: number;
  queued_fetch: number;
  sample_due_id: string | null;
  sample_due_key: string | null;
  sample_process_id: string | null;
  scheduled_fetch: number;
  total: number;
  unscheduled_fetch: number;
}

/** 'GetCrawlerFrontierStatus' query type */
export interface IGetCrawlerFrontierStatusQuery {
  params: IGetCrawlerFrontierStatusParams;
  result: IGetCrawlerFrontierStatusResult;
}

const getCrawlerFrontierStatusIR: any = {"usedParamSet":{"federation":true,"kind":true,"allowRefetch":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":277,"b":287},{"a":321,"b":331},{"a":1646,"b":1656},{"a":1691,"b":1701},{"a":2342,"b":2352},{"a":2384,"b":2394}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":343,"b":347},{"a":375,"b":379},{"a":1713,"b":1717},{"a":1746,"b":1750},{"a":2406,"b":2410},{"a":2436,"b":2440}]},{"name":"allowRefetch","required":false,"transform":{"type":"scalar"},"locs":[{"a":1610,"b":1622}]}],"statement":"WITH frontiers AS (\n  SELECT\n    f.*,\n    row_number() OVER (\n      PARTITION BY f.federation, f.kind\n      ORDER BY\n        CASE WHEN f.fetch_status = 'pending' THEN 0 ELSE 1 END,\n        f.discovered_at DESC,\n        f.id\n    ) AS key_rank\n  FROM crawler.frontier f\n  WHERE (:federation::text IS NULL OR f.federation = :federation)\n    AND (:kind::text IS NULL OR f.kind = :kind)\n), frontier_status AS (\n  SELECT\n    federation,\n    kind,\n    count(*)::int AS total,\n    coalesce(\n      string_agg(key, ', ' ORDER BY key_rank) FILTER (WHERE key <> '' AND key_rank <= 3)\n        || CASE WHEN count(*) FILTER (WHERE key <> '') > 3 THEN ', ...' ELSE '' END,\n      '-'\n    ) AS keys,\n    count(*) FILTER (WHERE fetch_status = 'pending')::int AS fetch_pending,\n    count(*) FILTER (WHERE fetch_status = 'transient')::int AS fetch_transient,\n    count(*) FILTER (WHERE fetch_status = 'error')::int AS fetch_error,\n    count(*) FILTER (WHERE process_status = 'ok')::int AS done,\n    count(*) FILTER (WHERE process_status = 'error')::int AS process_error,\n    count(*) FILTER (WHERE process_status = 'pending' AND fetch_status = 'ok')::int\n      AS process_ready,\n    min(last_fetched_at) FILTER (WHERE process_status = 'pending' AND fetch_status = 'ok')\n      AS oldest_process_ready_at,\n    (array_agg(id ORDER BY last_fetched_at NULLS FIRST, discovered_at, id)\n      FILTER (WHERE process_status = 'pending' AND fetch_status = 'ok'))[1]\n      AS sample_process_id,\n    max(last_fetched_at) AS latest\n  FROM frontiers f\n  GROUP BY federation, kind\n), due_fetch AS (\n  SELECT df.*\n  FROM crawler.frontier_fetch_due(:allowRefetch::boolean) df\n  WHERE (:federation::text IS NULL OR df.federation = :federation)\n    AND (:kind::text IS NULL OR df.kind = :kind)\n), due_status AS (\n  SELECT\n    federation,\n    kind,\n    count(*)::int AS fetch_due,\n    min(due_at) AS oldest_due_at,\n    (array_agg(id ORDER BY due_at, last_fetched_at NULLS FIRST, id))[1] AS sample_due_id,\n    (array_agg(key ORDER BY due_at, last_fetched_at NULLS FIRST, id))[1] AS sample_due_key\n  FROM due_fetch\n  GROUP BY federation, kind\n), fetch_jobs AS (\n  SELECT\n    federation,\n    kind,\n    count(*) FILTER (WHERE state IN ('ready', 'delayed'))::int AS queued_fetch,\n    count(*) FILTER (WHERE state = 'locked')::int AS locked_fetch\n  FROM crawler.frontier_fetch_job\n  WHERE (:federation::text IS NULL OR federation = :federation)\n    AND (:kind::text IS NULL OR kind = :kind)\n    AND state IN ('ready', 'delayed', 'locked')\n  GROUP BY federation, kind\n)\nSELECT\n  fs.federation AS \"federation!\",\n  fs.kind AS \"kind!\",\n  fs.total AS \"total!\",\n  fs.keys AS \"keys!\",\n  fs.fetch_pending AS \"fetch_pending!\",\n  fs.fetch_transient AS \"fetch_transient!\",\n  fs.fetch_error AS \"fetch_error!\",\n  fs.done AS \"done!\",\n  fs.process_error AS \"process_error!\",\n  fs.latest,\n  coalesce(ds.fetch_due, 0)::int AS \"fetch_due!\",\n  ds.oldest_due_at,\n  ds.sample_due_id,\n  ds.sample_due_key,\n  fs.process_ready AS \"process_ready!\",\n  fs.oldest_process_ready_at,\n  fs.sample_process_id,\n  coalesce(fj.queued_fetch, 0)::int AS \"queued_fetch!\",\n  coalesce(fj.locked_fetch, 0)::int AS \"locked_fetch!\",\n  (coalesce(fj.queued_fetch, 0) + coalesce(fj.locked_fetch, 0))::int\n    AS \"scheduled_fetch!\",\n  greatest(\n    coalesce(ds.fetch_due, 0) - coalesce(fj.queued_fetch, 0) - coalesce(fj.locked_fetch, 0),\n    0\n  )::int AS \"unscheduled_fetch!\"\nFROM frontier_status fs\nLEFT JOIN due_status ds USING (federation, kind)\nLEFT JOIN fetch_jobs fj USING (federation, kind)\nORDER BY fs.federation, fs.kind"};

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
 * ), frontier_status AS (
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
 *     count(*) FILTER (WHERE fetch_status = 'error')::int AS fetch_error,
 *     count(*) FILTER (WHERE process_status = 'ok')::int AS done,
 *     count(*) FILTER (WHERE process_status = 'error')::int AS process_error,
 *     count(*) FILTER (WHERE process_status = 'pending' AND fetch_status = 'ok')::int
 *       AS process_ready,
 *     min(last_fetched_at) FILTER (WHERE process_status = 'pending' AND fetch_status = 'ok')
 *       AS oldest_process_ready_at,
 *     (array_agg(id ORDER BY last_fetched_at NULLS FIRST, discovered_at, id)
 *       FILTER (WHERE process_status = 'pending' AND fetch_status = 'ok'))[1]
 *       AS sample_process_id,
 *     max(last_fetched_at) AS latest
 *   FROM frontiers f
 *   GROUP BY federation, kind
 * ), due_fetch AS (
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
 * ), fetch_jobs AS (
 *   SELECT
 *     federation,
 *     kind,
 *     count(*) FILTER (WHERE state IN ('ready', 'delayed'))::int AS queued_fetch,
 *     count(*) FILTER (WHERE state = 'locked')::int AS locked_fetch
 *   FROM crawler.frontier_fetch_job
 *   WHERE (:federation::text IS NULL OR federation = :federation)
 *     AND (:kind::text IS NULL OR kind = :kind)
 *     AND state IN ('ready', 'delayed', 'locked')
 *   GROUP BY federation, kind
 * )
 * SELECT
 *   fs.federation AS "federation!",
 *   fs.kind AS "kind!",
 *   fs.total AS "total!",
 *   fs.keys AS "keys!",
 *   fs.fetch_pending AS "fetch_pending!",
 *   fs.fetch_transient AS "fetch_transient!",
 *   fs.fetch_error AS "fetch_error!",
 *   fs.done AS "done!",
 *   fs.process_error AS "process_error!",
 *   fs.latest,
 *   coalesce(ds.fetch_due, 0)::int AS "fetch_due!",
 *   ds.oldest_due_at,
 *   ds.sample_due_id,
 *   ds.sample_due_key,
 *   fs.process_ready AS "process_ready!",
 *   fs.oldest_process_ready_at,
 *   fs.sample_process_id,
 *   coalesce(fj.queued_fetch, 0)::int AS "queued_fetch!",
 *   coalesce(fj.locked_fetch, 0)::int AS "locked_fetch!",
 *   (coalesce(fj.queued_fetch, 0) + coalesce(fj.locked_fetch, 0))::int
 *     AS "scheduled_fetch!",
 *   greatest(
 *     coalesce(ds.fetch_due, 0) - coalesce(fj.queued_fetch, 0) - coalesce(fj.locked_fetch, 0),
 *     0
 *   )::int AS "unscheduled_fetch!"
 * FROM frontier_status fs
 * LEFT JOIN due_status ds USING (federation, kind)
 * LEFT JOIN fetch_jobs fj USING (federation, kind)
 * ORDER BY fs.federation, fs.kind
 * ```
 */
export const getCrawlerFrontierStatus = new PreparedQuery<IGetCrawlerFrontierStatusParams,IGetCrawlerFrontierStatusResult>(getCrawlerFrontierStatusIR);


/** 'QueueCrawlerKick' parameters type */
export type IQueueCrawlerKickParams = void;

/** 'QueueCrawlerKick' return type */
export interface IQueueCrawlerKickResult {
  job_id: string;
}

/** 'QueueCrawlerKick' query type */
export interface IQueueCrawlerKickQuery {
  params: IQueueCrawlerKickParams;
  result: IQueueCrawlerKickResult;
}

const queueCrawlerKickIR: any = {"usedParamSet":{},"params":[],"statement":"SELECT (\n  graphile_worker.add_job(\n    identifier => 'frontier_schedule',\n    payload => '{}'::json,\n    run_at => now(),\n    job_key => 'frontier_schedule',\n    job_key_mode => 'replace'\n  )\n).id AS \"job_id!\""};

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
 * ).id AS "job_id!"
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
  errorContains?: string | null | void;
  excludeHttpStatuses?: numberArray | null | void;
  federation?: string | null | void;
  httpStatuses?: numberArray | null | void;
  key?: string | null | void;
  kind?: string | null | void;
  limit?: NumberOrString | null | void;
}

/** 'GetLatestFrontierFailures' return type */
export interface IGetLatestFrontierFailuresResult {
  error_count: number;
  error_text: string;
  failed_at: Date;
  failure: string;
  federation: string;
  fetch_status: fetch_status;
  http_status: number | null;
  id: string;
  key: string;
  kind: string;
  next_fetch_at: Date | null;
  process_error: string | null;
  process_status: process_status;
  response_error: string | null;
  url: string | null;
}

/** 'GetLatestFrontierFailures' query type */
export interface IGetLatestFrontierFailuresQuery {
  params: IGetLatestFrontierFailuresParams;
  result: IGetLatestFrontierFailuresResult;
}

const getLatestFrontierFailuresIR: any = {"usedParamSet":{"federation":true,"kind":true,"key":true,"excludeHttpStatuses":true,"httpStatuses":true,"errorContains":true,"limit":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":391,"b":401},{"a":433,"b":443}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":453,"b":457},{"a":483,"b":487}]},{"name":"key","required":false,"transform":{"type":"scalar"},"locs":[{"a":497,"b":500},{"a":525,"b":528}]},{"name":"excludeHttpStatuses","required":false,"transform":{"type":"scalar"},"locs":[{"a":543,"b":562},{"a":636,"b":655}]},{"name":"httpStatuses","required":false,"transform":{"type":"scalar"},"locs":[{"a":677,"b":689},{"a":727,"b":739}]},{"name":"errorContains","required":false,"transform":{"type":"scalar"},"locs":[{"a":755,"b":768},{"a":815,"b":828}]},{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":880,"b":885}]}],"statement":"SELECT\n  id AS \"id!\",\n  federation AS \"federation!\",\n  kind AS \"kind!\",\n  key AS \"key!\",\n  fetch_status AS \"fetch_status!\",\n  process_status AS \"process_status!\",\n  error_count AS \"error_count!\",\n  next_fetch_at,\n  failed_at AS \"failed_at!\",\n  url,\n  http_status,\n  response_error,\n  process_error,\n  failure AS \"failure!\",\n  error_text AS \"error_text!\"\nFROM crawler.frontier_failure\nWHERE (:federation::text IS NULL OR federation = :federation)\n  AND (:kind::text IS NULL OR kind = :kind)\n  AND (:key::text IS NULL OR key = :key)\n  AND (\n    :excludeHttpStatuses::int[] IS NULL\n    OR http_status IS NULL\n    OR NOT (http_status = ANY(:excludeHttpStatuses::int[]))\n  )\n  AND (:httpStatuses::int[] IS NULL OR http_status = ANY(:httpStatuses))\n  AND (\n    :errorContains::text IS NULL\n    OR error_text ILIKE '%' || :errorContains || '%'\n  )\nORDER BY failed_at DESC, id DESC\nLIMIT :limit"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   id AS "id!",
 *   federation AS "federation!",
 *   kind AS "kind!",
 *   key AS "key!",
 *   fetch_status AS "fetch_status!",
 *   process_status AS "process_status!",
 *   error_count AS "error_count!",
 *   next_fetch_at,
 *   failed_at AS "failed_at!",
 *   url,
 *   http_status,
 *   response_error,
 *   process_error,
 *   failure AS "failure!",
 *   error_text AS "error_text!"
 * FROM crawler.frontier_failure
 * WHERE (:federation::text IS NULL OR federation = :federation)
 *   AND (:kind::text IS NULL OR kind = :kind)
 *   AND (:key::text IS NULL OR key = :key)
 *   AND (
 *     :excludeHttpStatuses::int[] IS NULL
 *     OR http_status IS NULL
 *     OR NOT (http_status = ANY(:excludeHttpStatuses::int[]))
 *   )
 *   AND (:httpStatuses::int[] IS NULL OR http_status = ANY(:httpStatuses))
 *   AND (
 *     :errorContains::text IS NULL
 *     OR error_text ILIKE '%' || :errorContains || '%'
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
  count: number;
  error_fingerprint: string;
  failure: string;
  federation: string;
  http_status: number | null;
  kind: string;
  latest_failed_at: Date;
  sample_id: string;
  sample_key: string;
  samples: string;
}

/** 'GetFrontierFailureGroups' query type */
export interface IGetFrontierFailureGroupsQuery {
  params: IGetFrontierFailureGroupsParams;
  result: IGetFrontierFailureGroupsResult;
}

const getFrontierFailureGroupsIR: any = {"usedParamSet":{"federation":true,"kind":true,"excludeHttpStatuses":true,"limit":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":213,"b":223},{"a":255,"b":265}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":277,"b":281},{"a":307,"b":311}]},{"name":"excludeHttpStatuses","required":false,"transform":{"type":"scalar"},"locs":[{"a":330,"b":349},{"a":427,"b":446}]},{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":1493,"b":1498}]}],"statement":"WITH failures AS (\n  SELECT\n    id,\n    federation,\n    kind,\n    key,\n    failed_at,\n    http_status,\n    failure,\n    coalesce(nullif(error_text, ''), '-') AS error_text\n  FROM crawler.frontier_failure\n  WHERE (:federation::text IS NULL OR federation = :federation)\n    AND (:kind::text IS NULL OR kind = :kind)\n    AND (\n      :excludeHttpStatuses::int[] IS NULL\n      OR http_status IS NULL\n      OR NOT (http_status = ANY(:excludeHttpStatuses::int[]))\n    )\n), fingerprinted AS (\n  SELECT\n    *,\n    left(regexp_replace(error_text, '\\s+', ' ', 'g'), 180) AS error_fingerprint\n  FROM failures\n), ranked AS (\n  SELECT\n    *,\n    row_number() OVER (\n      PARTITION BY federation, kind, failure, http_status, error_fingerprint\n      ORDER BY failed_at DESC, id DESC\n    ) AS sample_rank\n  FROM fingerprinted\n)\nSELECT\n  federation AS \"federation!\",\n  kind AS \"kind!\",\n  failure AS \"failure!\",\n  http_status,\n  error_fingerprint AS \"error_fingerprint!\",\n  count(*)::int AS \"count!\",\n  max(failed_at) AS \"latest_failed_at!\",\n  (array_agg(id ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank = 1))[1]\n    AS \"sample_id!\",\n  (array_agg(key ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank = 1))[1]\n    AS \"sample_key!\",\n  string_agg(id::text || ':' || key, ', ' ORDER BY failed_at DESC, id DESC)\n    FILTER (WHERE sample_rank <= 5) AS \"samples!\"\nFROM ranked\nGROUP BY federation, kind, failure, http_status, error_fingerprint\nORDER BY count(*) DESC, max(failed_at) DESC\nLIMIT :limit"};

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
 *   federation AS "federation!",
 *   kind AS "kind!",
 *   failure AS "failure!",
 *   http_status,
 *   error_fingerprint AS "error_fingerprint!",
 *   count(*)::int AS "count!",
 *   max(failed_at) AS "latest_failed_at!",
 *   (array_agg(id ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank = 1))[1]
 *     AS "sample_id!",
 *   (array_agg(key ORDER BY failed_at DESC, id DESC) FILTER (WHERE sample_rank = 1))[1]
 *     AS "sample_key!",
 *   string_agg(id::text || ':' || key, ', ' ORDER BY failed_at DESC, id DESC)
 *     FILTER (WHERE sample_rank <= 5) AS "samples!"
 * FROM ranked
 * GROUP BY federation, kind, failure, http_status, error_fingerprint
 * ORDER BY count(*) DESC, max(failed_at) DESC
 * LIMIT :limit
 * ```
 */
export const getFrontierFailureGroups = new PreparedQuery<IGetFrontierFailureGroupsParams,IGetFrontierFailureGroupsResult>(getFrontierFailureGroupsIR);


/** 'GetCrawlerWorkerJobStatus' parameters type */
export type IGetCrawlerWorkerJobStatusParams = void;

/** 'GetCrawlerWorkerJobStatus' return type */
export interface IGetCrawlerWorkerJobStatusResult {
  delayed: number;
  failed: number;
  latest_failed_at: Date | null;
  locked: number;
  ready: number;
  task_identifier: string;
}

/** 'GetCrawlerWorkerJobStatus' query type */
export interface IGetCrawlerWorkerJobStatusQuery {
  params: IGetCrawlerWorkerJobStatusParams;
  result: IGetCrawlerWorkerJobStatusResult;
}

const getCrawlerWorkerJobStatusIR: any = {"usedParamSet":{},"params":[],"statement":"WITH jobs AS (\n  SELECT\n    'frontier_fetch' AS task_identifier,\n    count(*) FILTER (WHERE state = 'ready')::int AS ready,\n    count(*) FILTER (WHERE state = 'delayed')::int AS delayed,\n    count(*) FILTER (WHERE state = 'locked')::int AS locked,\n    count(*) FILTER (WHERE state = 'failed')::int AS failed,\n    max(job_updated_at) FILTER (WHERE state = 'failed') AS latest_failed_at\n  FROM crawler.frontier_fetch_job\n  UNION ALL\n  SELECT\n    task_identifier,\n    count(*) FILTER (\n      WHERE locked_at IS NULL AND run_at <= now() AND attempts < max_attempts\n    )::int AS ready,\n    count(*) FILTER (\n      WHERE locked_at IS NULL AND run_at > now() AND attempts < max_attempts\n    )::int AS delayed,\n    count(*) FILTER (WHERE locked_at IS NOT NULL)::int AS locked,\n    count(*) FILTER (WHERE attempts >= max_attempts)::int AS failed,\n    max(updated_at) FILTER (WHERE attempts >= max_attempts) AS latest_failed_at\n  FROM graphile_worker.jobs\n  WHERE task_identifier IN ('frontier_schedule', 'frontier_process')\n  GROUP BY task_identifier\n)\nSELECT\n  task_identifier AS \"task_identifier!\",\n  ready AS \"ready!\",\n  delayed AS \"delayed!\",\n  locked AS \"locked!\",\n  failed AS \"failed!\",\n  latest_failed_at\nFROM jobs\nORDER BY task_identifier"};

/**
 * Query generated from SQL:
 * ```
 * WITH jobs AS (
 *   SELECT
 *     'frontier_fetch' AS task_identifier,
 *     count(*) FILTER (WHERE state = 'ready')::int AS ready,
 *     count(*) FILTER (WHERE state = 'delayed')::int AS delayed,
 *     count(*) FILTER (WHERE state = 'locked')::int AS locked,
 *     count(*) FILTER (WHERE state = 'failed')::int AS failed,
 *     max(job_updated_at) FILTER (WHERE state = 'failed') AS latest_failed_at
 *   FROM crawler.frontier_fetch_job
 *   UNION ALL
 *   SELECT
 *     task_identifier,
 *     count(*) FILTER (
 *       WHERE locked_at IS NULL AND run_at <= now() AND attempts < max_attempts
 *     )::int AS ready,
 *     count(*) FILTER (
 *       WHERE locked_at IS NULL AND run_at > now() AND attempts < max_attempts
 *     )::int AS delayed,
 *     count(*) FILTER (WHERE locked_at IS NOT NULL)::int AS locked,
 *     count(*) FILTER (WHERE attempts >= max_attempts)::int AS failed,
 *     max(updated_at) FILTER (WHERE attempts >= max_attempts) AS latest_failed_at
 *   FROM graphile_worker.jobs
 *   WHERE task_identifier IN ('frontier_schedule', 'frontier_process')
 *   GROUP BY task_identifier
 * )
 * SELECT
 *   task_identifier AS "task_identifier!",
 *   ready AS "ready!",
 *   delayed AS "delayed!",
 *   locked AS "locked!",
 *   failed AS "failed!",
 *   latest_failed_at
 * FROM jobs
 * ORDER BY task_identifier
 * ```
 */
export const getCrawlerWorkerJobStatus = new PreparedQuery<IGetCrawlerWorkerJobStatusParams,IGetCrawlerWorkerJobStatusResult>(getCrawlerWorkerJobStatusIR);


/** 'GetCrawlerFrontierJobs' parameters type */
export interface IGetCrawlerFrontierJobsParams {
  federation?: string | null | void;
  kind?: string | null | void;
  limit?: NumberOrString | null | void;
  state?: string | null | void;
}

/** 'GetCrawlerFrontierJobs' return type */
export interface IGetCrawlerFrontierJobsResult {
  attempts: number;
  federation: string;
  fetch_status: fetch_status;
  frontier_id: string;
  frontier_key: string;
  job_error: string | null;
  job_id: string;
  job_updated_at: Date;
  kind: string;
  locked_at: Date | null;
  max_attempts: number;
  process_error: string | null;
  process_status: process_status;
  response_content: Json | null;
  response_error: string | null;
  response_http_status: number | null;
  run_at: Date;
  state: string;
}

/** 'GetCrawlerFrontierJobs' query type */
export interface IGetCrawlerFrontierJobsQuery {
  params: IGetCrawlerFrontierJobsParams;
  result: IGetCrawlerFrontierJobsResult;
}

const getCrawlerFrontierJobsIR: any = {"usedParamSet":{"state":true,"federation":true,"kind":true,"limit":true},"params":[{"name":"state","required":false,"transform":{"type":"scalar"},"locs":[{"a":292,"b":297},{"a":321,"b":326},{"a":377,"b":382}]},{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":439,"b":449},{"a":483,"b":493}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":505,"b":509},{"a":537,"b":541}]},{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":1694,"b":1699}]}],"statement":"WITH jobs AS (\n  SELECT\n    j.*,\n    (\n      j.state = 'failed'\n      OR j.job_error IS NOT NULL\n      OR j.process_error IS NOT NULL\n      OR j.fetch_status IN ('error', 'transient')\n      OR j.process_status = 'error'\n    ) AS needs_detail\n  FROM crawler.frontier_fetch_job j\n  WHERE (\n    :state::text IS NULL\n    OR (:state::text = 'failed' AND j.state = 'failed')\n    OR (:state::text = 'active' AND j.state <> 'failed')\n  )\n    AND (:federation::text IS NULL OR j.federation = :federation)\n    AND (:kind::text IS NULL OR j.kind = :kind)\n)\nSELECT\n  j.job_id AS \"job_id!\",\n  j.run_at AS \"run_at!\",\n  j.locked_at,\n  j.attempts AS \"attempts!\",\n  j.max_attempts AS \"max_attempts!\",\n  j.job_error,\n  j.job_updated_at AS \"job_updated_at!\",\n  j.frontier_id AS \"frontier_id!\",\n  j.federation AS \"federation!\",\n  j.kind AS \"kind!\",\n  j.frontier_key AS \"frontier_key!\",\n  j.fetch_status AS \"fetch_status!\",\n  j.process_status AS \"process_status!\",\n  response.http_status AS response_http_status,\n  response.error AS response_error,\n  CASE\n    WHEN j.needs_detail OR response.error IS NOT NULL THEN response.content\n  END AS response_content,\n  j.process_error,\n  j.state AS \"state!\"\nFROM jobs j\nLEFT JOIN LATERAL (\n  SELECT jr.http_status, jr.error, jrc.content\n  FROM crawler.json_response jr\n  LEFT JOIN crawler.json_response_cache jrc ON jrc.content_hash = jr.content_hash\n  WHERE jr.frontier_id = j.frontier_id\n    AND (j.needs_detail OR jr.error IS NOT NULL)\n  ORDER BY jr.fetched_at DESC\n  LIMIT 1\n) response ON true\nORDER BY\n  CASE j.state\n    WHEN 'failed' THEN 0\n    WHEN 'locked' THEN 1\n    WHEN 'ready' THEN 2\n    ELSE 3\n  END,\n  j.job_updated_at DESC,\n  j.run_at,\n  j.job_id DESC\nLIMIT :limit"};

/**
 * Query generated from SQL:
 * ```
 * WITH jobs AS (
 *   SELECT
 *     j.*,
 *     (
 *       j.state = 'failed'
 *       OR j.job_error IS NOT NULL
 *       OR j.process_error IS NOT NULL
 *       OR j.fetch_status IN ('error', 'transient')
 *       OR j.process_status = 'error'
 *     ) AS needs_detail
 *   FROM crawler.frontier_fetch_job j
 *   WHERE (
 *     :state::text IS NULL
 *     OR (:state::text = 'failed' AND j.state = 'failed')
 *     OR (:state::text = 'active' AND j.state <> 'failed')
 *   )
 *     AND (:federation::text IS NULL OR j.federation = :federation)
 *     AND (:kind::text IS NULL OR j.kind = :kind)
 * )
 * SELECT
 *   j.job_id AS "job_id!",
 *   j.run_at AS "run_at!",
 *   j.locked_at,
 *   j.attempts AS "attempts!",
 *   j.max_attempts AS "max_attempts!",
 *   j.job_error,
 *   j.job_updated_at AS "job_updated_at!",
 *   j.frontier_id AS "frontier_id!",
 *   j.federation AS "federation!",
 *   j.kind AS "kind!",
 *   j.frontier_key AS "frontier_key!",
 *   j.fetch_status AS "fetch_status!",
 *   j.process_status AS "process_status!",
 *   response.http_status AS response_http_status,
 *   response.error AS response_error,
 *   CASE
 *     WHEN j.needs_detail OR response.error IS NOT NULL THEN response.content
 *   END AS response_content,
 *   j.process_error,
 *   j.state AS "state!"
 * FROM jobs j
 * LEFT JOIN LATERAL (
 *   SELECT jr.http_status, jr.error, jrc.content
 *   FROM crawler.json_response jr
 *   LEFT JOIN crawler.json_response_cache jrc ON jrc.content_hash = jr.content_hash
 *   WHERE jr.frontier_id = j.frontier_id
 *     AND (j.needs_detail OR jr.error IS NOT NULL)
 *   ORDER BY jr.fetched_at DESC
 *   LIMIT 1
 * ) response ON true
 * ORDER BY
 *   CASE j.state
 *     WHEN 'failed' THEN 0
 *     WHEN 'locked' THEN 1
 *     WHEN 'ready' THEN 2
 *     ELSE 3
 *   END,
 *   j.job_updated_at DESC,
 *   j.run_at,
 *   j.job_id DESC
 * LIMIT :limit
 * ```
 */
export const getCrawlerFrontierJobs = new PreparedQuery<IGetCrawlerFrontierJobsParams,IGetCrawlerFrontierJobsResult>(getCrawlerFrontierJobsIR);


/** 'GetCrawlerScheduleStatus' parameters type */
export type IGetCrawlerScheduleStatusParams = void;

/** 'GetCrawlerScheduleStatus' return type */
export interface IGetCrawlerScheduleStatusResult {
  delayed: number;
  host: string;
  locked: number;
  max_requests: number | null;
  next_available_at: Date | null;
  next_run_at: Date | null;
  per_interval: string | null;
  queue_tail_at: Date | null;
  queued: number;
  ready: number;
  spacing: number | null;
}

/** 'GetCrawlerScheduleStatus' query type */
export interface IGetCrawlerScheduleStatusQuery {
  params: IGetCrawlerScheduleStatusParams;
  result: IGetCrawlerScheduleStatusResult;
}

const getCrawlerScheduleStatusIR: any = {"usedParamSet":{},"params":[],"statement":"WITH fetch_jobs AS (\n  SELECT\n    host,\n    count(*) FILTER (WHERE state IN ('ready', 'delayed'))::int AS queued,\n    count(*) FILTER (WHERE state = 'ready')::int AS ready,\n    count(*) FILTER (WHERE state = 'delayed')::int AS delayed,\n    count(*) FILTER (WHERE state = 'locked')::int AS locked,\n    min(run_at) FILTER (WHERE state IN ('ready', 'delayed')) AS next_run_at,\n    max(run_at) FILTER (WHERE state IN ('ready', 'delayed')) AS queue_tail_at\n  FROM crawler.frontier_fetch_job\n  WHERE host IS NOT NULL\n  GROUP BY host\n)\nSELECT\n  COALESCE(r.host, fj.host) AS \"host!\",\n  r.max_requests AS \"max_requests?\",\n  r.per_interval::text AS \"per_interval?\",\n  (extract(epoch from r.spacing) * 1000)::int AS \"spacing?\",\n  r.next_available_at AS \"next_available_at?\",\n  COALESCE(fj.queued, 0)::int AS \"queued!\",\n  COALESCE(fj.ready, 0)::int AS \"ready!\",\n  COALESCE(fj.delayed, 0)::int AS \"delayed!\",\n  COALESCE(fj.locked, 0)::int AS \"locked!\",\n  fj.next_run_at,\n  fj.queue_tail_at\nFROM crawler.rate_limit_rule r\nFULL JOIN fetch_jobs fj USING (host)\nORDER BY host"};

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
 *     max(run_at) FILTER (WHERE state IN ('ready', 'delayed')) AS queue_tail_at
 *   FROM crawler.frontier_fetch_job
 *   WHERE host IS NOT NULL
 *   GROUP BY host
 * )
 * SELECT
 *   COALESCE(r.host, fj.host) AS "host!",
 *   r.max_requests AS "max_requests?",
 *   r.per_interval::text AS "per_interval?",
 *   (extract(epoch from r.spacing) * 1000)::int AS "spacing?",
 *   r.next_available_at AS "next_available_at?",
 *   COALESCE(fj.queued, 0)::int AS "queued!",
 *   COALESCE(fj.ready, 0)::int AS "ready!",
 *   COALESCE(fj.delayed, 0)::int AS "delayed!",
 *   COALESCE(fj.locked, 0)::int AS "locked!",
 *   fj.next_run_at,
 *   fj.queue_tail_at
 * FROM crawler.rate_limit_rule r
 * FULL JOIN fetch_jobs fj USING (host)
 * ORDER BY host
 * ```
 */
export const getCrawlerScheduleStatus = new PreparedQuery<IGetCrawlerScheduleStatusParams,IGetCrawlerScheduleStatusResult>(getCrawlerScheduleStatusIR);


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
  response_fetched_at: Date | null;
  response_http_status: number | null;
  response_url: string | null;
}

/** 'GetFrontierDetail' query type */
export interface IGetFrontierDetailQuery {
  params: IGetFrontierDetailParams;
  result: IGetFrontierDetailResult;
}

const getFrontierDetailIR: any = {"usedParamSet":{"id":true,"federation":true,"kind":true,"key":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":916,"b":918},{"a":955,"b":957},{"a":980,"b":982}]},{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":1023,"b":1033}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":1052,"b":1056}]},{"name":"key","required":false,"transform":{"type":"scalar"},"locs":[{"a":1074,"b":1077}]}],"statement":"SELECT\n  f.id,\n  f.federation,\n  f.kind,\n  f.key,\n  f.discovered_at,\n  f.last_fetched_at,\n  f.fetch_status,\n  f.process_status,\n  f.error_count,\n  f.next_fetch_at,\n  f.last_process_error,\n  f.meta,\n  jr.url AS \"response_url?\",\n  jr.fetched_at AS \"response_fetched_at?\",\n  jr.http_status AS \"response_http_status?\",\n  jr.error AS \"response_error?\",\n  jr.content_hash AS \"response_content_hash?\",\n  j.job_id,\n  j.job_key,\n  j.run_at AS job_run_at,\n  j.attempts AS job_attempts,\n  j.max_attempts AS job_max_attempts,\n  j.job_error AS job_last_error,\n  j.locked_at AS job_locked_at\nFROM crawler.frontier f\nLEFT JOIN LATERAL (\n  SELECT jr.*\n  FROM crawler.json_response jr\n  WHERE jr.frontier_id = f.id\n  ORDER BY jr.fetched_at DESC\n  LIMIT 1\n) jr ON true\nLEFT JOIN LATERAL (\n  SELECT j.*\n  FROM crawler.frontier_fetch_job j\n  WHERE j.frontier_id = f.id\n  ORDER BY j.job_updated_at DESC\n  LIMIT 1\n) j ON true\nWHERE (\n    :id::bigint IS NOT NULL\n    AND f.id = :id::bigint\n  ) OR (\n    :id::bigint IS NULL\n    AND f.federation = :federation\n    AND f.kind = :kind\n    AND f.key = :key\n  )"};

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
 *   jr.url AS "response_url?",
 *   jr.fetched_at AS "response_fetched_at?",
 *   jr.http_status AS "response_http_status?",
 *   jr.error AS "response_error?",
 *   jr.content_hash AS "response_content_hash?",
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
  count: number;
}

/** 'GetOutstandingJobCountForTask' query type */
export interface IGetOutstandingJobCountForTaskQuery {
  params: IGetOutstandingJobCountForTaskParams;
  result: IGetOutstandingJobCountForTaskResult;
}

const getOutstandingJobCountForTaskIR: any = {"usedParamSet":{"task":true},"params":[{"name":"task","required":false,"transform":{"type":"scalar"},"locs":[{"a":83,"b":87}]}],"statement":"SELECT count(*)::int AS \"count!\"\nFROM graphile_worker.jobs\nWHERE task_identifier = :task::text\n  AND attempts < max_attempts"};

/**
 * Query generated from SQL:
 * ```
 * SELECT count(*)::int AS "count!"
 * FROM graphile_worker.jobs
 * WHERE task_identifier = :task::text
 *   AND attempts < max_attempts
 * ```
 */
export const getOutstandingJobCountForTask = new PreparedQuery<IGetOutstandingJobCountForTaskParams,IGetOutstandingJobCountForTaskResult>(getOutstandingJobCountForTaskIR);


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

const getPendingFetchIR: any = {"usedParamSet":{"allowRefetch":true,"capacity":true},"params":[{"name":"allowRefetch","required":false,"transform":{"type":"scalar"},"locs":[{"a":243,"b":255}]},{"name":"capacity","required":false,"transform":{"type":"scalar"},"locs":[{"a":420,"b":428}]}],"statement":"WITH ranked AS (\n  SELECT\n    id,\n    federation,\n    kind,\n    key,\n    last_fetched_at,\n    row_number() OVER (\n      PARTITION BY federation, kind\n      ORDER BY last_fetched_at NULLS FIRST, id\n    ) AS rn\n  FROM crawler.frontier_fetch_due(:allowRefetch::boolean)\n)\nSELECT\n  id AS \"id!\",\n  federation AS \"federation!\",\n  kind AS \"kind!\",\n  key AS \"key!\"\nFROM ranked\nORDER BY rn, last_fetched_at NULLS FIRST, id\nLIMIT :capacity"};

/**
 * Query generated from SQL:
 * ```
 * WITH ranked AS (
 *   SELECT
 *     id,
 *     federation,
 *     kind,
 *     key,
 *     last_fetched_at,
 *     row_number() OVER (
 *       PARTITION BY federation, kind
 *       ORDER BY last_fetched_at NULLS FIRST, id
 *     ) AS rn
 *   FROM crawler.frontier_fetch_due(:allowRefetch::boolean)
 * )
 * SELECT
 *   id AS "id!",
 *   federation AS "federation!",
 *   kind AS "kind!",
 *   key AS "key!"
 * FROM ranked
 * ORDER BY rn, last_fetched_at NULLS FIRST, id
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

const getNextPendingProcessIR: any = {"usedParamSet":{"limit":true},"params":[{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":522,"b":527}]}],"statement":"SELECT\n  f.id AS \"id!\",\n  f.federation AS \"federation!\",\n  f.kind AS \"kind!\",\n  f.key AS \"key!\",\n  jr.url,\n  jr.http_status,\n  jr.error,\n  jrc.content\nFROM crawler.frontier f\nJOIN LATERAL (\n  SELECT jr.*\n  FROM crawler.json_response jr\n  WHERE jr.frontier_id = f.id\n  ORDER BY jr.fetched_at DESC\n  LIMIT 1\n) jr ON true\nJOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash\nWHERE process_status = 'pending'\n  AND fetch_status = 'ok'\nORDER BY last_fetched_at, discovered_at\nFOR UPDATE SKIP LOCKED\nLIMIT :limit"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   f.id AS "id!",
 *   f.federation AS "federation!",
 *   f.kind AS "kind!",
 *   f.key AS "key!",
 *   jr.url,
 *   jr.http_status,
 *   jr.error,
 *   jrc.content
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

const getFrontierRefetchTargetIR: any = {"usedParamSet":{"federation":true,"kind":true,"key":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":121,"b":131}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":146,"b":150}]},{"name":"key","required":false,"transform":{"type":"scalar"},"locs":[{"a":164,"b":167}]}],"statement":"SELECT id AS \"id!\", federation AS \"federation!\", kind AS \"kind!\", key AS \"key!\"\nFROM crawler.frontier\nWHERE federation = :federation\n  AND kind = :kind\n  AND key = :key"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id AS "id!", federation AS "federation!", kind AS "kind!", key AS "key!"
 * FROM crawler.frontier
 * WHERE federation = :federation
 *   AND kind = :kind
 *   AND key = :key
 * ```
 */
export const getFrontierRefetchTarget = new PreparedQuery<IGetFrontierRefetchTargetParams,IGetFrontierRefetchTargetResult>(getFrontierRefetchTargetIR);


/** 'QueueFrontierRefetch' parameters type */
export interface IQueueFrontierRefetchParams {
  ids?: NumberOrStringArray | null | void;
}

/** 'QueueFrontierRefetch' return type */
export interface IQueueFrontierRefetchResult {
  federation: string;
  id: string;
  key: string;
  kind: string;
}

/** 'QueueFrontierRefetch' query type */
export interface IQueueFrontierRefetchQuery {
  params: IQueueFrontierRefetchParams;
  result: IQueueFrontierRefetchResult;
}

const queueFrontierRefetchIR: any = {"usedParamSet":{"ids":true},"params":[{"name":"ids","required":false,"transform":{"type":"scalar"},"locs":[{"a":252,"b":255}]}],"statement":"WITH frontier AS (\n  UPDATE crawler.frontier f\n  SET fetch_status = 'pending',\n      process_status = 'pending',\n      last_process_error = NULL,\n      last_process_error_at = NULL,\n      error_count = 0,\n      next_fetch_at = now()\n  WHERE f.id = ANY(:ids::bigint[])\n  RETURNING f.id, f.federation, f.kind, f.key\n)\nSELECT id AS \"id!\", federation AS \"federation!\", kind AS \"kind!\", key AS \"key!\"\nFROM frontier"};

/**
 * Query generated from SQL:
 * ```
 * WITH frontier AS (
 *   UPDATE crawler.frontier f
 *   SET fetch_status = 'pending',
 *       process_status = 'pending',
 *       last_process_error = NULL,
 *       last_process_error_at = NULL,
 *       error_count = 0,
 *       next_fetch_at = now()
 *   WHERE f.id = ANY(:ids::bigint[])
 *   RETURNING f.id, f.federation, f.kind, f.key
 * )
 * SELECT id AS "id!", federation AS "federation!", kind AS "kind!", key AS "key!"
 * FROM frontier
 * ```
 */
export const queueFrontierRefetch = new PreparedQuery<IQueueFrontierRefetchParams,IQueueFrontierRefetchResult>(queueFrontierRefetchIR);


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


/** 'UpsertFrontiers' parameters type */
export interface IUpsertFrontiersParams {
  federations?: stringArray | null | void;
  keys?: stringArray | null | void;
  kinds?: stringArray | null | void;
}

/** 'UpsertFrontiers' return type */
export type IUpsertFrontiersResult = void;

/** 'UpsertFrontiers' query type */
export interface IUpsertFrontiersQuery {
  params: IUpsertFrontiersParams;
  result: IUpsertFrontiersResult;
}

const upsertFrontiersIR: any = {"usedParamSet":{"federations":true,"kinds":true,"keys":true},"params":[{"name":"federations","required":false,"transform":{"type":"scalar"},"locs":[{"a":97,"b":108}]},{"name":"kinds","required":false,"transform":{"type":"scalar"},"locs":[{"a":121,"b":126}]},{"name":"keys","required":false,"transform":{"type":"scalar"},"locs":[{"a":139,"b":143}]}],"statement":"INSERT INTO crawler.frontier (federation, kind, key)\nSELECT federation, kind, key\nFROM unnest(\n  :federations::text[],\n  :kinds::text[],\n  :keys::text[]\n) AS input(federation, kind, key)\nON CONFLICT (federation, kind, key) DO NOTHING"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO crawler.frontier (federation, kind, key)
 * SELECT federation, kind, key
 * FROM unnest(
 *   :federations::text[],
 *   :kinds::text[],
 *   :keys::text[]
 * ) AS input(federation, kind, key)
 * ON CONFLICT (federation, kind, key) DO NOTHING
 * ```
 */
export const upsertFrontiers = new PreparedQuery<IUpsertFrontiersParams,IUpsertFrontiersResult>(upsertFrontiersIR);


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


