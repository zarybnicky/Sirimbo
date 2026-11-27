/** Types generated for queries found in "crawler/crawler.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type fetch_status = 'error' | 'gone' | 'ok' | 'pending' | 'transient';

export type DateArray = (Date)[];

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
  next_fetch_at: Date;
}

/** 'GetFrontierForUpdate' query type */
export interface IGetFrontierForUpdateQuery {
  params: IGetFrontierForUpdateParams;
  result: IGetFrontierForUpdateResult;
}

const getFrontierForUpdateIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":114,"b":116}]}],"statement":"SELECT id, federation, kind, key, fetch_status, error_count, meta, next_fetch_at\nFROM crawler.frontier\nWHERE id = :id::bigint\nFOR UPDATE"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id, federation, kind, key, fetch_status, error_count, meta, next_fetch_at
 * FROM crawler.frontier
 * WHERE id = :id::bigint
 * FOR UPDATE
 * ```
 */
export const getFrontierForUpdate = new PreparedQuery<IGetFrontierForUpdateParams,IGetFrontierForUpdateResult>(getFrontierForUpdateIR);


/** 'GetFrontierResponses' parameters type */
export interface IGetFrontierResponsesParams {
  allowErrors?: boolean | null | void;
  federation?: string | null | void;
  id?: NumberOrString | null | void;
  key?: string | null | void;
  kind?: string | null | void;
  limit?: NumberOrString | null | void;
}

/** 'GetFrontierResponses' return type */
export interface IGetFrontierResponsesResult {
  content: Json;
  federation: string;
  fetched_at: Date;
  http_status: number | null;
  id: string;
  key: string;
  kind: string;
  reference_date: string | null;
  url: string;
}

/** 'GetFrontierResponses' query type */
export interface IGetFrontierResponsesQuery {
  params: IGetFrontierResponsesParams;
  result: IGetFrontierResponsesResult;
}

const getFrontierResponsesIR: any = {"usedParamSet":{"federation":true,"kind":true,"key":true,"id":true,"allowErrors":true,"limit":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":452,"b":462},{"a":496,"b":506}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":516,"b":520},{"a":548,"b":552}]},{"name":"key","required":false,"transform":{"type":"scalar"},"locs":[{"a":562,"b":565},{"a":592,"b":595}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":605,"b":607},{"a":635,"b":637}]},{"name":"allowErrors","required":false,"transform":{"type":"scalar"},"locs":[{"a":647,"b":658}]},{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":742,"b":747}]}],"statement":"SELECT\n  f.id AS \"id!\",\n  f.federation AS \"federation!\",\n  f.kind AS \"kind!\",\n  f.key AS \"key!\",\n  f.reference_date::text AS \"reference_date?\",\n  jr.url AS \"url!\",\n  jr.http_status,\n  coalesce(jr.last_observed_at, jr.fetched_at) AS \"fetched_at!\",\n  jrc.content AS \"content!\"\nFROM crawler.frontier f\nJOIN crawler.json_response jr ON jr.id = f.last_successful_response_id\nJOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash\nWHERE (:federation::text IS NULL OR f.federation = :federation)\n  AND (:kind::text IS NULL OR f.kind = :kind)\n  AND (:key::text IS NULL OR f.key = :key)\n  AND (:id::bigint IS NULL OR f.id = :id)\n  AND (:allowErrors OR f.fetch_status = 'ok')\nORDER BY f.discovered_at, f.last_fetched_at, f.id\nLIMIT :limit"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   f.id AS "id!",
 *   f.federation AS "federation!",
 *   f.kind AS "kind!",
 *   f.key AS "key!",
 *   f.reference_date::text AS "reference_date?",
 *   jr.url AS "url!",
 *   jr.http_status,
 *   coalesce(jr.last_observed_at, jr.fetched_at) AS "fetched_at!",
 *   jrc.content AS "content!"
 * FROM crawler.frontier f
 * JOIN crawler.json_response jr ON jr.id = f.last_successful_response_id
 * JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
 * WHERE (:federation::text IS NULL OR f.federation = :federation)
 *   AND (:kind::text IS NULL OR f.kind = :kind)
 *   AND (:key::text IS NULL OR f.key = :key)
 *   AND (:id::bigint IS NULL OR f.id = :id)
 *   AND (:allowErrors OR f.fetch_status = 'ok')
 * ORDER BY f.discovered_at, f.last_fetched_at, f.id
 * LIMIT :limit
 * ```
 */
export const getFrontierResponses = new PreparedQuery<IGetFrontierResponsesParams,IGetFrontierResponsesResult>(getFrontierResponsesIR);


/** 'GetScheduleStatus' parameters type */
export type IGetScheduleStatusParams = void;

/** 'GetScheduleStatus' return type */
export interface IGetScheduleStatusResult {
  host: string;
  locked: number;
  next_available_at: Date | null;
  queued: number;
  slots: DateArray;
  spacing: number | null;
}

/** 'GetScheduleStatus' query type */
export interface IGetScheduleStatusQuery {
  params: IGetScheduleStatusParams;
  result: IGetScheduleStatusResult;
}

const getScheduleStatusIR: any = {"usedParamSet":{},"params":[],"statement":"WITH fetch_jobs AS (\n  SELECT\n    host,\n    count(*) FILTER (WHERE state = 'queued')::int AS queued,\n    count(*) FILTER (WHERE state = 'locked')::int AS locked,\n    array_agg(run_at ORDER BY run_at) FILTER (WHERE state = 'queued') AS slots\n  FROM crawler.frontier_fetch_job\n  WHERE host IS NOT NULL\n  GROUP BY host\n)\nSELECT\n  COALESCE(r.host, fj.host) AS \"host!\",\n  (extract(epoch from r.spacing) * 1000)::int AS \"spacing?\",\n  r.next_available_at AS \"next_available_at?\",\n  COALESCE(fj.queued, 0)::int AS \"queued!\",\n  COALESCE(fj.locked, 0)::int AS \"locked!\",\n  COALESCE(fj.slots, '{}'::timestamptz[]) AS \"slots!\"\nFROM crawler.rate_limit_rule r\nFULL JOIN fetch_jobs fj USING (host)\nORDER BY host"};

/**
 * Query generated from SQL:
 * ```
 * WITH fetch_jobs AS (
 *   SELECT
 *     host,
 *     count(*) FILTER (WHERE state = 'queued')::int AS queued,
 *     count(*) FILTER (WHERE state = 'locked')::int AS locked,
 *     array_agg(run_at ORDER BY run_at) FILTER (WHERE state = 'queued') AS slots
 *   FROM crawler.frontier_fetch_job
 *   WHERE host IS NOT NULL
 *   GROUP BY host
 * )
 * SELECT
 *   COALESCE(r.host, fj.host) AS "host!",
 *   (extract(epoch from r.spacing) * 1000)::int AS "spacing?",
 *   r.next_available_at AS "next_available_at?",
 *   COALESCE(fj.queued, 0)::int AS "queued!",
 *   COALESCE(fj.locked, 0)::int AS "locked!",
 *   COALESCE(fj.slots, '{}'::timestamptz[]) AS "slots!"
 * FROM crawler.rate_limit_rule r
 * FULL JOIN fetch_jobs fj USING (host)
 * ORDER BY host
 * ```
 */
export const getScheduleStatus = new PreparedQuery<IGetScheduleStatusParams,IGetScheduleStatusResult>(getScheduleStatusIR);


/** 'QueuePendingProcess' parameters type */
export type IQueuePendingProcessParams = void;

/** 'QueuePendingProcess' return type */
export interface IQueuePendingProcessResult {
  job_id: string;
}

/** 'QueuePendingProcess' query type */
export interface IQueuePendingProcessQuery {
  params: IQueuePendingProcessParams;
  result: IQueuePendingProcessResult;
}

const queuePendingProcessIR: any = {"usedParamSet":{},"params":[],"statement":"SELECT (\n  graphile_worker.add_job(\n    identifier => 'frontier_process',\n    job_key => 'frontier_process',\n    job_key_mode => 'preserve_run_at'\n  )\n).id AS \"job_id!\"\nWHERE NOT EXISTS (\n  SELECT FROM graphile_worker.jobs\n  WHERE task_identifier = 'frontier_process' AND attempts < max_attempts\n)\nAND EXISTS (\n  SELECT FROM crawler.frontier\n  WHERE fetch_status = 'ok' AND process_status = 'pending'\n)"};

/**
 * Query generated from SQL:
 * ```
 * SELECT (
 *   graphile_worker.add_job(
 *     identifier => 'frontier_process',
 *     job_key => 'frontier_process',
 *     job_key_mode => 'preserve_run_at'
 *   )
 * ).id AS "job_id!"
 * WHERE NOT EXISTS (
 *   SELECT FROM graphile_worker.jobs
 *   WHERE task_identifier = 'frontier_process' AND attempts < max_attempts
 * )
 * AND EXISTS (
 *   SELECT FROM crawler.frontier
 *   WHERE fetch_status = 'ok' AND process_status = 'pending'
 * )
 * ```
 */
export const queuePendingProcess = new PreparedQuery<IQueuePendingProcessParams,IQueuePendingProcessResult>(queuePendingProcessIR);


/** 'GetPendingFetch' parameters type */
export interface IGetPendingFetchParams {
  allowRefetch?: boolean | null | void;
  capacity?: NumberOrString | null | void;
  loaderFederations?: stringArray | null | void;
  loaderKinds?: stringArray | null | void;
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

const getPendingFetchIR: any = {"usedParamSet":{"loaderFederations":true,"loaderKinds":true,"allowRefetch":true,"capacity":true},"params":[{"name":"loaderFederations","required":false,"transform":{"type":"scalar"},"locs":[{"a":71,"b":88}]},{"name":"loaderKinds","required":false,"transform":{"type":"scalar"},"locs":[{"a":103,"b":114}]},{"name":"allowRefetch","required":false,"transform":{"type":"scalar"},"locs":[{"a":911,"b":923}]},{"name":"capacity","required":false,"transform":{"type":"scalar"},"locs":[{"a":1233,"b":1241},{"a":1387,"b":1395}]}],"statement":"WITH allowed_loaders AS (\n  SELECT federation, kind\n  FROM unnest(\n    :loaderFederations::text[],\n    :loaderKinds::text[]\n  ) AS input(federation, kind)\n), active_fetches AS MATERIALIZED (\n  SELECT frontier_id\n  FROM crawler.frontier_fetch_job\n  WHERE state IN ('queued', 'locked')\n), ranked AS (\n  SELECT\n    candidate.*,\n    row_number() OVER (\n      PARTITION BY candidate.federation, candidate.kind\n      ORDER BY candidate.next_fetch_at, candidate.last_fetched_at NULLS FIRST, candidate.id\n    ) AS rn\n  FROM allowed_loaders loader\n  CROSS JOIN LATERAL (\n    SELECT\n      f.id,\n      f.federation,\n      f.kind,\n      f.key,\n      f.next_fetch_at,\n      f.last_fetched_at\n    FROM crawler.frontier f\n    WHERE f.federation = loader.federation\n      AND f.kind = loader.kind\n      AND f.next_fetch_at <= now()\n      AND (\n        f.fetch_status IN ('pending', 'transient')\n        OR (\n          coalesce(:allowRefetch::boolean, false)\n          AND f.fetch_status = 'ok'\n          AND f.process_status = 'ok'\n        )\n      )\n      AND NOT EXISTS (\n        SELECT 1\n        FROM active_fetches active\n        WHERE active.frontier_id = f.id\n      )\n    ORDER BY f.next_fetch_at, f.last_fetched_at NULLS FIRST, f.id\n    LIMIT :capacity\n  ) candidate\n)\nSELECT\n  id AS \"id!\",\n  federation AS \"federation!\",\n  kind AS \"kind!\",\n  key AS \"key!\"\nFROM ranked\nORDER BY rn, random()\nLIMIT :capacity"};

/**
 * Query generated from SQL:
 * ```
 * WITH allowed_loaders AS (
 *   SELECT federation, kind
 *   FROM unnest(
 *     :loaderFederations::text[],
 *     :loaderKinds::text[]
 *   ) AS input(federation, kind)
 * ), active_fetches AS MATERIALIZED (
 *   SELECT frontier_id
 *   FROM crawler.frontier_fetch_job
 *   WHERE state IN ('queued', 'locked')
 * ), ranked AS (
 *   SELECT
 *     candidate.*,
 *     row_number() OVER (
 *       PARTITION BY candidate.federation, candidate.kind
 *       ORDER BY candidate.next_fetch_at, candidate.last_fetched_at NULLS FIRST, candidate.id
 *     ) AS rn
 *   FROM allowed_loaders loader
 *   CROSS JOIN LATERAL (
 *     SELECT
 *       f.id,
 *       f.federation,
 *       f.kind,
 *       f.key,
 *       f.next_fetch_at,
 *       f.last_fetched_at
 *     FROM crawler.frontier f
 *     WHERE f.federation = loader.federation
 *       AND f.kind = loader.kind
 *       AND f.next_fetch_at <= now()
 *       AND (
 *         f.fetch_status IN ('pending', 'transient')
 *         OR (
 *           coalesce(:allowRefetch::boolean, false)
 *           AND f.fetch_status = 'ok'
 *           AND f.process_status = 'ok'
 *         )
 *       )
 *       AND NOT EXISTS (
 *         SELECT 1
 *         FROM active_fetches active
 *         WHERE active.frontier_id = f.id
 *       )
 *     ORDER BY f.next_fetch_at, f.last_fetched_at NULLS FIRST, f.id
 *     LIMIT :capacity
 *   ) candidate
 * )
 * SELECT
 *   id AS "id!",
 *   federation AS "federation!",
 *   kind AS "kind!",
 *   key AS "key!"
 * FROM ranked
 * ORDER BY rn, random()
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
  canProcess: boolean;
  content: Json;
  error: string | null;
  federation: string;
  http_status: number | null;
  id: string;
  key: string;
  kind: string;
  reference_date: string | null;
  url: string;
}

/** 'GetNextPendingProcess' query type */
export interface IGetNextPendingProcessQuery {
  params: IGetNextPendingProcessParams;
  result: IGetNextPendingProcessResult;
}

const getNextPendingProcessIR: any = {"usedParamSet":{"limit":true},"params":[{"name":"limit","required":false,"transform":{"type":"scalar"},"locs":[{"a":687,"b":692}]}],"statement":"WITH pending AS MATERIALIZED (\n  SELECT f.id, f.federation, f.kind, f.key, f.reference_date::text, jr.url, jr.http_status, jr.error, jrc.content,\n    c.id AS competition_id\n  FROM crawler.frontier f\n  JOIN crawler.json_response jr ON jr.id = f.last_successful_response_id\n  JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash\n  LEFT JOIN federated.competition c\n    ON c.federation = 'wdsf' AND c.external_id = CASE\n      WHEN f.federation = 'wdsf' AND f.kind = 'participant'\n      THEN jrc.content->>'competitionId'\n    END\n  WHERE process_status = 'pending' AND fetch_status = 'ok'\n  ORDER BY discovered_at, last_fetched_at\n  FOR UPDATE OF f SKIP LOCKED\n  LIMIT :limit\n), owned_competitions AS MATERIALIZED (\n  SELECT c.id FROM federated.competition c\n  WHERE c.id IN (SELECT competition_id FROM pending)\n  ORDER BY c.id\n  FOR NO KEY UPDATE SKIP LOCKED\n)\nSELECT id AS \"id!\", federation AS \"federation!\", kind AS \"kind!\", key AS \"key!\",\n  reference_date, url, http_status, error, content,\n  competition_id IS NULL OR competition_id IN (SELECT id FROM owned_competitions) AS \"canProcess!\"\nFROM pending"};

/**
 * Query generated from SQL:
 * ```
 * WITH pending AS MATERIALIZED (
 *   SELECT f.id, f.federation, f.kind, f.key, f.reference_date::text, jr.url, jr.http_status, jr.error, jrc.content,
 *     c.id AS competition_id
 *   FROM crawler.frontier f
 *   JOIN crawler.json_response jr ON jr.id = f.last_successful_response_id
 *   JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
 *   LEFT JOIN federated.competition c
 *     ON c.federation = 'wdsf' AND c.external_id = CASE
 *       WHEN f.federation = 'wdsf' AND f.kind = 'participant'
 *       THEN jrc.content->>'competitionId'
 *     END
 *   WHERE process_status = 'pending' AND fetch_status = 'ok'
 *   ORDER BY discovered_at, last_fetched_at
 *   FOR UPDATE OF f SKIP LOCKED
 *   LIMIT :limit
 * ), owned_competitions AS MATERIALIZED (
 *   SELECT c.id FROM federated.competition c
 *   WHERE c.id IN (SELECT competition_id FROM pending)
 *   ORDER BY c.id
 *   FOR NO KEY UPDATE SKIP LOCKED
 * )
 * SELECT id AS "id!", federation AS "federation!", kind AS "kind!", key AS "key!",
 *   reference_date, url, http_status, error, content,
 *   competition_id IS NULL OR competition_id IN (SELECT id FROM owned_competitions) AS "canProcess!"
 * FROM pending
 * ```
 */
export const getNextPendingProcess = new PreparedQuery<IGetNextPendingProcessParams,IGetNextPendingProcessResult>(getNextPendingProcessIR);


/** 'GetPendingProcessById' parameters type */
export interface IGetPendingProcessByIdParams {
  id?: NumberOrString | null | void;
}

/** 'GetPendingProcessById' return type */
export interface IGetPendingProcessByIdResult {
  content: Json;
  federation: string;
  key: string;
  kind: string;
  reference_date: string | null;
}

/** 'GetPendingProcessById' query type */
export interface IGetPendingProcessByIdQuery {
  params: IGetPendingProcessByIdParams;
  result: IGetPendingProcessByIdResult;
}

const getPendingProcessByIdIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":324,"b":326}]}],"statement":"SELECT\n  f.federation AS \"federation!\",\n  f.kind AS \"kind!\",\n  f.key AS \"key!\",\n  f.reference_date::text AS \"reference_date?\",\n  jrc.content\nFROM crawler.frontier f\nJOIN crawler.json_response jr ON jr.id = f.last_successful_response_id\nJOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash\nWHERE f.id = :id::bigint\n  AND process_status = 'pending'\n  AND fetch_status = 'ok'\nFOR UPDATE OF f SKIP LOCKED"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   f.federation AS "federation!",
 *   f.kind AS "kind!",
 *   f.key AS "key!",
 *   f.reference_date::text AS "reference_date?",
 *   jrc.content
 * FROM crawler.frontier f
 * JOIN crawler.json_response jr ON jr.id = f.last_successful_response_id
 * JOIN crawler.json_response_cache jrc ON jr.content_hash = jrc.content_hash
 * WHERE f.id = :id::bigint
 *   AND process_status = 'pending'
 *   AND fetch_status = 'ok'
 * FOR UPDATE OF f SKIP LOCKED
 * ```
 */
export const getPendingProcessById = new PreparedQuery<IGetPendingProcessByIdParams,IGetPendingProcessByIdResult>(getPendingProcessByIdIR);


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

const markFrontierFetchErrorIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":286,"b":288}]}],"statement":"UPDATE crawler.frontier\nSET last_fetched_at = clock_timestamp(),\n    fetch_status    = 'error',\n    error_count     = error_count + 1,\n    next_fetch_at   = now() + least(\n      300::numeric,\n      power(2::numeric, least(error_count, 9) + 1) * 5\n    ) * interval '1 second'\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET last_fetched_at = clock_timestamp(),
 *     fetch_status    = 'error',
 *     error_count     = error_count + 1,
 *     next_fetch_at   = now() + least(
 *       300::numeric,
 *       power(2::numeric, least(error_count, 9) + 1) * 5
 *     ) * interval '1 second'
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

const markFrontierTransientIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":291,"b":293}]}],"statement":"UPDATE crawler.frontier\nSET last_fetched_at = clock_timestamp(),\n    fetch_status    = 'transient',\n    error_count     = error_count + 1,\n    next_fetch_at   = now() + least(\n      1800::numeric,\n      power(2::numeric, least(error_count, 9) + 1) * 5\n    ) * interval '1 second'\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET last_fetched_at = clock_timestamp(),
 *     fetch_status    = 'transient',
 *     error_count     = error_count + 1,
 *     next_fetch_at   = now() + least(
 *       1800::numeric,
 *       power(2::numeric, least(error_count, 9) + 1) * 5
 *     ) * interval '1 second'
 * WHERE id = :id::bigint
 * ```
 */
export const markFrontierTransient = new PreparedQuery<IMarkFrontierTransientParams,IMarkFrontierTransientResult>(markFrontierTransientIR);


/** 'MarkFrontierFetchSuccess' parameters type */
export interface IMarkFrontierFetchSuccessParams {
  fetchStatus?: fetch_status | null | void;
  id?: NumberOrString | null | void;
}

/** 'MarkFrontierFetchSuccess' return type */
export type IMarkFrontierFetchSuccessResult = void;

/** 'MarkFrontierFetchSuccess' query type */
export interface IMarkFrontierFetchSuccessQuery {
  params: IMarkFrontierFetchSuccessParams;
  result: IMarkFrontierFetchSuccessResult;
}

const markFrontierFetchSuccessIR: any = {"usedParamSet":{"fetchStatus":true,"id":true},"params":[{"name":"fetchStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":72,"b":83},{"a":145,"b":156}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":485,"b":487}]}],"statement":"UPDATE crawler.frontier\nSET last_fetched_at = now(),\n    fetch_status = :fetchStatus::crawler.fetch_status,\n    process_status = CASE\n      WHEN :fetchStatus::crawler.fetch_status = 'gone'::crawler.fetch_status\n      THEN 'ok'::crawler.process_status\n      ELSE 'pending'::crawler.process_status\n    END,\n    last_process_error = NULL,\n    last_process_error_at = NULL,\n    error_count = 0,\n    next_fetch_at = crawler.next_fetch_at(federation, kind, reference_date, now())\nWHERE id = :id::bigint"};

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
 *     next_fetch_at = crawler.next_fetch_at(federation, kind, reference_date, now())
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


/** 'ReprocessWdsfParticipantIndex' parameters type */
export interface IReprocessWdsfParticipantIndexParams {
  competitionExternalId?: string | null | void;
  referenceDate?: DateOrString | null | void;
}

/** 'ReprocessWdsfParticipantIndex' return type */
export type IReprocessWdsfParticipantIndexResult = void;

/** 'ReprocessWdsfParticipantIndex' query type */
export interface IReprocessWdsfParticipantIndexQuery {
  params: IReprocessWdsfParticipantIndexParams;
  result: IReprocessWdsfParticipantIndexResult;
}

const reprocessWdsfParticipantIndexIR: any = {"usedParamSet":{"competitionExternalId":true,"referenceDate":true},"params":[{"name":"competitionExternalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":121,"b":142}]},{"name":"referenceDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":209,"b":222},{"a":273,"b":286}]}],"statement":"UPDATE crawler.frontier\nSET process_status = 'pending'\nWHERE federation = 'wdsf' AND kind = 'participantIndex' AND key = :competitionExternalId\n  AND fetch_status = 'ok' AND process_status <> 'pending'\n  AND (:referenceDate::date IS NULL OR reference_date IS DISTINCT FROM :referenceDate::date)"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET process_status = 'pending'
 * WHERE federation = 'wdsf' AND kind = 'participantIndex' AND key = :competitionExternalId
 *   AND fetch_status = 'ok' AND process_status <> 'pending'
 *   AND (:referenceDate::date IS NULL OR reference_date IS DISTINCT FROM :referenceDate::date)
 * ```
 */
export const reprocessWdsfParticipantIndex = new PreparedQuery<IReprocessWdsfParticipantIndexParams,IReprocessWdsfParticipantIndexResult>(reprocessWdsfParticipantIndexIR);


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
  fetchStatus?: fetch_status | null | void;
  httpStatus?: number | null | void;
  id?: NumberOrString | null | void;
  url?: string | null | void;
}

/** 'InsertResponse' return type */
export interface IInsertResponseResult {
  id: string;
}

/** 'InsertResponse' query type */
export interface IInsertResponseQuery {
  params: IInsertResponseParams;
  result: IInsertResponseResult;
}

const insertResponseIR: any = {"usedParamSet":{"id":true,"url":true,"httpStatus":true,"error":true,"content":true,"fetchStatus":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":33,"b":35}]},{"name":"url","required":false,"transform":{"type":"scalar"},"locs":[{"a":46,"b":49}]},{"name":"httpStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":58,"b":68}]},{"name":"error","required":false,"transform":{"type":"scalar"},"locs":[{"a":80,"b":85}]},{"name":"content","required":false,"transform":{"type":"scalar"},"locs":[{"a":96,"b":103}]},{"name":"fetchStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":119,"b":130}]}],"statement":"SELECT crawler.store_response(\n  :id::bigint, :url::text, :httpStatus::integer, :error::text,\n  :content::text::jsonb, :fetchStatus::crawler.fetch_status\n) AS \"id!\""};

/**
 * Query generated from SQL:
 * ```
 * SELECT crawler.store_response(
 *   :id::bigint, :url::text, :httpStatus::integer, :error::text,
 *   :content::text::jsonb, :fetchStatus::crawler.fetch_status
 * ) AS "id!"
 * ```
 */
export const insertResponse = new PreparedQuery<IInsertResponseParams,IInsertResponseResult>(insertResponseIR);


/** 'UpsertFrontiers' parameters type */
export interface IUpsertFrontiersParams {
  federations?: stringArray | null | void;
  keys?: stringArray | null | void;
  kinds?: stringArray | null | void;
  referenceDates?: stringArray | null | void;
}

/** 'UpsertFrontiers' return type */
export type IUpsertFrontiersResult = void;

/** 'UpsertFrontiers' query type */
export interface IUpsertFrontiersQuery {
  params: IUpsertFrontiersParams;
  result: IUpsertFrontiersResult;
}

const upsertFrontiersIR: any = {"usedParamSet":{"federations":true,"kinds":true,"keys":true,"referenceDates":true},"params":[{"name":"federations","required":false,"transform":{"type":"scalar"},"locs":[{"a":118,"b":129}]},{"name":"kinds","required":false,"transform":{"type":"scalar"},"locs":[{"a":144,"b":149}]},{"name":"keys","required":false,"transform":{"type":"scalar"},"locs":[{"a":164,"b":168}]},{"name":"referenceDates","required":false,"transform":{"type":"scalar"},"locs":[{"a":183,"b":197}]}],"statement":"WITH input AS (\n  SELECT federation, kind, key, nullif(reference_date, '')::date AS reference_date\n  FROM unnest(\n    :federations::text[],\n    :kinds::text[],\n    :keys::text[],\n    :referenceDates::text[]\n  ) AS rows(federation, kind, key, reference_date)\n)\nINSERT INTO crawler.frontier AS f (federation, kind, key, reference_date)\nSELECT federation, kind, key, reference_date\nFROM input\nWHERE NOT EXISTS (\n  SELECT 1 FROM crawler.frontier existing\n  WHERE (existing.federation, existing.kind, existing.key) = (input.federation, input.kind, input.key)\n    AND existing.process_status <> 'error' AND existing.fetch_status <> 'gone'\n    AND (input.reference_date IS NULL OR existing.reference_date IS NOT DISTINCT FROM input.reference_date)\n)\nORDER BY federation, kind, key\nON CONFLICT (federation, kind, key) DO UPDATE\nSET reference_date = coalesce(EXCLUDED.reference_date, f.reference_date),\n    process_status = CASE WHEN f.process_status = 'error' OR f.fetch_status = 'gone'\n      THEN 'pending'::crawler.process_status ELSE f.process_status END,\n    fetch_status = CASE f.fetch_status WHEN 'gone' THEN 'pending' ELSE f.fetch_status END,\n    next_fetch_at = CASE f.fetch_status WHEN 'gone' THEN now() ELSE f.next_fetch_at END\nWHERE f.process_status = 'error'\n   OR f.fetch_status = 'gone'\n   OR (EXCLUDED.reference_date IS NOT NULL AND f.reference_date IS DISTINCT FROM EXCLUDED.reference_date)"};

/**
 * Query generated from SQL:
 * ```
 * WITH input AS (
 *   SELECT federation, kind, key, nullif(reference_date, '')::date AS reference_date
 *   FROM unnest(
 *     :federations::text[],
 *     :kinds::text[],
 *     :keys::text[],
 *     :referenceDates::text[]
 *   ) AS rows(federation, kind, key, reference_date)
 * )
 * INSERT INTO crawler.frontier AS f (federation, kind, key, reference_date)
 * SELECT federation, kind, key, reference_date
 * FROM input
 * WHERE NOT EXISTS (
 *   SELECT 1 FROM crawler.frontier existing
 *   WHERE (existing.federation, existing.kind, existing.key) = (input.federation, input.kind, input.key)
 *     AND existing.process_status <> 'error' AND existing.fetch_status <> 'gone'
 *     AND (input.reference_date IS NULL OR existing.reference_date IS NOT DISTINCT FROM input.reference_date)
 * )
 * ORDER BY federation, kind, key
 * ON CONFLICT (federation, kind, key) DO UPDATE
 * SET reference_date = coalesce(EXCLUDED.reference_date, f.reference_date),
 *     process_status = CASE WHEN f.process_status = 'error' OR f.fetch_status = 'gone'
 *       THEN 'pending'::crawler.process_status ELSE f.process_status END,
 *     fetch_status = CASE f.fetch_status WHEN 'gone' THEN 'pending' ELSE f.fetch_status END,
 *     next_fetch_at = CASE f.fetch_status WHEN 'gone' THEN now() ELSE f.next_fetch_at END
 * WHERE f.process_status = 'error'
 *    OR f.fetch_status = 'gone'
 *    OR (EXCLUDED.reference_date IS NOT NULL AND f.reference_date IS DISTINCT FROM EXCLUDED.reference_date)
 * ```
 */
export const upsertFrontiers = new PreparedQuery<IUpsertFrontiersParams,IUpsertFrontiersResult>(upsertFrontiersIR);


/** 'SetFrontierReferenceDate' parameters type */
export interface ISetFrontierReferenceDateParams {
  federation: string;
  key: string;
  kind: string;
  referenceDate: DateOrString;
}

/** 'SetFrontierReferenceDate' return type */
export type ISetFrontierReferenceDateResult = void;

/** 'SetFrontierReferenceDate' query type */
export interface ISetFrontierReferenceDateQuery {
  params: ISetFrontierReferenceDateParams;
  result: ISetFrontierReferenceDateResult;
}

const setFrontierReferenceDateIR: any = {"usedParamSet":{"referenceDate":true,"federation":true,"kind":true,"key":true},"params":[{"name":"referenceDate","required":true,"transform":{"type":"scalar"},"locs":[{"a":45,"b":59},{"a":167,"b":181}]},{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":100,"b":111}]},{"name":"kind","required":true,"transform":{"type":"scalar"},"locs":[{"a":114,"b":119}]},{"name":"key","required":true,"transform":{"type":"scalar"},"locs":[{"a":122,"b":126}]}],"statement":"UPDATE crawler.frontier SET reference_date = :referenceDate!::date\nWHERE (federation, kind, key) = (:federation!, :kind!, :key!)\n  AND reference_date IS DISTINCT FROM :referenceDate!::date"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier SET reference_date = :referenceDate!::date
 * WHERE (federation, kind, key) = (:federation!, :kind!, :key!)
 *   AND reference_date IS DISTINCT FROM :referenceDate!::date
 * ```
 */
export const setFrontierReferenceDate = new PreparedQuery<ISetFrontierReferenceDateParams,ISetFrontierReferenceDateResult>(setFrontierReferenceDateIR);


