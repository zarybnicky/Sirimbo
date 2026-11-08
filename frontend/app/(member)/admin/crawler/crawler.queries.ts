/** Types generated for queries found in "app/(member)/admin/crawler/crawler.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type NumberOrString = number | string;

/** 'RefetchCrawlerErrors' parameters type */
export interface IRefetchCrawlerErrorsParams {
  errorFingerprint?: string | null | void;
  failure?: string | null | void;
  federation: string;
  httpStatus?: number | null | void;
  keyContains: string;
  kind: string;
}

/** 'RefetchCrawlerErrors' return type */
export interface IRefetchCrawlerErrorsResult {
  count: number;
}

/** 'RefetchCrawlerErrors' query type */
export interface IRefetchCrawlerErrorsQuery {
  params: IRefetchCrawlerErrorsParams;
  result: IRefetchCrawlerErrorsResult;
}

const refetchCrawlerErrorsIR: any = {"usedParamSet":{"federation":true,"kind":true,"keyContains":true,"failure":true,"httpStatus":true,"errorFingerprint":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":255,"b":266}]},{"name":"kind","required":true,"transform":{"type":"scalar"},"locs":[{"a":285,"b":290}]},{"name":"keyContains","required":true,"transform":{"type":"scalar"},"locs":[{"a":314,"b":326}]},{"name":"failure","required":false,"transform":{"type":"scalar"},"locs":[{"a":430,"b":437},{"a":575,"b":582}]},{"name":"httpStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":634,"b":644}]},{"name":"errorFingerprint","required":false,"transform":{"type":"scalar"},"locs":[{"a":756,"b":772}]}],"statement":"WITH updated AS (\n  UPDATE crawler.frontier f\n  SET fetch_status = 'pending',\n      process_status = 'pending',\n      last_process_error = NULL,\n      last_process_error_at = NULL,\n      error_count = 0,\n      next_fetch_at = now()\n  WHERE f.federation = :federation!\n    AND f.kind = :kind!\n    AND strpos(f.key, :keyContains!) > 0\n    AND (f.fetch_status IN ('error', 'transient') OR f.process_status = 'error')\n    AND (\n      :failure::text IS NULL\n      OR EXISTS (\n        SELECT 1 FROM crawler.frontier_failure ff\n        WHERE ff.id = f.id\n          AND ff.failure = :failure\n          AND ff.http_status IS NOT DISTINCT FROM :httpStatus::integer\n          AND left(regexp_replace(coalesce(nullif(ff.error_text, ''), '-'), '\\s+', ' ', 'g'), 180) = :errorFingerprint::text\n      )\n    )\n  RETURNING f.id\n)\nSELECT count(*)::int AS \"count!\" FROM updated"};

/**
 * Query generated from SQL:
 * ```
 * WITH updated AS (
 *   UPDATE crawler.frontier f
 *   SET fetch_status = 'pending',
 *       process_status = 'pending',
 *       last_process_error = NULL,
 *       last_process_error_at = NULL,
 *       error_count = 0,
 *       next_fetch_at = now()
 *   WHERE f.federation = :federation!
 *     AND f.kind = :kind!
 *     AND strpos(f.key, :keyContains!) > 0
 *     AND (f.fetch_status IN ('error', 'transient') OR f.process_status = 'error')
 *     AND (
 *       :failure::text IS NULL
 *       OR EXISTS (
 *         SELECT 1 FROM crawler.frontier_failure ff
 *         WHERE ff.id = f.id
 *           AND ff.failure = :failure
 *           AND ff.http_status IS NOT DISTINCT FROM :httpStatus::integer
 *           AND left(regexp_replace(coalesce(nullif(ff.error_text, ''), '-'), '\s+', ' ', 'g'), 180) = :errorFingerprint::text
 *       )
 *     )
 *   RETURNING f.id
 * )
 * SELECT count(*)::int AS "count!" FROM updated
 * ```
 */
export const refetchCrawlerErrors = new PreparedQuery<IRefetchCrawlerErrorsParams,IRefetchCrawlerErrorsResult>(refetchCrawlerErrorsIR);


/** 'ReprocessCrawlerTargets' parameters type */
export interface IReprocessCrawlerTargetsParams {
  errorFingerprint?: string | null | void;
  failure?: string | null | void;
  federation: string;
  httpStatus?: number | null | void;
  keyContains: string;
  kind: string;
}

/** 'ReprocessCrawlerTargets' return type */
export interface IReprocessCrawlerTargetsResult {
  count: number;
}

/** 'ReprocessCrawlerTargets' query type */
export interface IReprocessCrawlerTargetsQuery {
  params: IReprocessCrawlerTargetsParams;
  result: IReprocessCrawlerTargetsResult;
}

const reprocessCrawlerTargetsIR: any = {"usedParamSet":{"federation":true,"kind":true,"keyContains":true,"failure":true,"httpStatus":true,"errorFingerprint":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":171,"b":182}]},{"name":"kind","required":true,"transform":{"type":"scalar"},"locs":[{"a":201,"b":206}]},{"name":"keyContains","required":true,"transform":{"type":"scalar"},"locs":[{"a":230,"b":242}]},{"name":"failure","required":false,"transform":{"type":"scalar"},"locs":[{"a":333,"b":340},{"a":478,"b":485}]},{"name":"httpStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":537,"b":547}]},{"name":"errorFingerprint","required":false,"transform":{"type":"scalar"},"locs":[{"a":659,"b":675}]}],"statement":"WITH updated AS (\n  UPDATE crawler.frontier f\n  SET process_status = 'pending',\n      last_process_error = NULL,\n      last_process_error_at = NULL\n  WHERE f.federation = :federation!\n    AND f.kind = :kind!\n    AND strpos(f.key, :keyContains!) > 0\n    AND f.fetch_status = 'ok'\n    AND f.process_status <> 'pending'\n    AND (\n      :failure::text IS NULL\n      OR EXISTS (\n        SELECT 1 FROM crawler.frontier_failure ff\n        WHERE ff.id = f.id\n          AND ff.failure = :failure\n          AND ff.http_status IS NOT DISTINCT FROM :httpStatus::integer\n          AND left(regexp_replace(coalesce(nullif(ff.error_text, ''), '-'), '\\s+', ' ', 'g'), 180) = :errorFingerprint::text\n      )\n    )\n  RETURNING f.id\n)\nSELECT count(*)::int AS \"count!\" FROM updated"};

/**
 * Query generated from SQL:
 * ```
 * WITH updated AS (
 *   UPDATE crawler.frontier f
 *   SET process_status = 'pending',
 *       last_process_error = NULL,
 *       last_process_error_at = NULL
 *   WHERE f.federation = :federation!
 *     AND f.kind = :kind!
 *     AND strpos(f.key, :keyContains!) > 0
 *     AND f.fetch_status = 'ok'
 *     AND f.process_status <> 'pending'
 *     AND (
 *       :failure::text IS NULL
 *       OR EXISTS (
 *         SELECT 1 FROM crawler.frontier_failure ff
 *         WHERE ff.id = f.id
 *           AND ff.failure = :failure
 *           AND ff.http_status IS NOT DISTINCT FROM :httpStatus::integer
 *           AND left(regexp_replace(coalesce(nullif(ff.error_text, ''), '-'), '\s+', ' ', 'g'), 180) = :errorFingerprint::text
 *       )
 *     )
 *   RETURNING f.id
 * )
 * SELECT count(*)::int AS "count!" FROM updated
 * ```
 */
export const reprocessCrawlerTargets = new PreparedQuery<IReprocessCrawlerTargetsParams,IReprocessCrawlerTargetsResult>(reprocessCrawlerTargetsIR);


/** 'ScheduleCrawler' parameters type */
export type IScheduleCrawlerParams = void;

/** 'ScheduleCrawler' return type */
export interface IScheduleCrawlerResult {
  id: string | null;
}

/** 'ScheduleCrawler' query type */
export interface IScheduleCrawlerQuery {
  params: IScheduleCrawlerParams;
  result: IScheduleCrawlerResult;
}

const scheduleCrawlerIR: any = {"usedParamSet":{},"params":[],"statement":"SELECT (graphile_worker.add_job(\n  identifier => 'frontier_schedule',\n  payload => '{}'::json,\n  run_at => now(),\n  job_key => 'frontier_schedule',\n  job_key_mode => 'replace'\n)).id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT (graphile_worker.add_job(
 *   identifier => 'frontier_schedule',
 *   payload => '{}'::json,
 *   run_at => now(),
 *   job_key => 'frontier_schedule',
 *   job_key_mode => 'replace'
 * )).id
 * ```
 */
export const scheduleCrawler = new PreparedQuery<IScheduleCrawlerParams,IScheduleCrawlerResult>(scheduleCrawlerIR);


/** 'CompleteCrawlerJob' parameters type */
export interface ICompleteCrawlerJobParams {
  jobId: NumberOrString;
}

/** 'CompleteCrawlerJob' return type */
export interface ICompleteCrawlerJobResult {
  id: string;
}

/** 'CompleteCrawlerJob' query type */
export interface ICompleteCrawlerJobQuery {
  params: ICompleteCrawlerJobParams;
  result: ICompleteCrawlerJobResult;
}

const completeCrawlerJobIR: any = {"usedParamSet":{"jobId":true},"params":[{"name":"jobId","required":true,"transform":{"type":"scalar"},"locs":[{"a":177,"b":183}]}],"statement":"-- complete_jobs() skips locked jobs; this action also removes those.\nDELETE FROM graphile_worker._private_jobs\nWHERE id IN (\n  SELECT id FROM graphile_worker.jobs\n  WHERE id = :jobId!::bigint\n    AND task_identifier = 'frontier_fetch'\n)\nRETURNING id"};

/**
 * Query generated from SQL:
 * ```
 * -- complete_jobs() skips locked jobs; this action also removes those.
 * DELETE FROM graphile_worker._private_jobs
 * WHERE id IN (
 *   SELECT id FROM graphile_worker.jobs
 *   WHERE id = :jobId!::bigint
 *     AND task_identifier = 'frontier_fetch'
 * )
 * RETURNING id
 * ```
 */
export const completeCrawlerJob = new PreparedQuery<ICompleteCrawlerJobParams,ICompleteCrawlerJobResult>(completeCrawlerJobIR);


/** 'GetRefetchOutlook' parameters type */
export type IGetRefetchOutlookParams = void;

/** 'GetRefetchOutlook' return type */
export interface IGetRefetchOutlookResult {
  count: number;
  day: string | null;
  federation: string | null;
}

/** 'GetRefetchOutlook' query type */
export interface IGetRefetchOutlookQuery {
  params: IGetRefetchOutlookParams;
  result: IGetRefetchOutlookResult;
}

const getRefetchOutlookIR: any = {"usedParamSet":{},"params":[],"statement":"WITH bounds AS (\n  SELECT (now() AT TIME ZONE 'UTC')::date AS today\n), counts AS (\n  SELECT\n    CASE WHEN next_fetch_at >= now()\n      THEN (next_fetch_at AT TIME ZONE 'UTC')::date\n    END AS day,\n    federation,\n    count(*)::int AS count\n  FROM crawler.frontier, bounds\n  WHERE fetch_status = 'ok' AND process_status = 'ok'\n    AND next_fetch_at < (today + 30)::timestamp AT TIME ZONE 'UTC'\n  GROUP BY 1, 2\n), buckets AS (\n  SELECT NULL::date AS day\n  UNION ALL\n  SELECT today + offset_day FROM bounds, generate_series(0, 29) AS offset_day\n)\nSELECT buckets.day::text AS \"day?\", counts.federation AS \"federation?\", coalesce(counts.count, 0)::int AS \"count!\"\nFROM buckets\nLEFT JOIN counts ON counts.day IS NOT DISTINCT FROM buckets.day\nORDER BY buckets.day NULLS FIRST, counts.federation"};

/**
 * Query generated from SQL:
 * ```
 * WITH bounds AS (
 *   SELECT (now() AT TIME ZONE 'UTC')::date AS today
 * ), counts AS (
 *   SELECT
 *     CASE WHEN next_fetch_at >= now()
 *       THEN (next_fetch_at AT TIME ZONE 'UTC')::date
 *     END AS day,
 *     federation,
 *     count(*)::int AS count
 *   FROM crawler.frontier, bounds
 *   WHERE fetch_status = 'ok' AND process_status = 'ok'
 *     AND next_fetch_at < (today + 30)::timestamp AT TIME ZONE 'UTC'
 *   GROUP BY 1, 2
 * ), buckets AS (
 *   SELECT NULL::date AS day
 *   UNION ALL
 *   SELECT today + offset_day FROM bounds, generate_series(0, 29) AS offset_day
 * )
 * SELECT buckets.day::text AS "day?", counts.federation AS "federation?", coalesce(counts.count, 0)::int AS "count!"
 * FROM buckets
 * LEFT JOIN counts ON counts.day IS NOT DISTINCT FROM buckets.day
 * ORDER BY buckets.day NULLS FIRST, counts.federation
 * ```
 */
export const getRefetchOutlook = new PreparedQuery<IGetRefetchOutlookParams,IGetRefetchOutlookResult>(getRefetchOutlookIR);


/** 'GetRefetchPolicies' parameters type */
export type IGetRefetchPoliciesParams = void;

/** 'GetRefetchPolicies' return type */
export interface IGetRefetchPoliciesResult {
  federation: string | null;
  fetchInterval: string | null;
  frontierCount: number;
  kind: string | null;
  offsetDays: number | null;
  offsetLabel: string | null;
}

/** 'GetRefetchPolicies' query type */
export interface IGetRefetchPoliciesQuery {
  params: IGetRefetchPoliciesParams;
  result: IGetRefetchPoliciesResult;
}

const getRefetchPoliciesIR: any = {"usedParamSet":{},"params":[],"statement":"WITH counts AS (\n  SELECT federation, kind, count(*)::int AS count\n  FROM crawler.frontier\n  GROUP BY federation, kind\n), kinds AS (\n  SELECT federation, kind FROM counts\n  UNION\n  SELECT federation, kind FROM crawler.refetch_policy\n)\nSELECT k.federation, k.kind,\n  coalesce(c.count, 0)::int AS \"frontierCount!\",\n  p.reference_offset::text AS \"offsetLabel?\",\n  (extract(epoch FROM p.reference_offset) / 86400)::float8 AS \"offsetDays?\",\n  p.fetch_interval::text AS \"fetchInterval?\"\nFROM kinds k\nLEFT JOIN counts c USING (federation, kind)\nLEFT JOIN crawler.refetch_policy p USING (federation, kind)\nORDER BY coalesce(c.count, 0) DESC, k.federation, k.kind, p.reference_offset NULLS FIRST"};

/**
 * Query generated from SQL:
 * ```
 * WITH counts AS (
 *   SELECT federation, kind, count(*)::int AS count
 *   FROM crawler.frontier
 *   GROUP BY federation, kind
 * ), kinds AS (
 *   SELECT federation, kind FROM counts
 *   UNION
 *   SELECT federation, kind FROM crawler.refetch_policy
 * )
 * SELECT k.federation, k.kind,
 *   coalesce(c.count, 0)::int AS "frontierCount!",
 *   p.reference_offset::text AS "offsetLabel?",
 *   (extract(epoch FROM p.reference_offset) / 86400)::float8 AS "offsetDays?",
 *   p.fetch_interval::text AS "fetchInterval?"
 * FROM kinds k
 * LEFT JOIN counts c USING (federation, kind)
 * LEFT JOIN crawler.refetch_policy p USING (federation, kind)
 * ORDER BY coalesce(c.count, 0) DESC, k.federation, k.kind, p.reference_offset NULLS FIRST
 * ```
 */
export const getRefetchPolicies = new PreparedQuery<IGetRefetchPoliciesParams,IGetRefetchPoliciesResult>(getRefetchPoliciesIR);


/** 'CreateRefetchPolicy' parameters type */
export interface ICreateRefetchPolicyParams {
  federation: string;
  fetchInterval: string;
  kind: string;
  offset: string;
}

/** 'CreateRefetchPolicy' return type */
export interface ICreateRefetchPolicyResult {
  federation: string;
}

/** 'CreateRefetchPolicy' query type */
export interface ICreateRefetchPolicyQuery {
  params: ICreateRefetchPolicyParams;
  result: ICreateRefetchPolicyResult;
}

const createRefetchPolicyIR: any = {"usedParamSet":{"federation":true,"kind":true,"offset":true,"fetchInterval":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":96,"b":107}]},{"name":"kind","required":true,"transform":{"type":"scalar"},"locs":[{"a":110,"b":115}]},{"name":"offset","required":true,"transform":{"type":"scalar"},"locs":[{"a":118,"b":125}]},{"name":"fetchInterval","required":true,"transform":{"type":"scalar"},"locs":[{"a":144,"b":158}]}],"statement":"INSERT INTO crawler.refetch_policy (federation, kind, reference_offset, fetch_interval)\nVALUES (:federation!, :kind!, :offset!::text::interval, :fetchInterval!::text::interval)\nON CONFLICT DO NOTHING\nRETURNING federation"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO crawler.refetch_policy (federation, kind, reference_offset, fetch_interval)
 * VALUES (:federation!, :kind!, :offset!::text::interval, :fetchInterval!::text::interval)
 * ON CONFLICT DO NOTHING
 * RETURNING federation
 * ```
 */
export const createRefetchPolicy = new PreparedQuery<ICreateRefetchPolicyParams,ICreateRefetchPolicyResult>(createRefetchPolicyIR);


/** 'UpdateRefetchPolicy' parameters type */
export interface IUpdateRefetchPolicyParams {
  federation: string;
  fetchInterval: string;
  kind: string;
  newFetchInterval: string;
  newOffset: string;
  offset?: string | null | void;
}

/** 'UpdateRefetchPolicy' return type */
export interface IUpdateRefetchPolicyResult {
  federation: string;
}

/** 'UpdateRefetchPolicy' query type */
export interface IUpdateRefetchPolicyQuery {
  params: IUpdateRefetchPolicyParams;
  result: IUpdateRefetchPolicyResult;
}

const updateRefetchPolicyIR: any = {"usedParamSet":{"newOffset":true,"newFetchInterval":true,"federation":true,"kind":true,"offset":true,"fetchInterval":true},"params":[{"name":"newOffset","required":true,"transform":{"type":"scalar"},"locs":[{"a":103,"b":113}]},{"name":"newFetchInterval","required":true,"transform":{"type":"scalar"},"locs":[{"a":157,"b":174}]},{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":211,"b":222}]},{"name":"kind","required":true,"transform":{"type":"scalar"},"locs":[{"a":235,"b":240}]},{"name":"offset","required":false,"transform":{"type":"scalar"},"locs":[{"a":286,"b":292}]},{"name":"fetchInterval","required":true,"transform":{"type":"scalar"},"locs":[{"a":333,"b":347}]}],"statement":"UPDATE crawler.refetch_policy\nSET reference_offset = CASE WHEN reference_offset IS NULL THEN NULL ELSE :newOffset!::text::interval END,\n    fetch_interval = :newFetchInterval!::text::interval\nWHERE federation = :federation! AND kind = :kind!\n  AND reference_offset IS NOT DISTINCT FROM :offset::text::interval\n  AND fetch_interval = :fetchInterval!::text::interval\nRETURNING federation"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.refetch_policy
 * SET reference_offset = CASE WHEN reference_offset IS NULL THEN NULL ELSE :newOffset!::text::interval END,
 *     fetch_interval = :newFetchInterval!::text::interval
 * WHERE federation = :federation! AND kind = :kind!
 *   AND reference_offset IS NOT DISTINCT FROM :offset::text::interval
 *   AND fetch_interval = :fetchInterval!::text::interval
 * RETURNING federation
 * ```
 */
export const updateRefetchPolicy = new PreparedQuery<IUpdateRefetchPolicyParams,IUpdateRefetchPolicyResult>(updateRefetchPolicyIR);


/** 'RemoveRefetchPolicy' parameters type */
export interface IRemoveRefetchPolicyParams {
  federation: string;
  kind: string;
  offset: string;
}

/** 'RemoveRefetchPolicy' return type */
export type IRemoveRefetchPolicyResult = void;

/** 'RemoveRefetchPolicy' query type */
export interface IRemoveRefetchPolicyQuery {
  params: IRemoveRefetchPolicyParams;
  result: IRemoveRefetchPolicyResult;
}

const removeRefetchPolicyIR: any = {"usedParamSet":{"federation":true,"kind":true,"offset":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":54,"b":65}]},{"name":"kind","required":true,"transform":{"type":"scalar"},"locs":[{"a":78,"b":83}]},{"name":"offset","required":true,"transform":{"type":"scalar"},"locs":[{"a":110,"b":117}]}],"statement":"DELETE FROM crawler.refetch_policy\nWHERE federation = :federation! AND kind = :kind!\n  AND reference_offset = :offset!::text::interval"};

/**
 * Query generated from SQL:
 * ```
 * DELETE FROM crawler.refetch_policy
 * WHERE federation = :federation! AND kind = :kind!
 *   AND reference_offset = :offset!::text::interval
 * ```
 */
export const removeRefetchPolicy = new PreparedQuery<IRemoveRefetchPolicyParams,IRemoveRefetchPolicyResult>(removeRefetchPolicyIR);


/** 'RecalculateCrawlerFetchTimes' parameters type */
export interface IRecalculateCrawlerFetchTimesParams {
  federation: string;
  kind: string;
}

/** 'RecalculateCrawlerFetchTimes' return type */
export interface IRecalculateCrawlerFetchTimesResult {
  count: number;
}

/** 'RecalculateCrawlerFetchTimes' query type */
export interface IRecalculateCrawlerFetchTimesQuery {
  params: IRecalculateCrawlerFetchTimesParams;
  result: IRecalculateCrawlerFetchTimesResult;
}

const recalculateCrawlerFetchTimesIR: any = {"usedParamSet":{"federation":true,"kind":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":39,"b":50}]},{"name":"kind","required":true,"transform":{"type":"scalar"},"locs":[{"a":53,"b":58}]}],"statement":"SELECT crawler.recalculate_fetch_times(:federation!, :kind!) AS \"count!\""};

/**
 * Query generated from SQL:
 * ```
 * SELECT crawler.recalculate_fetch_times(:federation!, :kind!) AS "count!"
 * ```
 */
export const recalculateCrawlerFetchTimes = new PreparedQuery<IRecalculateCrawlerFetchTimesParams,IRecalculateCrawlerFetchTimesResult>(recalculateCrawlerFetchTimesIR);


