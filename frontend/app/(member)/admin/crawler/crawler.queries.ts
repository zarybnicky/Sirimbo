/** Types generated for queries found in "app/(member)/admin/crawler/crawler.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type NumberOrString = number | string;

/** 'RefetchCrawlerTargets' parameters type */
export interface IRefetchCrawlerTargetsParams {
  errorFingerprint?: string | null | void;
  failure?: string | null | void;
  federation: string;
  frontierId?: NumberOrString | null | void;
  httpStatus?: number | null | void;
  keyContains: string;
  kind: string;
}

/** 'RefetchCrawlerTargets' return type */
export interface IRefetchCrawlerTargetsResult {
  count: number;
}

/** 'RefetchCrawlerTargets' query type */
export interface IRefetchCrawlerTargetsQuery {
  params: IRefetchCrawlerTargetsParams;
  result: IRefetchCrawlerTargetsResult;
}

const refetchCrawlerTargetsIR: any = {"usedParamSet":{"federation":true,"kind":true,"keyContains":true,"frontierId":true,"failure":true,"httpStatus":true,"errorFingerprint":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":255,"b":266}]},{"name":"kind","required":true,"transform":{"type":"scalar"},"locs":[{"a":285,"b":290}]},{"name":"keyContains","required":true,"transform":{"type":"scalar"},"locs":[{"a":314,"b":326}]},{"name":"frontierId","required":false,"transform":{"type":"scalar"},"locs":[{"a":342,"b":352},{"a":380,"b":390},{"a":402,"b":412}]},{"name":"failure","required":false,"transform":{"type":"scalar"},"locs":[{"a":525,"b":532},{"a":670,"b":677}]},{"name":"httpStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":729,"b":739}]},{"name":"errorFingerprint","required":false,"transform":{"type":"scalar"},"locs":[{"a":851,"b":867}]}],"statement":"WITH updated AS (\n  UPDATE crawler.frontier f\n  SET fetch_status = 'pending',\n      process_status = 'pending',\n      last_process_error = NULL,\n      last_process_error_at = NULL,\n      error_count = 0,\n      next_fetch_at = now()\n  WHERE f.federation = :federation!\n    AND f.kind = :kind!\n    AND strpos(f.key, :keyContains!) > 0\n    AND (:frontierId::bigint IS NULL OR f.id = :frontierId)\n    AND (:frontierId::bigint IS NOT NULL OR f.fetch_status IN ('error', 'transient') OR f.process_status = 'error')\n    AND (\n      :failure::text IS NULL\n      OR EXISTS (\n        SELECT 1 FROM crawler.frontier_failure ff\n        WHERE ff.id = f.id\n          AND ff.failure = :failure\n          AND ff.http_status IS NOT DISTINCT FROM :httpStatus::integer\n          AND left(regexp_replace(coalesce(nullif(ff.error_text, ''), '-'), '\\s+', ' ', 'g'), 180) = :errorFingerprint::text\n      )\n    )\n  RETURNING f.id\n)\nSELECT count(*)::int AS \"count!\" FROM updated"};

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
 *     AND (:frontierId::bigint IS NULL OR f.id = :frontierId)
 *     AND (:frontierId::bigint IS NOT NULL OR f.fetch_status IN ('error', 'transient') OR f.process_status = 'error')
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
export const refetchCrawlerTargets = new PreparedQuery<IRefetchCrawlerTargetsParams,IRefetchCrawlerTargetsResult>(refetchCrawlerTargetsIR);


/** 'MarkCrawlerFrontierGone' parameters type */
export interface IMarkCrawlerFrontierGoneParams {
  federation: string;
  frontierId?: NumberOrString | null | void;
  kind: string;
}

/** 'MarkCrawlerFrontierGone' return type */
export interface IMarkCrawlerFrontierGoneResult {
  count: number;
}

/** 'MarkCrawlerFrontierGone' query type */
export interface IMarkCrawlerFrontierGoneQuery {
  params: IMarkCrawlerFrontierGoneParams;
  result: IMarkCrawlerFrontierGoneResult;
}

const markCrawlerFrontierGoneIR: any = {"usedParamSet":{"frontierId":true,"federation":true,"kind":true},"params":[{"name":"frontierId","required":false,"transform":{"type":"scalar"},"locs":[{"a":206,"b":216}]},{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":247,"b":258}]},{"name":"kind","required":true,"transform":{"type":"scalar"},"locs":[{"a":271,"b":276}]}],"statement":"WITH updated AS (\n  UPDATE crawler.frontier\n  SET fetch_status = 'gone',\n      process_status = 'ok',\n      last_process_error = NULL,\n      last_process_error_at = NULL,\n      error_count = 0\n  WHERE id = :frontierId::bigint\n    AND federation = :federation! AND kind = :kind!\n  RETURNING id\n)\nSELECT count(*)::int AS \"count!\" FROM updated"};

/**
 * Query generated from SQL:
 * ```
 * WITH updated AS (
 *   UPDATE crawler.frontier
 *   SET fetch_status = 'gone',
 *       process_status = 'ok',
 *       last_process_error = NULL,
 *       last_process_error_at = NULL,
 *       error_count = 0
 *   WHERE id = :frontierId::bigint
 *     AND federation = :federation! AND kind = :kind!
 *   RETURNING id
 * )
 * SELECT count(*)::int AS "count!" FROM updated
 * ```
 */
export const markCrawlerFrontierGone = new PreparedQuery<IMarkCrawlerFrontierGoneParams,IMarkCrawlerFrontierGoneResult>(markCrawlerFrontierGoneIR);


/** 'RemoveQueuedFrontierFetches' parameters type */
export interface IRemoveQueuedFrontierFetchesParams {
  id: NumberOrString;
}

/** 'RemoveQueuedFrontierFetches' return type */
export type IRemoveQueuedFrontierFetchesResult = void;

/** 'RemoveQueuedFrontierFetches' query type */
export interface IRemoveQueuedFrontierFetchesQuery {
  params: IRemoveQueuedFrontierFetchesParams;
  result: IRemoveQueuedFrontierFetchesResult;
}

const removeQueuedFrontierFetchesIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":123,"b":126}]}],"statement":"DELETE FROM graphile_worker._private_jobs j\nUSING crawler.frontier_fetch_job f\nWHERE j.id = f.job_id\n  AND f.frontier_id = :id!::bigint\n  AND j.locked_at IS NULL"};

/**
 * Query generated from SQL:
 * ```
 * DELETE FROM graphile_worker._private_jobs j
 * USING crawler.frontier_fetch_job f
 * WHERE j.id = f.job_id
 *   AND f.frontier_id = :id!::bigint
 *   AND j.locked_at IS NULL
 * ```
 */
export const removeQueuedFrontierFetches = new PreparedQuery<IRemoveQueuedFrontierFetchesParams,IRemoveQueuedFrontierFetchesResult>(removeQueuedFrontierFetchesIR);


/** 'ReprocessCrawlerTargets' parameters type */
export interface IReprocessCrawlerTargetsParams {
  errorFingerprint?: string | null | void;
  failure?: string | null | void;
  federation: string;
  frontierId?: NumberOrString | null | void;
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

const reprocessCrawlerTargetsIR: any = {"usedParamSet":{"federation":true,"kind":true,"keyContains":true,"frontierId":true,"failure":true,"httpStatus":true,"errorFingerprint":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":171,"b":182}]},{"name":"kind","required":true,"transform":{"type":"scalar"},"locs":[{"a":201,"b":206}]},{"name":"keyContains","required":true,"transform":{"type":"scalar"},"locs":[{"a":230,"b":242}]},{"name":"frontierId","required":false,"transform":{"type":"scalar"},"locs":[{"a":258,"b":268},{"a":296,"b":306},{"a":398,"b":408}]},{"name":"failure","required":false,"transform":{"type":"scalar"},"locs":[{"a":480,"b":487},{"a":625,"b":632}]},{"name":"httpStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":684,"b":694}]},{"name":"errorFingerprint","required":false,"transform":{"type":"scalar"},"locs":[{"a":806,"b":822}]}],"statement":"WITH updated AS (\n  UPDATE crawler.frontier f\n  SET process_status = 'pending',\n      last_process_error = NULL,\n      last_process_error_at = NULL\n  WHERE f.federation = :federation!\n    AND f.kind = :kind!\n    AND strpos(f.key, :keyContains!) > 0\n    AND (:frontierId::bigint IS NULL OR f.id = :frontierId)\n    AND f.fetch_status = 'ok'\n    AND f.last_successful_response_id IS NOT NULL\n    AND (:frontierId::bigint IS NOT NULL OR f.process_status <> 'pending')\n    AND (\n      :failure::text IS NULL\n      OR EXISTS (\n        SELECT 1 FROM crawler.frontier_failure ff\n        WHERE ff.id = f.id\n          AND ff.failure = :failure\n          AND ff.http_status IS NOT DISTINCT FROM :httpStatus::integer\n          AND left(regexp_replace(coalesce(nullif(ff.error_text, ''), '-'), '\\s+', ' ', 'g'), 180) = :errorFingerprint::text\n      )\n    )\n  RETURNING f.id\n)\nSELECT count(*)::int AS \"count!\" FROM updated"};

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
 *     AND (:frontierId::bigint IS NULL OR f.id = :frontierId)
 *     AND f.fetch_status = 'ok'
 *     AND f.last_successful_response_id IS NOT NULL
 *     AND (:frontierId::bigint IS NOT NULL OR f.process_status <> 'pending')
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
  day: string;
  federation: string | null;
}

/** 'GetRefetchOutlook' query type */
export interface IGetRefetchOutlookQuery {
  params: IGetRefetchOutlookParams;
  result: IGetRefetchOutlookResult;
}

const getRefetchOutlookIR: any = {"usedParamSet":{},"params":[],"statement":"WITH bounds AS (\n  SELECT (now() AT TIME ZONE 'UTC')::date AS today\n), counts AS (\n  SELECT\n    (next_fetch_at AT TIME ZONE 'UTC')::date AS day,\n    federation,\n    count(*)::int AS count\n  FROM crawler.frontier, bounds\n  WHERE fetch_status = 'ok' AND process_status = 'ok'\n    AND next_fetch_at >= now()\n    AND next_fetch_at < (today + 30)::timestamp AT TIME ZONE 'UTC'\n  GROUP BY 1, 2\n), buckets AS (\n  SELECT today + offset_day AS day FROM bounds, generate_series(0, 29) AS offset_day\n)\nSELECT buckets.day::text AS \"day!\", counts.federation AS \"federation?\", coalesce(counts.count, 0)::int AS \"count!\"\nFROM buckets\nLEFT JOIN counts ON counts.day = buckets.day\nORDER BY buckets.day, counts.federation"};

/**
 * Query generated from SQL:
 * ```
 * WITH bounds AS (
 *   SELECT (now() AT TIME ZONE 'UTC')::date AS today
 * ), counts AS (
 *   SELECT
 *     (next_fetch_at AT TIME ZONE 'UTC')::date AS day,
 *     federation,
 *     count(*)::int AS count
 *   FROM crawler.frontier, bounds
 *   WHERE fetch_status = 'ok' AND process_status = 'ok'
 *     AND next_fetch_at >= now()
 *     AND next_fetch_at < (today + 30)::timestamp AT TIME ZONE 'UTC'
 *   GROUP BY 1, 2
 * ), buckets AS (
 *   SELECT today + offset_day AS day FROM bounds, generate_series(0, 29) AS offset_day
 * )
 * SELECT buckets.day::text AS "day!", counts.federation AS "federation?", coalesce(counts.count, 0)::int AS "count!"
 * FROM buckets
 * LEFT JOIN counts ON counts.day = buckets.day
 * ORDER BY buckets.day, counts.federation
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


