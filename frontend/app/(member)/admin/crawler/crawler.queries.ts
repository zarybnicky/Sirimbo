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


