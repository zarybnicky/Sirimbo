/** Types generated for queries found in "lib/jobs.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

/** 'EnqueueJob' parameters type */
export interface IEnqueueJobParams {
  identifier: string;
  jobKey?: string | null | void;
  payload: string;
}

/** 'EnqueueJob' return type */
export interface IEnqueueJobResult {
  id: string | null;
}

/** 'EnqueueJob' query type */
export interface IEnqueueJobQuery {
  params: IEnqueueJobParams;
  result: IEnqueueJobResult;
}

const enqueueJobIR: any = {"usedParamSet":{"identifier":true,"payload":true,"jobKey":true},"params":[{"name":"identifier","required":true,"transform":{"type":"scalar"},"locs":[{"a":49,"b":60}]},{"name":"payload","required":true,"transform":{"type":"scalar"},"locs":[{"a":76,"b":84}]},{"name":"jobKey","required":false,"transform":{"type":"scalar"},"locs":[{"a":112,"b":118}]}],"statement":"select (graphile_worker.add_job(\n  identifier => :identifier!,\n  payload => :payload!::text::json,\n  job_key => :jobKey\n)).id"};

/**
 * Query generated from SQL:
 * ```
 * select (graphile_worker.add_job(
 *   identifier => :identifier!,
 *   payload => :payload!::text::json,
 *   job_key => :jobKey
 * )).id
 * ```
 */
export const enqueueJob = new PreparedQuery<IEnqueueJobParams,IEnqueueJobResult>(enqueueJobIR);


