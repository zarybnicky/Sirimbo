/** Types generated for queries found in "app/(standalone)/akce/[id]/akce.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type NumberOrString = number | string;

/** 'EventSeriesEvents' parameters type */
export interface IEventSeriesEventsParams {
  id?: NumberOrString | null | void;
}

/** 'EventSeriesEvents' return type */
export interface IEventSeriesEventsResult {
  id: string;
}

/** 'EventSeriesEvents' query type */
export interface IEventSeriesEventsQuery {
  params: IEventSeriesEventsParams;
  result: IEventSeriesEventsResult;
}

const eventSeriesEventsIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":89,"b":91}]}],"statement":"select i.id from event_series s join event_instance i on s.id = i.series_id where s.id = :id"};

/**
 * Query generated from SQL:
 * ```
 * select i.id from event_series s join event_instance i on s.id = i.series_id where s.id = :id
 * ```
 */
export const eventSeriesEvents = new PreparedQuery<IEventSeriesEventsParams,IEventSeriesEventsResult>(eventSeriesEventsIR);


