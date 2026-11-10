/** Types generated for queries found in "app/api/calendar/[key]/calendar.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

export type stringArray = (string)[];

/** 'CalendarFeedOwner' parameters type */
export interface ICalendarFeedOwnerParams {
  key: string;
}

/** 'CalendarFeedOwner' return type */
export interface ICalendarFeedOwnerResult {
  claims: Json | null;
  cross_tenant: boolean;
  id: string;
  name: string;
  tenant_ids: stringArray | null;
  tenant_name: string;
  with_competitions: boolean | null;
}

/** 'CalendarFeedOwner' query type */
export interface ICalendarFeedOwnerQuery {
  params: ICalendarFeedOwnerParams;
  result: ICalendarFeedOwnerResult;
}

const calendarFeedOwnerIR: any = {"usedParamSet":{"key":true},"params":[{"name":"key","required":true,"transform":{"type":"scalar"},"locs":[{"a":409,"b":413}]}],"statement":"select s.id, s.name, s.cross_tenant, owner_tenant.name as tenant_name,\n  (s.condition).with_competitions,\n  array(select t.id::text from tenant t where s.cross_tenant or t.id = s.tenant_id order by t.id) as tenant_ids,\n  to_jsonb(app_private.create_jwt_token(u)) as claims\nfrom event_saved_search s\njoin users u on u.id = s.user_id\njoin tenant owner_tenant on owner_tenant.id = s.tenant_id\nwhere s.feed_key = :key!"};

/**
 * Query generated from SQL:
 * ```
 * select s.id, s.name, s.cross_tenant, owner_tenant.name as tenant_name,
 *   (s.condition).with_competitions,
 *   array(select t.id::text from tenant t where s.cross_tenant or t.id = s.tenant_id order by t.id) as tenant_ids,
 *   to_jsonb(app_private.create_jwt_token(u)) as claims
 * from event_saved_search s
 * join users u on u.id = s.user_id
 * join tenant owner_tenant on owner_tenant.id = s.tenant_id
 * where s.feed_key = :key!
 * ```
 */
export const calendarFeedOwner = new PreparedQuery<ICalendarFeedOwnerParams,ICalendarFeedOwnerResult>(calendarFeedOwnerIR);


