/** Types generated for queries found in "app/api/auth/handoff/handoff.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type NumberOrString = number | string;

/** 'HandoffEvent' parameters type */
export interface IHandoffEventParams {
  eventId: NumberOrString;
}

/** 'HandoffEvent' return type */
export interface IHandoffEventResult {
  tenant_id: string;
}

/** 'HandoffEvent' query type */
export interface IHandoffEventQuery {
  params: IHandoffEventParams;
  result: IHandoffEventResult;
}

const handoffEventIR: any = {"usedParamSet":{"eventId":true},"params":[{"name":"eventId","required":true,"transform":{"type":"scalar"},"locs":[{"a":48,"b":56}]}],"statement":"select tenant_id from event_instance where id = :eventId! and is_public"};

/**
 * Query generated from SQL:
 * ```
 * select tenant_id from event_instance where id = :eventId! and is_public
 * ```
 */
export const handoffEvent = new PreparedQuery<IHandoffEventParams,IHandoffEventResult>(handoffEventIR);


/** 'CreateHandoffOtp' parameters type */
export interface ICreateHandoffOtpParams {
  eventId: NumberOrString;
  tenantId: NumberOrString;
  userId: NumberOrString;
}

/** 'CreateHandoffOtp' return type */
export interface ICreateHandoffOtpResult {
  access_token: string;
}

/** 'CreateHandoffOtp' query type */
export interface ICreateHandoffOtpQuery {
  params: ICreateHandoffOtpParams;
  result: ICreateHandoffOtpResult;
}

const createHandoffOtpIR: any = {"usedParamSet":{"userId":true,"tenantId":true,"eventId":true},"params":[{"name":"userId","required":true,"transform":{"type":"scalar"},"locs":[{"a":81,"b":88}]},{"name":"tenantId","required":true,"transform":{"type":"scalar"},"locs":[{"a":91,"b":100}]},{"name":"eventId","required":true,"transform":{"type":"scalar"},"locs":[{"a":120,"b":128}]}],"statement":"insert into otp_token (user_id, tenant_id, intent, event_id, expires_at)\nvalues (:userId!, :tenantId!, 'event_handoff', :eventId!, now() + interval '10 minutes')\nreturning access_token"};

/**
 * Query generated from SQL:
 * ```
 * insert into otp_token (user_id, tenant_id, intent, event_id, expires_at)
 * values (:userId!, :tenantId!, 'event_handoff', :eventId!, now() + interval '10 minutes')
 * returning access_token
 * ```
 */
export const createHandoffOtp = new PreparedQuery<ICreateHandoffOtpParams,ICreateHandoffOtpResult>(createHandoffOtpIR);


