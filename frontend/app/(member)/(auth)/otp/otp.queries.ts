/** Types generated for queries found in "app/(member)/(auth)/otp/otp.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

/** 'OtpDestination' parameters type */
export interface IOtpDestinationParams {
  token: string;
}

/** 'OtpDestination' return type */
export interface IOtpDestinationResult {
  event_id: string | null;
  intent: string;
  tenant_id: string;
}

/** 'OtpDestination' query type */
export interface IOtpDestinationQuery {
  params: IOtpDestinationParams;
  result: IOtpDestinationResult;
}

const otpDestinationIR: any = {"usedParamSet":{"token":true},"params":[{"name":"token","required":true,"transform":{"type":"scalar"},"locs":[{"a":71,"b":77}]}],"statement":"select intent, event_id, tenant_id from otp_token where access_token = :token!"};

/**
 * Query generated from SQL:
 * ```
 * select intent, event_id, tenant_id from otp_token where access_token = :token!
 * ```
 */
export const otpDestination = new PreparedQuery<IOtpDestinationParams,IOtpDestinationResult>(otpDestinationIR);


