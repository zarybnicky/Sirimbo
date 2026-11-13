/** Types generated for queries found in "app/api/access-control/access-control.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type access_credential_kind = 'MIFARE';

export type NumberOrString = number | string;

/** 'AccessControlContext' parameters type */
export interface IAccessControlContextParams {
  token: string;
}

/** 'AccessControlContext' return type */
export interface IAccessControlContextResult {
  tenantId: string;
}

/** 'AccessControlContext' query type */
export interface IAccessControlContextQuery {
  params: IAccessControlContextParams;
  result: IAccessControlContextResult;
}

const accessControlContextIR: any = {"usedParamSet":{"token":true},"params":[{"name":"token","required":true,"transform":{"type":"scalar"},"locs":[{"a":102,"b":108}]}],"statement":"select tenant_id::text as \"tenantId!\" from tenant_settings\nwhere settings->>'accessCredentialToken' = :token! limit 2"};

/**
 * Query generated from SQL:
 * ```
 * select tenant_id::text as "tenantId!" from tenant_settings
 * where settings->>'accessCredentialToken' = :token! limit 2
 * ```
 */
export const accessControlContext = new PreparedQuery<IAccessControlContextParams,IAccessControlContextResult>(accessControlContextIR);


/** 'SetAccessControlTenant' parameters type */
export interface ISetAccessControlTenantParams {
  tenantId: string;
}

/** 'SetAccessControlTenant' return type */
export interface ISetAccessControlTenantResult {
  set_config: string | null;
}

/** 'SetAccessControlTenant' query type */
export interface ISetAccessControlTenantQuery {
  params: ISetAccessControlTenantParams;
  result: ISetAccessControlTenantResult;
}

const setAccessControlTenantIR: any = {"usedParamSet":{"tenantId":true},"params":[{"name":"tenantId","required":true,"transform":{"type":"scalar"},"locs":[{"a":42,"b":51}]}],"statement":"select set_config('jwt.claims.tenant_id', :tenantId!, true)"};

/**
 * Query generated from SQL:
 * ```
 * select set_config('jwt.claims.tenant_id', :tenantId!, true)
 * ```
 */
export const setAccessControlTenant = new PreparedQuery<ISetAccessControlTenantParams,ISetAccessControlTenantResult>(setAccessControlTenantIR);


/** 'AccessControlCredentials' parameters type */
export interface IAccessControlCredentialsParams {
  tenantId: NumberOrString;
}

/** 'AccessControlCredentials' return type */
export interface IAccessControlCredentialsResult {
  code: string;
  kind: access_credential_kind;
  personId: string;
  personName: string;
}

/** 'AccessControlCredentials' query type */
export interface IAccessControlCredentialsQuery {
  params: IAccessControlCredentialsParams;
  result: IAccessControlCredentialsResult;
}

const accessControlCredentialsIR: any = {"usedParamSet":{"tenantId":true},"params":[{"name":"tenantId","required":true,"transform":{"type":"scalar"},"locs":[{"a":161,"b":170}]}],"statement":"select c.kind, c.code, c.person_id::text as \"personId!\", p.name as \"personName\"\nfrom access_credential c\njoin person p on p.id = c.person_id\nwhere c.tenant_id = :tenantId! and c.valid_range @> now()\n  and c.person_id in (\n    select person_id from current_tenant_membership\n    union all select person_id from current_tenant_trainer\n    union all select person_id from current_tenant_administrator\n  )\norder by c.id"};

/**
 * Query generated from SQL:
 * ```
 * select c.kind, c.code, c.person_id::text as "personId!", p.name as "personName"
 * from access_credential c
 * join person p on p.id = c.person_id
 * where c.tenant_id = :tenantId! and c.valid_range @> now()
 *   and c.person_id in (
 *     select person_id from current_tenant_membership
 *     union all select person_id from current_tenant_trainer
 *     union all select person_id from current_tenant_administrator
 *   )
 * order by c.id
 * ```
 */
export const accessControlCredentials = new PreparedQuery<IAccessControlCredentialsParams,IAccessControlCredentialsResult>(accessControlCredentialsIR);


/** 'InsertAccessControlEvents' parameters type */
export interface IInsertAccessControlEventsParams {
  events: string;
  locationId: NumberOrString;
}

/** 'InsertAccessControlEvents' return type */
export interface IInsertAccessControlEventsResult {
  accepted: number;
}

/** 'InsertAccessControlEvents' query type */
export interface IInsertAccessControlEventsQuery {
  params: IInsertAccessControlEventsParams;
  result: IInsertAccessControlEventsResult;
}

const insertAccessControlEventsIR: any = {"usedParamSet":{"locationId":true,"events":true},"params":[{"name":"locationId","required":true,"transform":{"type":"scalar"},"locs":[{"a":187,"b":198}]},{"name":"events","required":true,"transform":{"type":"scalar"},"locs":[{"a":601,"b":608}]}],"statement":"with inserted as (\n  insert into access_event (\n    external_id, device, location_id, kind, code, person_id, occurred_at, allowed, reason\n  )\n  select\n    event.id,\n    event.device,\n    :locationId!::bigint,\n    event.kind::access_credential_kind,\n    event.code,\n    (\n      select credential.person_id\n      from access_credential credential\n      where credential.kind = event.kind::access_credential_kind\n        and credential.code = event.code\n        and credential.valid_range @> event.\"occurredAt\"\n    ),\n    event.\"occurredAt\",\n    event.allowed,\n    event.reason\n  from jsonb_to_recordset(:events!::text::jsonb) as event(\n    id text,\n    device text,\n    kind text,\n    code text,\n    \"occurredAt\" timestamptz,\n    allowed boolean,\n    reason text\n  )\n  on conflict (tenant_id, external_id) do nothing\n  returning id\n)\nselect count(*)::int as \"accepted!\" from inserted"};

/**
 * Query generated from SQL:
 * ```
 * with inserted as (
 *   insert into access_event (
 *     external_id, device, location_id, kind, code, person_id, occurred_at, allowed, reason
 *   )
 *   select
 *     event.id,
 *     event.device,
 *     :locationId!::bigint,
 *     event.kind::access_credential_kind,
 *     event.code,
 *     (
 *       select credential.person_id
 *       from access_credential credential
 *       where credential.kind = event.kind::access_credential_kind
 *         and credential.code = event.code
 *         and credential.valid_range @> event."occurredAt"
 *     ),
 *     event."occurredAt",
 *     event.allowed,
 *     event.reason
 *   from jsonb_to_recordset(:events!::text::jsonb) as event(
 *     id text,
 *     device text,
 *     kind text,
 *     code text,
 *     "occurredAt" timestamptz,
 *     allowed boolean,
 *     reason text
 *   )
 *   on conflict (tenant_id, external_id) do nothing
 *   returning id
 * )
 * select count(*)::int as "accepted!" from inserted
 * ```
 */
export const insertAccessControlEvents = new PreparedQuery<IInsertAccessControlEventsParams,IInsertAccessControlEventsResult>(insertAccessControlEventsIR);


