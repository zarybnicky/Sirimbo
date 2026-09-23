import { CurrentUserDocument } from '@/graphql/CurrentUser';
import type { RequestAuthState, SessionClaims } from '@/lib/auth';
import { buildId } from '@/lib/build-id';
import { executeGraphql } from '@/lib/server/graphql';
import { SESSION_COOKIE } from '@/lib/session-cookies';
import { defaultTenant, getTenant, hostToTenant, type TenantCatalogEntry } from '@/tenant/catalog';
import jwt from 'jsonwebtoken';
import { cookies, headers } from 'next/headers';
import { cache } from 'react';

const asInt = (x: any) => typeof x === 'number' ? x : !x ? Number.NaN : Number.parseInt(x.toString(), 10);

export type JwtClaims = jwt.JwtPayload & {
  exp: number;
  user_id: string;
  tenant_id: string;
  email: string;
  my_person_ids: string[];
  my_tenant_ids: string[];
  my_cohort_ids: string[];
  my_couple_ids: string[];
  is_system_admin: boolean;
  guest_tenant_ids: string[];
  member_tenant_ids: string[];
  trainer_tenant_ids: string[];
  admin_tenant_ids: string[];
};

export type RequestContext = {
  token: string | undefined;
  tenant: TenantCatalogEntry;
  claims: JwtClaims | undefined;
  pgSettings: Record<string, string>;
};

export const getRequestAuth = cache(async (): Promise<RequestAuthState> => {
  const cookieStore = await cookies();

  if (!cookieStore.has(SESSION_COOKIE)) {
    return { claims: null, user: null };
  }

  const data = await executeGraphql(CurrentUserDocument, { versionId: buildId });
  const claims =
    typeof data.currentClaims === 'string'
      ? (JSON.parse(data.currentClaims) as SessionClaims)
      : (data.currentClaims as SessionClaims | null);

  return {
    claims,
    user: data.getCurrentUser,
  };
});

export const getRequestContext = cache(async (): Promise<RequestContext> => {
  const cookieStore = await cookies();
  const token = cookieStore.get(SESSION_COOKIE)?.value;
  let claims: JwtClaims | undefined;

  if (token) {
    try {
      claims = jwt.verify(token, process.env.JWT_SECRET!, {
        algorithms: ['HS256'],
        ignoreExpiration: true,
      }) as JwtClaims;
    } catch (error) {
      if (!(error instanceof jwt.JsonWebTokenError)) throw error;
    }
  }

  const cookieTenant = getTenant(cookieStore.get('tenant_id')?.value);
  const tenant = cookieTenant ?? await getRequestHostTenant();
  const pgSettings: Record<string, string> = {
    role: 'anonymous',
    'jwt.claims.user_id': '',
    'jwt.claims.tenant_id': tenant.id.toString(),
  };

  if (claims) {
    pgSettings.role = claims.is_system_admin
      ? 'system_admin'
      : claims.admin_tenant_ids?.map(asInt).includes(tenant.id)
        ? 'administrator'
        : claims.trainer_tenant_ids?.map(asInt).includes(tenant.id)
          ? 'trainer'
          : claims.member_tenant_ids?.map(asInt).includes(tenant.id)
            ? 'member'
            : 'anonymous';

    for (const [key, value] of Object.entries(claims)) {
      if (!['exp', 'aud', 'iat', 'iss', 'tenant_id'].includes(key)) {
        pgSettings[`jwt.claims.${key}`] = Array.isArray(value)
          ? `{${value.join(',')}}`
          : String(value);
      }
    }
  }

  return { token, tenant, claims, pgSettings };
});

async function getRequestHostTenant() {
  const headerStore = await headers();
  const host = headerStore.get('x-forwarded-host') ?? headerStore.get('host');
  const hostname = host?.split(',', 1)[0]?.trim()?.split(':', 1)[0]?.toLowerCase() || null;

  return hostToTenant.get(hostname ?? '') ?? defaultTenant;
}
