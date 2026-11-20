import 'server-only';

import {
  type JwtClaims,
  normalizeClaims,
  resolveAuth,
  type ResolvedAuth,
} from '@/lib/auth-claims';
import { SESSION_COOKIE } from '@/lib/session-cookies';
import {
  defaultTenant,
  getTenant,
  hostToTenant,
  type TenantCatalogEntry,
} from '@/tenant/catalog';
import jwt from 'jsonwebtoken';
import { cookies, headers } from 'next/headers';
import { cache } from 'react';

export type RequestContext = {
  claims: JwtClaims | null;
  tenant: TenantCatalogEntry;
  auth: ResolvedAuth;
  pgSettings: Record<string, string>;
};

export const getRequestContext = cache(async (eventShare?: string, apiRequest?: Request): Promise<RequestContext> => {
  const cookieStore = await cookies();
  const authorization = apiRequest?.headers.get('authorization');
  const token = authorization?.toLowerCase().startsWith('bearer ')
    ? authorization.slice(7)
    : cookieStore.get(SESSION_COOKIE)?.value;
  let claims: JwtClaims | null = null;

  if (token) {
    try {
      const payload = jwt.verify(token, process.env.JWT_SECRET!, {
        algorithms: ['HS256'],
        ignoreExpiration: true,
      }) as JwtClaims;
      claims = normalizeClaims(payload);
    } catch (error) {
      if (!(error instanceof jwt.JsonWebTokenError)) throw error;
    }
  }

  const cookieTenant = getTenant(cookieStore.get('tenant_id')?.value);
  const tenant = getTenant(apiRequest?.headers.get('x-tenant-id'))
    ?? cookieTenant ?? (await getRequestHostTenant());
  eventShare ??= apiRequest?.headers.get('x-event-share') ?? undefined;
  return { claims, tenant, ...resolveClaims(claims, tenant.id.toString(), eventShare) };
});

export async function getRequestHostTenant() {
  const headerStore = await headers();
  const host = headerStore.get('x-forwarded-host') ?? headerStore.get('host');
  const hostname =
    host?.split(',', 1)[0]?.trim()?.split(':', 1)[0]?.toLowerCase() || null;

  return hostToTenant.get(hostname ?? '') ?? defaultTenant;
}

export function resolveClaims(
  claims: JwtClaims | null,
  tenantId: string,
  eventShare?: string,
) {
  const auth = resolveAuth(claims, tenantId);
  const pgSettings: Record<string, string> = {
    role: auth.role,
    'jwt.claims.tenant_id': tenantId,
    'jwt.claims.user_id': '',
    'jwt.claims.email': '',
    'jwt.claims.my_person_ids': '{}',
    'jwt.claims.my_tenant_ids': '{}',
    'jwt.claims.my_cohort_ids': '{}',
    'jwt.claims.my_couple_ids': '{}',
  };

  if (claims) {
    for (const [key, value] of Object.entries(claims)) {
      if (key !== 'tenant_id') {
        pgSettings[`jwt.claims.${key}`] = Array.isArray(value)
          ? `{${value.join(',')}}`
          : String(value);
      }
    }
  }

  pgSettings['jwt.claims.shared.event_token'] =
    eventShare && /^(?:\d{1,18}|[A-Za-z0-9_-]{32})$/.test(eventShare) ? eventShare : '';

  return { auth, pgSettings };
}
