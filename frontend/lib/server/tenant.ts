import { CurrentUserDocument } from '@/graphql/CurrentUser';
import type { RequestAuthState } from '@/lib/auth';
import {
  parseCurrentClaims,
  resolveAuth,
  type JwtClaims,
  type ResolvedAuth,
} from '@/lib/auth-claims';
import { buildId } from '@/lib/build-id';
import { executeGraphql } from '@/lib/server/graphql';
import { SESSION_COOKIE } from '@/lib/session-cookies';
import { defaultTenant, getTenant, hostToTenant, type TenantCatalogEntry } from '@/tenant/catalog';
import jwt from 'jsonwebtoken';
import { cookies, headers } from 'next/headers';
import { cache } from 'react';

export type RequestContext = {
  token: string | undefined;
  tenant: TenantCatalogEntry;
  auth: ResolvedAuth;
  pgSettings: Record<string, string>;
};

export const getRequestAuth = cache(async (): Promise<RequestAuthState> => {
  const cookieStore = await cookies();

  if (!cookieStore.has(SESSION_COOKIE)) {
    return { claims: null, user: null };
  }

  const data = await executeGraphql(CurrentUserDocument, { versionId: buildId });
  const claims = parseCurrentClaims(data.currentClaims);

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
  const auth = resolveAuth(claims, tenant.id.toString());
  const pgSettings: Record<string, string> = {
    role: auth.role,
    'jwt.claims.tenant_id': tenant.id.toString(),
  };

  if (claims) {
    for (const [key, value] of Object.entries(claims)) {
      if (!['exp', 'aud', 'iat', 'iss', 'tenant_id'].includes(key)) {
        pgSettings[`jwt.claims.${key}`] = Array.isArray(value)
          ? `{${value.join(',')}}`
          : String(value);
      }
    }
  }

  return { token, tenant, auth, pgSettings };
});

async function getRequestHostTenant() {
  const headerStore = await headers();
  const host = headerStore.get('x-forwarded-host') ?? headerStore.get('host');
  const hostname = host?.split(',', 1)[0]?.trim()?.split(':', 1)[0]?.toLowerCase() || null;

  return hostToTenant.get(hostname ?? '') ?? defaultTenant;
}
