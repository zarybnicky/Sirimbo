import { CurrentUserDocument } from '@/graphql/CurrentUser';
import type { RequestAuthState, SessionClaims } from '@/lib/auth';
import { buildId } from '@/lib/build-id';
import { executeGraphql } from '@/lib/server/graphql';
import { SESSION_COOKIE } from '@/lib/session-cookies';
import { defaultTenant, getTenant, hostToTenant, type TenantCatalogEntry } from '@/tenant/catalog';
import jwt from 'jsonwebtoken';
import { cookies, headers } from 'next/headers';

const asInt = (x: any) => typeof x === 'number' ? x : !x ? Number.NaN : Number.parseInt(x.toString(), 10);

export type RequestContext = {
  tenant: TenantCatalogEntry;
  claims: jwt.JwtPayload | undefined;
  settings: Record<string, string>;
};

export async function getRequestAuth(): Promise<RequestAuthState> {
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
}

export async function getRequestTenant() {
  const { tenant } = await getRequestContext();
  return tenant;
}

export async function getRequestContext(): Promise<RequestContext> {
  const cookieStore = await cookies();
  const token = cookieStore.get(SESSION_COOKIE)?.value;
  let claims: jwt.JwtPayload | undefined;

  if (token) {
    try {
      claims = jwt.verify(token, process.env.JWT_SECRET!, {
        algorithms: ['HS256'],
        ignoreExpiration: true,
      }) as jwt.JwtPayload;
    } catch (error) {
      if (!(error instanceof jwt.JsonWebTokenError)) throw error;
    }
  }

  const cookieTenant = getTenant(cookieStore.get('tenant_id')?.value);
  const tenant = cookieTenant ?? await getRequestHostTenant();
  const settings: Record<string, string> = {
    role: 'anonymous',
    'jwt.claims.tenant_id': tenant.id.toString(),
  };

  if (claims) {
    settings.role = claims.is_system_admin
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
        settings[`jwt.claims.${key}`] = Array.isArray(value)
          ? `{${value.join(',')}}`
          : String(value);
      }
    }
  }

  return { tenant, claims, settings };
}

async function getRequestHostTenant() {
  const headerStore = await headers();
  const host = headerStore.get('x-forwarded-host') ?? headerStore.get('host');
  const hostname = host?.split(',', 1)[0]?.trim()?.split(':', 1)[0]?.toLowerCase() || null;

  return hostToTenant.get(hostname ?? '') ?? defaultTenant;
}
