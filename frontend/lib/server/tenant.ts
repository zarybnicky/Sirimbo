import { CurrentUserDocument } from '@/graphql/CurrentUser';
import type { RequestAuthState, SessionClaims } from '@/lib/auth';
import { buildId } from '@/lib/build-id';
import { executeGraphql } from '@/lib/server/graphql';
import { SESSION_COOKIE } from '@/lib/session-cookies';
import { defaultTenant, getTenant, hostToTenant } from '@/tenant/catalog';
import { cookies, headers } from 'next/headers';

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
  const cookieStore = await cookies();
  const cookieTenant = getTenant(cookieStore.get('tenant_id')?.value);
  if (cookieTenant) return cookieTenant;

  const headerStore = await headers();
  const host = headerStore.get('x-forwarded-host') ?? headerStore.get('host');
  const hostname = host?.split(',', 1)[0]?.trim()?.split(':', 1)[0]?.toLowerCase() || null;

  return hostToTenant.get(hostname ?? '') ?? defaultTenant;
}
