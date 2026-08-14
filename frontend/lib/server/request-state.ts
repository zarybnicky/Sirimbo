import { CurrentUserDocument } from '@/graphql/CurrentUser';
import { buildId } from '@/lib/build-id';
import { executeGraphql } from '@/lib/server/graphql';
import { SESSION_COOKIE } from '@/lib/session-cookies';
import { getRequestTenant } from '@/tenant/server';
import type { RequestAuthState, SessionClaims } from '@/ui/state/auth';
import { cookies } from 'next/headers';

export async function getRequestState() {
  const tenantPromise = getRequestTenant();
  const cookieStore = await cookies();

  if (!cookieStore.has(SESSION_COOKIE)) {
    const tenant = await tenantPromise;
    return {
      tenant,
      auth: { tenantId: tenant.id, claims: null, user: null } satisfies RequestAuthState,
    };
  }

  const [tenant, data] = await Promise.all([
    tenantPromise,
    executeGraphql(CurrentUserDocument, { versionId: buildId }),
  ]);
  const claims =
    typeof data.currentClaims === 'string'
      ? (JSON.parse(data.currentClaims) as SessionClaims)
      : (data.currentClaims as SessionClaims | null);

  return {
    tenant,
    auth: {
      tenantId: tenant.id,
      claims,
      user: data.getCurrentUser,
    } satisfies RequestAuthState,
  };
}
