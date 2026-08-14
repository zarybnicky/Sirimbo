import { CurrentUserDocument } from '@/graphql/CurrentUser';
import { buildId } from '@/lib/build-id';
import { executeGraphql } from '@/lib/server/graphql';
import { SESSION_COOKIE } from '@/lib/session-cookies';
import type { RequestAuthState, SessionClaims } from '@/ui/state/auth';
import { cookies } from 'next/headers';

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
