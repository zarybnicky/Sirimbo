import * as React from 'react';
import { CurrentUserDocument } from '@/graphql/CurrentUser';
import { useQuery } from 'urql';
import {
  authAtom,
  authHelpersAtom,
  authLoadingAtom,
  sessionPresentAtom,
  tokenAtom,
} from '@/ui/state/auth';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import { buildId } from '@/lib/build-id';
import type { SessionClaims } from '@/lib/session-claims';

export const UserRefresher = React.memo(function ProvideAuth() {
  const [token] = useAtom(tokenAtom);
  const sessionPresent = useAtomValue(sessionPresentAtom);
  const setSessionPresent = useSetAtom(sessionPresentAtom);
  const setAuthLoading = useSetAtom(authLoadingAtom);
  const setAuth = useSetAtom(authAtom);
  const [claims, setClaims] = React.useState<SessionClaims | null>(null);

  const [{ data: currentUser, fetching }, refetch] = useQuery({
    query: CurrentUserDocument,
    // The httpOnly cookie authenticates the request; also gate on `token` for
    // legacy sessions not yet upgraded to the cookie.
    pause: !token && !sessionPresent,
    variables: { versionId: buildId },
  });

  React.useEffect(() => setAuthLoading(fetching), [fetching, setAuthLoading]);

  // Claims come from the server as data (no client-side JWT decode). A legacy
  // session POSTs its token once to establish the cookie; otherwise read via GET.
  const refreshClaims = React.useCallback(async () => {
    if (!token && !sessionPresent) {
      setClaims(null);
      return;
    }
    const res =
      !sessionPresent && token
        ? await fetch('/api/auth/session', {
            method: 'POST',
            headers: { 'content-type': 'application/json' },
            body: JSON.stringify({ token }),
          })
        : await fetch('/api/auth/session');
    if (!res.ok) return;
    const data = (await res.json()) as { claims: SessionClaims | null };
    setClaims(data.claims ?? null);
    if (!sessionPresent && token && data.claims) setSessionPresent(true);
  }, [token, sessionPresent, setSessionPresent]);

  React.useEffect(() => {
    void refreshClaims().catch(() => {});
  }, [refreshClaims]);

  React.useEffect(() => {
    if (!fetching && currentUser && claims) {
      setAuth(claims, currentUser.getCurrentUser);
    }
  }, [fetching, setAuth, currentUser, claims]);

  // Poll the current user and claims together; authAtom's deepEqual skips no-ops.
  React.useEffect(() => {
    const poll = () => {
      if (
        typeof document === 'undefined' ||
        document.visibilityState === undefined ||
        document.visibilityState === 'visible'
      ) {
        refetch({ requestPolicy: 'network-only' });
        void refreshClaims().catch(() => {});
      }
    };
    const interval = setInterval(poll, 30_000);
    return () => clearInterval(interval);
  }, [refetch, refreshClaims]);

  return null;
});

export const useAuth = () => useAtomValue(authHelpersAtom);
export const useAuthLoading = () => useAtomValue(authLoadingAtom);
