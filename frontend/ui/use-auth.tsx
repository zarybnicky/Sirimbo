import * as React from 'react';
import { CurrentUserDocument } from '@/graphql/CurrentUser';
import { useQuery } from 'urql';
import {
  authAtom,
  authLoadingAtom,
  clearLegacySession,
  requestAuthAtom,
  sessionPresentAtom,
  tokenAtom,
  type SessionClaims,
} from '@/ui/state/auth';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import { buildId } from '@/lib/build-id';
import { redirect } from 'next/navigation';

export const UserRefresher = React.memo(function ProvideAuth() {
  const token = useAtomValue(tokenAtom);
  const [sessionPresent, setSessionPresent] = useAtom(sessionPresentAtom);
  const setAuthLoading = useSetAtom(authLoadingAtom);
  const setRequestAuth = useSetAtom(requestAuthAtom);

  const [{ data, fetching }, refetch] = useQuery({
    query: CurrentUserDocument,
    pause: !token && !sessionPresent,
    variables: { versionId: buildId },
  });

  React.useEffect(() => setAuthLoading(fetching), [fetching, setAuthLoading]);

  React.useEffect(() => {
    if (!fetching && data) {
      setRequestAuth({
        claims:
          typeof data.currentClaims === 'string'
            ? (JSON.parse(data.currentClaims) as SessionClaims)
            : (data.currentClaims as SessionClaims | null),
        user: data.getCurrentUser,
      });
    }
  }, [data, fetching, setRequestAuth]);

  React.useEffect(() => {
    if (!token || sessionPresent) return;

    void fetch('/api/auth/session', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ token }),
    })
      .then((response) => {
        if (!response.ok) return;
        clearLegacySession();
        setSessionPresent(true);
      })
      .catch(() => {});
  }, [sessionPresent, setSessionPresent, token]);

  React.useEffect(() => {
    const launchQuery = () => {
      if (
        typeof document === 'undefined' ||
        document.visibilityState === undefined ||
        document.visibilityState === 'visible'
      ) {
        refetch({ requestPolicy: 'network-only' });
      }
    };
    const interval = setInterval(launchQuery, 30_000);
    return () => clearInterval(interval);
  }, [refetch]);

  return null;
});

export const useAuth = () => useAtomValue(authAtom);
export const useAuthLoading = () => useAtomValue(authLoadingAtom);

export function useRedirectLoggedIn() {
  const auth = useAuth();
  if (auth.user) redirect(auth.personIds.length > 0 ? '/dashboard' : '/profil');
}
