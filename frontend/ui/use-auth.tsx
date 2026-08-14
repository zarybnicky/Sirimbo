import * as React from 'react';
import { CurrentUserDocument } from '@/graphql/CurrentUser';
import { useQuery } from 'urql';
import {
  authAtom,
  authHelpersAtom,
  authLoadingAtom,
  clearLegacySession,
  sessionPresentAtom,
  tokenAtom,
  type SessionClaims,
} from '@/ui/state/auth';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import { buildId } from '@/lib/build-id';
import { useRouter } from 'next/navigation';

export const UserRefresher = React.memo(function ProvideAuth() {
  const token = useAtomValue(tokenAtom);
  const [sessionPresent, setSessionPresent] = useAtom(sessionPresentAtom);
  const setAuthLoading = useSetAtom(authLoadingAtom);
  const setAuth = useSetAtom(authAtom);

  const [{ data, fetching }, refetch] = useQuery({
    query: CurrentUserDocument,
    pause: !token && !sessionPresent,
    variables: { versionId: buildId },
  });

  React.useEffect(() => setAuthLoading(fetching), [fetching, setAuthLoading]);

  React.useEffect(() => {
    if (!fetching && data) {
      setAuth(
        typeof data.currentClaims === 'string'
          ? (JSON.parse(data.currentClaims) as SessionClaims)
          : (data.currentClaims as SessionClaims | null),
        data.getCurrentUser,
      );
    }
  }, [data, fetching, setAuth]);

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

export const useAuth = () => useAtomValue(authHelpersAtom);
export const useAuthLoading = () => useAtomValue(authLoadingAtom);

export function useRedirectLoggedIn() {
  const router = useRouter();
  const auth = useAuth();

  React.useEffect(() => {
    if (auth.user) {
      router.replace(auth.personIds.length > 0 ? '/dashboard' : '/profil');
    }
  }, [auth.personIds.length, auth.user, router]);
}
