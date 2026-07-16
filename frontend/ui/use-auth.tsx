import * as React from 'react';
import { CurrentUserDocument } from '@/graphql/CurrentUser';
import { useQuery } from 'urql';
import {
  authAtom,
  authHelpersAtom,
  authLoadingAtom,
  sessionPresentAtom,
  tokenAtom,
  type SessionClaims,
} from '@/ui/state/auth';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import { useRouter } from 'next/navigation';
import { buildId } from '@/lib/build-id';

export const UserRefresher = React.memo(function ProvideAuth() {
  const [token] = useAtom(tokenAtom);
  const [sessionPresent, setSessionPresent] = useAtom(sessionPresentAtom);
  const setAuthLoading = useSetAtom(authLoadingAtom);
  const setAuth = useSetAtom(authAtom);

  const [{ data, fetching }, refetch] = useQuery({
    query: CurrentUserDocument,
    // The httpOnly cookie authenticates the request; `token` covers legacy
    // sessions not yet upgraded to the cookie.
    pause: !token && !sessionPresent,
    variables: { versionId: buildId },
  });

  React.useEffect(() => setAuthLoading(fetching), [fetching, setAuthLoading]);

  React.useEffect(() => {
    if (!fetching && data) {
      setAuth(
        (data.currentClaims as SessionClaims | null) ?? null,
        data.getCurrentUser ?? null,
      );
    }
  }, [fetching, data, setAuth]);

  // TODO(cookie-migration): one-time upgrade of legacy bearer sessions — plant
  // the httpOnly cookie from the localStorage token.
  React.useEffect(() => {
    if (!token || sessionPresent) return;
    void fetch('/api/auth/session', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ token }),
    })
      .then((res) => {
        if (res.ok) setSessionPresent(true);
      })
      .catch(() => {});
  }, [token, sessionPresent, setSessionPresent]);

  React.useEffect(() => {
    const poll = () => {
      if (
        typeof document === 'undefined' ||
        document.visibilityState === undefined ||
        document.visibilityState === 'visible'
      ) {
        refetch({ requestPolicy: 'network-only' });
      }
    };
    const interval = setInterval(poll, 30_000);
    return () => clearInterval(interval);
  }, [refetch]);

  return null;
});

export const useAuth = () => useAtomValue(authHelpersAtom);
export const useAuthLoading = () => useAtomValue(authLoadingAtom);

// Auth entry pages (login/otp/register): bounce an already-signed-in visitor
// into the app — to their profile first if no person is linked yet.
export function useRedirectLoggedIn() {
  const router = useRouter();
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const personCount = auth.personIds.length;
  React.useEffect(() => {
    if (!authLoading && auth.user) {
      router.replace(personCount === 0 ? '/profil' : '/dashboard');
    }
  }, [authLoading, auth.user, personCount, router]);
}
