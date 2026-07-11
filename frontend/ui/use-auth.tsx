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

export const UserRefresher = React.memo(function ProvideAuth() {
  const [token] = useAtom(tokenAtom);
  const sessionPresent = useAtomValue(sessionPresentAtom);
  const setAuthLoading = useSetAtom(authLoadingAtom);
  const setAuth = useSetAtom(authAtom);

  const [{ data: currentUser, fetching }, refetch] = useQuery({
    query: CurrentUserDocument,
    // Cookie-based sessions have no client token to gate on; the httpOnly cookie
    // authenticates the request. Keep gating on `token` for not-yet-migrated flows.
    pause: !token && !sessionPresent,
    variables: { versionId: buildId },
  });

  React.useEffect(() => setAuthLoading(fetching), [fetching, setAuthLoading]);

  React.useEffect(() => {
    if (!fetching && currentUser) {
      setAuth(currentUser.refreshJwt, currentUser.getCurrentUser);
    }
  }, [fetching, setAuth, currentUser]);

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
