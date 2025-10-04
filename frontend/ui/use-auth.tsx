import * as React from 'react';
import { CurrentUserDocument } from '@/graphql/CurrentUser';
import { useQuery } from 'urql';
import { tokenAtom, authAtom, authLoadingAtom } from '@/ui/state/auth';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';

export const UserRefresher = React.memo(function ProvideAuth() {
  const [token] = useAtom(tokenAtom);
  const setAuthLoading = useSetAtom(authLoadingAtom);
  const setAuth = useSetAtom(authAtom);
  const [versionId, setVersionId] = React.useState('');

  const [{ data: currentUser, fetching }, refetch] = useQuery({
    query: CurrentUserDocument,
    pause: !token,
    variables: { versionId },
  });

  React.useEffect(() => {
    let cancelled = false;
    const loadVersion = async () => {
      try {
        const response = await fetch('/api/build-id');
        if (!response.ok) throw new Error('Failed to fetch build id');
        const payload = await response.json();
        if (!cancelled) {
          setVersionId(typeof payload?.buildId === 'string' ? payload.buildId : '');
        }
      } catch {
        if (!cancelled) {
          setVersionId('');
        }
      }
    };

    loadVersion();

    return () => {
      cancelled = true;
    };
  }, []);

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

export const useAuth = () => useAtomValue(authAtom);
export const useAuthLoading = () => useAtomValue(authLoadingAtom);
