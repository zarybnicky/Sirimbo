import * as React from 'react';
import {
  CurrentUserDocument,
} from '@/graphql/CurrentUser';
import { useQuery } from 'urql';
import { tokenAtom, authAtom } from '@/ui/state/auth';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';

export const UserRefresher = React.memo(function ProvideAuth() {
  const [token] = useAtom(tokenAtom);
  const setAuth = useSetAtom(authAtom);

  const [{ data: currentUser, fetching }, refetch] = useQuery({ query: CurrentUserDocument, pause: !token });

  React.useEffect(() => {
    if (!fetching && currentUser) {
      setAuth(currentUser.refreshJwt, currentUser.getCurrentUser);
    }
  }, [fetching, setAuth, currentUser]);

  React.useEffect(() => {
    const launchQuery = () => {
      if (typeof document === "undefined" ||
        document.visibilityState === undefined ||
        document.visibilityState === "visible"
      ) {
        refetch({ requestPolicy: 'network-only' });
      }
    };
    const interval = setInterval(launchQuery, 30000);
    return () => clearInterval(interval);
  }, [refetch]);

  return null;
});

export const useAuth = () => useAtomValue(authAtom);
