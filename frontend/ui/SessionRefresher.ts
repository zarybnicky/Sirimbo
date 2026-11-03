import * as React from 'react';
import { authAtom, sessionPresentAtom } from '@/lib/auth';
import { refreshSessionAction } from '@/lib/auth-actions';
import { useAtomValue, useSetAtom } from 'jotai';
import { useRouter } from 'next/navigation';

export const SessionRefresher = React.memo(function SessionRefresher({
  initialSessionStale,
}: {
  initialSessionStale: boolean;
}) {
  const router = useRouter();
  const sessionPresent = useAtomValue(sessionPresentAtom);
  const setRequestAuth = useSetAtom(authAtom);
  const initialRefreshStarted = React.useRef(false);

  const refreshSession = React.useCallback(async () => {
    setRequestAuth(await refreshSessionAction());
  }, [setRequestAuth]);

  React.useEffect(() => {
    if (!initialSessionStale || initialRefreshStarted.current) return;
    initialRefreshStarted.current = true;

    void refreshSession()
      .then(() => router.refresh())
      .catch(() => {});
  }, [initialSessionStale, refreshSession, router]);

  React.useEffect(() => {
    if (!sessionPresent) return;

    const refresh = () => {
      if (
        typeof document === 'undefined' ||
        document.visibilityState === undefined ||
        document.visibilityState === 'visible'
      ) {
        void refreshSession().catch(() => {});
      }
    };
    const interval = setInterval(refresh, 30_000);
    return () => clearInterval(interval);
  }, [refreshSession, sessionPresent]);

  return null;
});
