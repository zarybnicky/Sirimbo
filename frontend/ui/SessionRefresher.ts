import * as React from 'react';
import {
  authLoadingAtom,
  clearLegacySession,
  authAtom,
  sessionPresentAtom,
  tokenAtom,
} from '@/lib/auth';
import { refreshSessionAction } from '@/lib/auth-actions';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import { useRouter } from 'next/navigation';

export const SessionRefresher = React.memo(function SessionRefresher({
  initialSessionStale,
}: {
  initialSessionStale: boolean;
}) {
  const router = useRouter();
  const token = useAtomValue(tokenAtom);
  const [sessionPresent, setSessionPresent] = useAtom(sessionPresentAtom);
  const setAuthLoading = useSetAtom(authLoadingAtom);
  const setRequestAuth = useSetAtom(authAtom);
  const initialRefreshStarted = React.useRef(false);

  const refreshSession = React.useCallback(async () => {
    setRequestAuth(await refreshSessionAction());
  }, [setRequestAuth]);

  React.useEffect(() => {
    if (!token) setAuthLoading(false);
  }, [setAuthLoading, token]);

  React.useEffect(() => {
    if (!initialSessionStale || initialRefreshStarted.current) return;
    initialRefreshStarted.current = true;

    void refreshSession()
      .then(() => router.refresh())
      .catch(() => {});
  }, [initialSessionStale, refreshSession, router]);

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
        router.refresh();
      })
      .catch(() => {});
  }, [router, sessionPresent, setSessionPresent, token]);

  React.useEffect(() => {
    if (!token && !sessionPresent) return;

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
  }, [refreshSession, sessionPresent, token]);

  return null;
});
