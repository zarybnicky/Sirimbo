import React, { useEffect, useRef } from 'react';
import { toast } from 'react-toastify';
import { buildId } from '@/lib/build-id';
import { Serwist } from '@serwist/window';

type DeploymentResponse = {
  deploymentId?: string | null;
};

export const UpdateNotifier = React.memo(function UpdateNotifier() {
  const toastRef = useRef<string | number | undefined>(undefined);

  useEffect(() => {
    if (process.env.NODE_ENV !== 'production') return;

    const check = async () => {
      if ('serviceWorker' in navigator) {
        navigator.serviceWorker
          .getRegistration('/')
          .then((r) => r?.update())
          .catch(() => {});
      }
      try {
        const currentDeploymentId = document.documentElement.dataset.dplId ?? buildId;
        if (!currentDeploymentId || currentDeploymentId === 'develop') {
          return;
        }

        const response = await fetch('/api/health', {
          cache: 'no-store',
          headers: { Accept: 'application/json' },
        });
        if (!response.ok) return;

        const data = (await response.json()) as DeploymentResponse;
        const deploymentId = data.deploymentId;
        if (!deploymentId || deploymentId === 'develop') return;

        if (deploymentId !== currentDeploymentId) {
          if (toastRef.current) return;

          toastRef.current = toast.warn(
            <>
              <div><b>Je k dispozici nová verze aplikace.</b></div>
              <div>Kliknutím zde ji aktualizujete.</div>
            </>,
            {
              autoClose: false,
              closeOnClick: true,
              onClick: () => window.location.reload(),
              onClose: () => {
                toastRef.current = undefined;
              },
            },
          );
        }
      } catch {
        // Ignore transient network and deploy-race failures.
      }
    };

    let interval: number | undefined;
    const start = () => {
      if (interval || document.visibilityState !== 'visible') return;
      interval = window.setInterval(check, (5 * 60 * 1000));
      void check();
    };

    const stop = () => {
      if (!interval) return;
      clearInterval(interval);
      interval = undefined;
    };

    const onVisibility = () =>
      document.visibilityState === 'visible' ? start() : stop();

    if ('serviceWorker' in navigator) {
      const serwist = new Serwist('/sw.js', { scope: '/', type: 'classic' });
      void serwist.register().catch(() => {});
    }

    start();
    document.addEventListener('visibilitychange', onVisibility);
    window.addEventListener('pagehide', stop);
    window.addEventListener('pageshow', start);

    return () => {
      stop();
      document.removeEventListener('visibilitychange', onVisibility);
      window.removeEventListener('pagehide', stop);
      window.removeEventListener('pageshow', start);
    };
  }, []);

  return null;
});
