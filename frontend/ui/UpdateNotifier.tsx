import React, { useEffect, useRef } from 'react';
import { toast } from 'react-toastify';
import { getNewDeploymentId } from '@/lib/build-id';
import { Serwist } from '@serwist/window';

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
        if (await getNewDeploymentId()) {
          if (toastRef.current) return;

          toastRef.current = toast.warn(
            <div className="flex flex-col gap-0.5">
              <div><b>Je k dispozici nová verze aplikace.</b></div>
              <div>Kliknutím zde ji aktualizujete.</div>
            </div>,
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
