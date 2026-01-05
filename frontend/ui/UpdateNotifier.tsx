import React, { useEffect, useRef } from 'react';
import { toast } from 'react-toastify';
import { Serwist, SerwistLifecycleEvent } from '@serwist/window';

const CHECK_MS = 5 * 60 * 1000;

export const UpdateNotifier = React.memo(function UpdateNotifier() {
  const toastRef = useRef<string | number | undefined>();

  useEffect(() => {
    if (!('serviceWorker' in navigator)) return;

    const serwist = new Serwist('/sw.js', { scope: '/', type: 'classic' });

    const showToast = () => {
      if (toastRef.current) return;

      toastRef.current = toast.warn(
        <>
          <b>Je k dispozici nová verze aplikace.</b> Kliknutím zde ji aktualizujete.
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
    };

    const onInstalled = (event: SerwistLifecycleEvent) => {
      if (event?.isUpdate) showToast();
    };
    serwist.addEventListener('installed', onInstalled);

    const check = () =>
      navigator.serviceWorker
        .getRegistration('/')
        .then((r) => r?.update())
        .catch(() => {});

    let interval: number | undefined;
    const start = () => {
      if (interval) return;
      if (document.visibilityState !== 'visible') return;
      interval = window.setInterval(check, CHECK_MS);
      void check();
    };

    const stop = () => {
      if (!interval) return;
      clearInterval(interval);
      interval = undefined;
    };

    const onVisibility = () =>
      document.visibilityState === 'visible' ? start() : stop();

    void serwist
      .register()
      .then(() => {
        start();
        document.addEventListener('visibilitychange', onVisibility);
        window.addEventListener('pagehide', stop);
      })
      .catch(() => {});

    return () => {
      stop();
      document.removeEventListener('visibilitychange', onVisibility);
      window.removeEventListener('pagehide', stop);
      serwist.removeEventListener('installed', onInstalled);
    };
  }, []);

  return null;
});
