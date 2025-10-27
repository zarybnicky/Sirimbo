import { useEffect, useRef } from 'react';
import { toast } from 'react-toastify';
import { Serwist as SerwistWindow } from "@serwist/window";

const CHECK_MS = 5 * 60 * 1000;

export function UpdateNotifier() {
  const swwRef = useRef<InstanceType<typeof SerwistWindow> | undefined>();

  useEffect(() => {
    if (!("serviceWorker" in navigator)) return;

    const serwist = new SerwistWindow("/sw.js", { scope: "/", type: "classic" });
    swwRef.current = serwist;

    const onWaiting = () => {
      toast.warn((
        <>
          <b>Je k dispozici nová verze aplikace.</b>
          {' '}
          Kliknutím zde ji aktualizujete.
        </>
      ), {
        autoClose: false,
        closeOnClick: true,
        onClick() {
          swwRef.current?.messageSkipWaiting();
        },
      });
    };
    serwist.addEventListener("waiting", onWaiting);

    const onControlling = () => {
      window.location.reload();
    };
    serwist.addEventListener("controlling", onControlling);
    navigator.serviceWorker.addEventListener("controllerchange", onControlling);

    const check = () => navigator.serviceWorker.getRegistration().then(x => x?.update());
    void serwist.register().then(() => {
      let t: number | undefined;
      const start = () => {
        if (t) return;
        if (document.visibilityState !== "visible") return;
        t = window.setInterval(check, CHECK_MS);
        void check();
      };
      const stop = () => { if (t) { clearInterval(t); t = undefined; } };

      start();
      const onVisible = () => document.visibilityState === "visible" ? start() : stop();
      document.addEventListener("visibilitychange", onVisible);
      window.addEventListener("pagehide", stop);

      return () => {
        stop();
        document.removeEventListener("visibilitychange", onVisible);
        window.removeEventListener("pagehide", stop);
      };
    });

    return () => {
      serwist.removeEventListener?.("waiting", onWaiting);
      serwist.removeEventListener?.("controlling", onControlling);
      navigator.serviceWorker.removeEventListener("controllerchange", onControlling);
    };
  }, []);

  return null;
}
