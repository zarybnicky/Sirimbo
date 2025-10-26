import { buildId } from '@/lib/build-id';
import type { Serwist, SerwistLifecycleEvent, SerwistLifecycleWaitingEvent } from '@serwist/window';
import { useSetAtom } from 'jotai';
import { useCallback, useEffect, useRef, useState } from 'react';
import { toast } from 'react-toastify';
import { buildIdAtom } from './state/build-id';

const LAST_APP_VERSION_KEY = 'app:lastVersion';

function normaliseAssetUrl(value: string, origin: string): string | null {
  try {
    return new URL(value, origin).href;
  } catch {
    return null;
  }
}

function getTrackedAssetUrls(): string[] {
  if (typeof document === 'undefined') return [];

  const origin = window.location.origin;

  const isTrackedUrl = (value: string | null): value is string =>
    typeof value === 'string' && value.startsWith(`${origin}/_next/`);

  const scriptUrls = Array.from(document.querySelectorAll<HTMLScriptElement>('script[src]'))
    .map(element => normaliseAssetUrl(element.src, origin))
    .filter(isTrackedUrl);

  const stylesheetUrls = Array.from(
    document.querySelectorAll<HTMLLinkElement>('link[rel="stylesheet"][href]'),
  )
    .map(element => normaliseAssetUrl(element.href, origin))
    .filter(isTrackedUrl);

  return Array.from(new Set([...scriptUrls, ...stylesheetUrls]));
}

async function assetsAreFresh(): Promise<boolean> {
  if (typeof window === 'undefined') return true;

  const urls = getTrackedAssetUrls();
  if (!urls.length) return true;

  const results = await Promise.all(
    urls.map(async url => {
      try {
        const response = await fetch(url, { method: 'HEAD', cache: 'no-store' });
        return response.ok;
      } catch {
        return false;
      }
    }),
  );

  return results.every(Boolean);
}

function useInterval<P extends (() => void)>(
  callback: P,
  { interval, lead }: { interval: number; lead?: boolean },
): void {
  const savedCallback = useRef<P | null>(null);
  const timerId = useRef<NodeJS.Timeout | null>(null);

  useEffect(() => {
    savedCallback.current = callback;
  }, [callback]);

  useEffect(() => {
    const tick = (): void => savedCallback.current?.();

    if (lead) tick();

    timerId.current = setInterval(tick, interval);

    const visibilityCallback = () => {
      if (document.hidden) {
        if (timerId.current) {
          clearInterval(timerId.current);
          timerId.current = null;
        }
      } else {
        if (!timerId.current) {
          timerId.current = setInterval(tick, interval);
        }
      }
    };

    document.addEventListener("visibilitychange", visibilityCallback);

    return () => {
      document.removeEventListener("visibilitychange", visibilityCallback);
      if (timerId.current)
        clearInterval(timerId.current);
    };
  }, [lead, interval]);
}

export function UpdateNotifier() {
  const setBuildId = useSetAtom(buildIdAtom);

  const toastId = useRef<ReturnType<typeof toast.warn> | null>(null);
  const [, setState] = useState<'ok' | 'prompt' | 'ignored'>('ok');
  const checkingForUpdate = useRef(false);

  const promptUpdate = useCallback((onConfirm: () => void) => {
    setState(prevState => {
      if (prevState === 'prompt' || prevState === 'ignored') return prevState;
      toastId.current = toast.warn(
        <>
          <b>Je k dispozici nová verze aplikace.</b>{' '}
          Kliknutím zde ji aktualizujete
        </>,
        {
          autoClose: false,
          closeOnClick: false,
          onClick() {
            onConfirm();
          },
          onClose() {
            toastId.current = null;
            setState('ignored');
            setTimeout(() => setState('ok'), 300_000);
          },
        },
      );
      return 'prompt';
    });
  }, []);

  useInterval(async () => {
    if (checkingForUpdate.current || typeof window === 'undefined') return;
    checkingForUpdate.current = true;
    const fresh = await assetsAreFresh();
    checkingForUpdate.current = false;

    if (fresh) return;

    promptUpdate(() => {
      window.location.reload();
    });
  }, { interval: 60_000, lead: true });

  useEffect(() => {
    if (typeof window === 'undefined') return;

    let cleanup = () => {};
    let waitForSerwist: ReturnType<typeof setInterval> | null = null;

    const register = (serwist: Serwist) => {
      const handleWaiting = async (_event: SerwistLifecycleWaitingEvent) => {
        promptUpdate(() => {
          serwist.messageSkipWaiting();
        });
      };

      const handleControlling = (event: SerwistLifecycleEvent) => {
        if (event.isUpdate) {
          if (toastId.current) {
            toast.dismiss(toastId.current);
            toastId.current = null;
          }
          setState('ok');
          window.location.reload();
        }
      };

      const handleActivated = (event: SerwistLifecycleEvent) => {
        if (event.isUpdate && toastId.current) {
          toast.dismiss(toastId.current);
          toastId.current = null;
        }
      };

      serwist.addEventListener('waiting', handleWaiting);
      serwist.addEventListener('controlling', handleControlling);
      serwist.addEventListener('activated', handleActivated);

      cleanup = () => {
        serwist.removeEventListener('waiting', handleWaiting);
        serwist.removeEventListener('controlling', handleControlling);
        serwist.removeEventListener('activated', handleActivated);
      };
    };

    const maybeRegister = () => {
      if (!window.serwist) return;
      register(window.serwist);
      if (waitForSerwist) {
        clearInterval(waitForSerwist);
        waitForSerwist = null;
      }
    };

    if (window.serwist) {
      register(window.serwist);
    } else {
      waitForSerwist = setInterval(maybeRegister, 1_000);
    }

    return () => {
      cleanup();
      if (waitForSerwist) {
        clearInterval(waitForSerwist);
      }
    };
  }, [promptUpdate]);

  useEffect(() => {
    if (typeof window === 'undefined') return;

    try {
      const previousVersion = window.localStorage.getItem(LAST_APP_VERSION_KEY);
      if (previousVersion && previousVersion !== buildId) {
        toast.success('Aplikace je aktualizována.');
      }
      window.localStorage.setItem(LAST_APP_VERSION_KEY, buildId);
      setBuildId(buildId);
    } catch (error) {
      console.warn('UpdateNotifier: failed to persist app version', error);
    }
  }, [setBuildId, buildId]);
  return null;
}
