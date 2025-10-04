import { buildId } from '@/lib/build-id';
import { useEffect, useRef, useState } from 'react';
import { useSetAtom } from 'jotai';
import { buildIdAtom } from '@/ui/state/build-id';
import { toast } from 'react-toastify';

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
  const [, setState] = useState<'ok' | 'prompt' | 'ignored'>('ok');
  const setBuildId = useSetAtom(buildIdAtom);
  useInterval(async () => {
    const newBuildId = await fetch('/api/build-id')
      .then(async response => {
        if (!response.ok) throw new Error('Failed to fetch build id');
        const payload = await response.json();
        return typeof payload?.buildId === 'string' ? payload.buildId : '';
      })
      .catch(() => '');

    setBuildId(newBuildId);

    if (!buildId || !newBuildId || buildId === newBuildId) return;

    setState(prevState => {
      if (prevState === 'prompt' || prevState === 'ignored') return prevState;
      toast.warn((
        <>
          <b>Je k dispozici nová verze aplikace.</b>
          {' '}
          Kliknutím zde ji aktualizujete
        </>
      ), {
        autoClose: false,
        closeOnClick: false,
        onClick() {
          window.location.reload();
        },
        onClose() {
          setState('ignored');
          setTimeout(() => setState('ok'), 300_000);
        },
      });
      return 'prompt';
    });
  }, { interval: 60_000, lead: true });
  return null;
}
