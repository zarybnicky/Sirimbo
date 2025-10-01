import { buildId } from '@/lib/build-id';
import { useEffect, useRef, useState } from 'react';
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
  useInterval(async () => {
    const { buildId: newBuildId } = await fetch('/api/build-id').then(x => x.json(), () => ({})) || {};
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
          globalThis.location.reload();
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
