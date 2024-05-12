import { buildId } from '@/lib/build-id';
import { useEffect, useRef, useState } from 'react';
import { toast } from 'react-toastify';

function useInterval<P extends (() => void)>(
  callback: P,
  { interval, lead }: { interval: number; lead?: boolean },
): void {
  const savedCallback = useRef<P | null>(null);

  useEffect(() => {
    savedCallback.current = callback;
  }, [callback]);

  useEffect(() => {
    const tick = (): void => savedCallback.current?.();

    lead && tick();

    if (interval !== null) {
      const id = setInterval(tick, interval);
      return () => clearInterval(id);
    }
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
          window.location.reload();
        },
        onClose() {
          setState('ignored');
          setTimeout(() => setState('ok'), 300_000);
        },
      });
      return 'prompt';
    });
  }, { interval: 30_000, lead: true });
  return null;
}
