import React from 'react';

// Unmounting has to flush rather than cancel: navigating away within the
// debounce window would otherwise drop whatever was typed last.
export function useDebounced<T>(fn: (value: T) => void | Promise<void>, delay: number) {
  const timer = React.useRef<ReturnType<typeof setTimeout>>(undefined);
  const pending = React.useRef<{ value: T } | null>(null);
  const latest = React.useRef(fn);

  React.useEffect(() => {
    latest.current = fn;
  });

  React.useEffect(
    () => () => {
      clearTimeout(timer.current);
      if (pending.current) {
        void latest.current(pending.current.value);
        pending.current = null;
      }
    },
    [],
  );

  return React.useCallback(
    (value: T) => {
      clearTimeout(timer.current);
      pending.current = { value };
      timer.current = setTimeout(() => {
        pending.current = null;
        void latest.current(value);
      }, delay);
    },
    [delay],
  );
}
