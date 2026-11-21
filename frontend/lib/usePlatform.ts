import { useSyncExternalStore } from 'react';

// The browser platform stays fixed for the life of the page.
const unsubscribe = () => {};
const subscribe = () => unsubscribe;

export function usePlatform() {
  return useSyncExternalStore(
    subscribe,
    () => navigator.platform,
    () => null,
  );
}
