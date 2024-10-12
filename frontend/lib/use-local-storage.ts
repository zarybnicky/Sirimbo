import React from 'react';

const setLocalStorageItem = (key: string, value: string | null | undefined) => {
  if (value) {
    window.localStorage.setItem(key, value);
  } else {
    window.localStorage.removeItem(key);
  }
  window.dispatchEvent(new StorageEvent('storage', { key, newValue: value }));
};

const getLocalStorageItem = (key: string) => window.localStorage.getItem(key);

const useLocalStorageSubscribe = (callback: (ev: StorageEvent) => void) => {
  const realCallback = (ev: StorageEvent) => {
    if (ev.type !== 'sessionStorage') {
      callback(ev);
    }
  };
  window.addEventListener('storage', realCallback);
  return () => window.removeEventListener('storage', realCallback);
};

const getLocalStorageServerSnapshot = () => undefined;

export function useLocalStorage(key: string, initialValue: string | null | undefined) {
  const getSnapshot = () => getLocalStorageItem(key);

  const store = React.useSyncExternalStore(
    useLocalStorageSubscribe,
    getSnapshot,
    getLocalStorageServerSnapshot,
  );

  const setState = React.useCallback(
    (v: React.SetStateAction<string | null | undefined>) => {
      try {
        const nextState = typeof v === 'function' ? v(store) : v;
        if (nextState === undefined || nextState === null) {
          window.localStorage.removeItem(key);
          window.dispatchEvent(new StorageEvent('storage', { key, newValue: null }));
        } else {
          setLocalStorageItem(key, nextState);
        }
      } catch (e) {
        console.warn(e);
      }
    },
    [key, store],
  );

  React.useEffect(() => {
    if (getLocalStorageItem(key) === null && typeof initialValue !== 'undefined') {
      setLocalStorageItem(key, initialValue);
    }
  }, [key, initialValue]);

  return [store ? store : initialValue, setState] as const;
}


const setSessionStorageItem = (key: string, value: string | null | undefined) => {
  if (value) {
    window.sessionStorage.setItem(key, value);
  } else {
    window.sessionStorage.removeItem(key);
  }
  window.dispatchEvent(new StorageEvent('sessionStorage', { key, newValue: value }));
};

const getSessionStorageItem = (key: string) => window.sessionStorage.getItem(key);

const useSessionStorageSubscribe = (callback: (ev: StorageEvent) => void) => {
  const realCallback = (ev: StorageEvent) => {
    if (ev.type === 'sessionStorage') {
      callback(ev);
    }
  };
  window.addEventListener('storage', realCallback);
  return () => window.removeEventListener('storage', realCallback);
};

const getSessionStorageServerSnapshot = () => undefined;

export function useSessionStorage(key: string, initialValue: string | null | undefined) {
  const getSnapshot = () => getSessionStorageItem(key);

  const store = React.useSyncExternalStore(
    useSessionStorageSubscribe,
    getSnapshot,
    getSessionStorageServerSnapshot,
  );

  const setState = React.useCallback(
    (v: React.SetStateAction<string | null | undefined>) => {
      try {
        const nextState = typeof v === 'function' ? v(store) : v;
        if (nextState === undefined || nextState === null) {
          window.sessionStorage.removeItem(key);
          window.dispatchEvent(
            new StorageEvent('sessionStorage', { key, newValue: null }),
          );
        } else {
          setSessionStorageItem(key, nextState);
        }
      } catch (e) {
        console.warn(e);
      }
    },
    [key, store],
  );

  React.useEffect(() => {
    if (getSessionStorageItem(key) === null && typeof initialValue !== 'undefined') {
      setSessionStorageItem(key, initialValue);
    }
  }, [key, initialValue]);

  return [store ? store : initialValue, setState] as const;
}
