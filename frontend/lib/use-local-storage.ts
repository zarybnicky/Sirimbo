import React from 'react';

const setLocalStorageItem = (key: string, value: string | null | undefined) => {
  if (value) {
    localStorage.setItem(key, value);
  } else {
    localStorage.removeItem(key);
  }
  window.dispatchEvent(new StorageEvent('storage', { key, newValue: value, storageArea: localStorage }));
};

const useLocalStorageSubscribe = (callback: (ev: StorageEvent) => void) => {
  const realCallback = (ev: StorageEvent) => {
    if (ev.storageArea === localStorage) {
      callback(ev);
    }
  };
  window.addEventListener('storage', realCallback);
  return () => window.removeEventListener('storage', realCallback);
};

const getLocalStorageServerSnapshot = () => undefined;

export function useLocalStorage(key: string, initialValue: string | null | undefined) {
  const getSnapshot = () => localStorage.getItem(key);

  const store = React.useSyncExternalStore(
    useLocalStorageSubscribe,
    getSnapshot,
    getLocalStorageServerSnapshot,
  );

  const setState = React.useCallback(
    (v: React.SetStateAction<string | null | undefined>) => {
      try {
        const nextState = typeof v === 'function' ? v(store) : v;
        setLocalStorageItem(key, nextState);
      } catch (e) {
        console.warn(e);
      }
    },
    [key, store],
  );

  React.useEffect(() => {
    if (localStorage.getItem(key) === null && typeof initialValue !== 'undefined') {
      setLocalStorageItem(key, initialValue);
    }
  }, [key, initialValue]);

  return [store ? store : initialValue, setState] as const;
}


const setSessionStorageItem = (key: string, value: string | null | undefined) => {
  if (value) {
    sessionStorage.setItem(key, value);
  } else {
    sessionStorage.removeItem(key);
  }
  window.dispatchEvent(new StorageEvent('storage', { key, newValue: value, storageArea: sessionStorage }));
};

const useSessionStorageSubscribe = (callback: (ev: StorageEvent) => void) => {
  const realCallback = (ev: StorageEvent) => {
    if (ev.storageArea === sessionStorage) {
      callback(ev);
    }
  };
  window.addEventListener('storage', realCallback);
  return () => window.removeEventListener('storage', realCallback);
};

const getSessionStorageServerSnapshot = () => undefined;

export function useSessionStorage(key: string, initialValue: string | null | undefined) {
  const getSnapshot = () => sessionStorage.getItem(key);

  const store = React.useSyncExternalStore(
    useSessionStorageSubscribe,
    getSnapshot,
    getSessionStorageServerSnapshot,
  );

  const setState = React.useCallback(
    (v: React.SetStateAction<string | null | undefined>) => {
      console.log(v);
      try {
        const nextState = typeof v === 'function' ? v(store) : v;
        setSessionStorageItem(key, nextState);
      } catch (e) {
        console.warn(e);
      }
    },
    [key, store],
  );

  React.useEffect(() => {
    if (sessionStorage.getItem(key) === null && typeof initialValue !== 'undefined') {
      setSessionStorageItem(key, initialValue);
    }
  }, [key, initialValue]);

  return [store ? store : initialValue, setState] as const;
}
