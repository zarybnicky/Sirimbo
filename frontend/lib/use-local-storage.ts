import React from "react";

const dispatchStorageEvent = (key: string, newValue: string | null | undefined) => {
  window.dispatchEvent(new StorageEvent("storage", { key, newValue }));
};

const setLocalStorageItem = (key: string, value: string | null | undefined) => {
  if (value) {
    window.localStorage.setItem(key, value);
  } else {
    window.localStorage.removeItem(key);
  }
  dispatchStorageEvent(key, value);
};

const removeLocalStorageItem = (key: string) => {
  window.localStorage.removeItem(key);
  dispatchStorageEvent(key, null);
};

const getLocalStorageItem = (key: string) => {
  return window.localStorage.getItem(key);
};

const useLocalStorageSubscribe = (callback: ((ev: StorageEvent) => void)) => {
  window.addEventListener("storage", callback);
  return () => window.removeEventListener("storage", callback);
};

const getLocalStorageServerSnapshot = () => undefined;

export function useLocalStorage(key: string, initialValue: string | null | undefined) {
  const getSnapshot = () => getLocalStorageItem(key);

  const store = React.useSyncExternalStore(
    useLocalStorageSubscribe,
    getSnapshot,
    getLocalStorageServerSnapshot
  );

  const setState = React.useCallback((v: React.SetStateAction<string | null | undefined>) => {
    try {
      const nextState = typeof v === "function" ? v(store) : v;
      if (nextState === undefined || nextState === null) {
        removeLocalStorageItem(key);
      } else {
        setLocalStorageItem(key, nextState);
      }
    } catch (e) {
      console.warn(e);
    }
  }, [key, store]);

  React.useEffect(() => {
    if (
      getLocalStorageItem(key) === null &&
      typeof initialValue !== "undefined"
    ) {
      setLocalStorageItem(key, initialValue);
    }
  }, [key, initialValue]);

  return [store ? store : initialValue, setState] as const;
}
