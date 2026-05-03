type PgtypedArrays<T extends Record<string, unknown>> = {
  [K in keyof T]: T[K][];
};

export function makePgtypedCollection<T extends Record<string, unknown>>(
  keys: readonly (keyof T)[],
  dedupBy?: readonly (keyof T)[],
): {
  add(...items: T[]): void;
  readonly params: PgtypedArrays<T>;
  readonly length: number;
} {
  const params = Object.fromEntries(
    keys.map((k) => [k, [] as unknown[]]),
  ) as PgtypedArrays<T>;
  const seen = new Set<string>();
  let length = 0;

  return {
    add(...items: T[]) {
      for (const item of items) {
        if (dedupBy) {
          const key = dedupBy.map((k) => item[k]).join('\0');
          if (seen.has(key)) return;
          seen.add(key);
        }
        for (const k of keys) {
          params[k].push(item[k]);
        }
        length++;
      }
    },
    get params() {
      return params;
    },
    get length() {
      return length;
    },
  };
}
