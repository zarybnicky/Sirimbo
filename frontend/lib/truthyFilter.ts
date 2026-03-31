export const isTruthy = Boolean as any as <T>(
  x: T | false | undefined | null | '' | 0,
) => x is T;

export const keyIsNonNull =
  <T, K extends keyof T>(key: K) =>
  (item: T): item is T & { [P in K]-?: NonNullable<T[P]> } =>
    item[key] != null;
