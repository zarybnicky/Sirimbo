export const merge =
  <A, B>(x: A) =>
  <C extends B>(y: C): A & C => ({ ...x, ...y });

export const pick =
  <A, K extends keyof A>(ks: readonly K[]) =>
  (x: A): Pick<A, K> => {
    const y = {} as Pick<A, K>;
    for (const k of ks) {
      y[k] = x[k];
    }
    return y;
  };

export const pickFrom = <A>(): (<K extends keyof A>(
  ks: readonly K[],
) => (x: A) => Pick<A, K>) => pick;

export const omit =
  <K extends string>(ks: readonly K[]) =>
  <V, A extends Record<K, V>>(x: A): Omit<A, K> => {
    const y = { ...x };
    for (const k of ks) {
      delete y[k];
    }
    return y as Omit<A, K>;
  };

export const omitFrom = <A>(): (<K extends keyof A & string>(
  ks: readonly K[],
) => (x: A) => Omit<A, K>) => omit;

type OptionalKeys<O extends object> = {
  [K in keyof O]-?: Record<string, unknown> extends Pick<O, K> ? K : never;
}[keyof O];

type Exact<A extends object, B extends A> = A & Record<Exclude<keyof B, keyof A>, never>;

export const withDefaults: <
  T extends object,
  PT extends Exact<{ [K in OptionalKeys<T>]-?: Exclude<T[K], undefined> }, PT>,
>(
  defaults: PT,
) => (t: T) => PT & T = merge;
