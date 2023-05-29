
export const pick =
  <A, K extends keyof A>(ks: readonly K[]) =>
  (x: A): Pick<A, K> => {
    const y = {} as Pick<A, K>;
    for (const k of ks) {
      y[k] = x[k];
    }
    return y;
  };
