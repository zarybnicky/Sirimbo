export const truthyFilter = Boolean as any as <T>(x: T | false | undefined | null | "" | 0) => x is T;
