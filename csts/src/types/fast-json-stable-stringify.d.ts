declare module 'fast-json-stable-stringify' {
  interface ComparisonValue {
    key: string;
    value: unknown;
  }

  export interface Options {
    cmp?: (a: ComparisonValue, b: ComparisonValue) => number;
    replacer?: (key: string, value: unknown) => unknown;
    space?: string | number;
  }

  export default function stringify(value: unknown, options?: Options): string;
}
