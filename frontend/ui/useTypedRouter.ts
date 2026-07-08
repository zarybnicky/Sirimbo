import { useParams, useSearchParams } from 'next/navigation';
import { useRouter } from 'next/compat/router';
import React from 'react';
import { z } from 'zod';

export const useTypedRouter = <T extends z.Schema>(schema: T) => {
  const router = useRouter();
  const params = useParams<Record<string, string | string[]>>();
  const searchParams = useSearchParams();
  const query = React.useMemo(() => {
    if (router) return router.query;

    const nextQuery: Record<string, string | string[]> = {};

    if (searchParams) {
      for (const [key, value] of searchParams.entries()) {
        const current = nextQuery[key];
        nextQuery[key] =
        current === undefined
          ? value
          : Array.isArray(current)
          ? [...current, value]
          : [current, value];
      }
    }
    return { ...nextQuery, ...params };
  }, [params, router, searchParams]);

  return {
    query: schema.parse(query) as z.infer<typeof schema>,
  };
};

export const zRouterId = z.preprocess(
  (x) => (Array.isArray(x) ? x[0] : x),
  z.coerce
    .number()
    .optional()
    .transform((x) => (x ? x.toString() : '')),
// eslint-disable-next-line unicorn/no-useless-undefined
).prefault(undefined);
export const zRouterString = z.preprocess(
  (x) => (Array.isArray(x) ? x[0] : x),
  z.coerce
    .string()
    .optional()
    .transform((x) => x || ''),
// eslint-disable-next-line unicorn/no-useless-undefined
).prefault(undefined);
