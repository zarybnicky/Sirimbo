import { useRouter } from "next/router";
import { z } from "zod";

export const useTypedRouter = <T extends z.Schema>(schema: T) => {
  const { query, ...router } = useRouter();

  return {
    query: schema.parse(query) as z.infer<typeof schema>,
    ...router
  };
};

export const zRouterId = z.preprocess(x => Array.isArray(x) ? x[0] : x, z.coerce.number().optional().transform(x => x ? x.toString() : ''));
export const zRouterString = z.preprocess(x => Array.isArray(x) ? x[0] : x, z.coerce.string().optional().transform(x => x || ''));
