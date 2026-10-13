'use client';

import { useEffect } from 'react';
import { parseAsInteger, useQueryState } from 'nuqs';
import { useQuery, type AnyVariables, type TypedDocumentNode } from 'urql';

export function usePaginatedQuery<Data, Variables extends AnyVariables>({
  query,
  variables,
  getTotal,
  limit,
  pageKey = 'page',
  pause = false,
}: {
  query: TypedDocumentNode<Data, Variables>;
  variables: (offset: number) => Variables;
  getTotal: (data: Data) => number | undefined;
  limit: number;
  pageKey?: string;
  pause?: boolean;
}) {
  const [page, setPage] = useQueryState(
    pageKey,
    parseAsInteger
      .withDefault(1)
      .withOptions({ shallow: true, scroll: false, history: 'push' }),
  );
  const [result, reexecuteQuery] = useQuery({
    query,
    variables: variables(Math.min(Math.max(0, page - 1) * limit, 2_147_483_647)),
    pause,
  });
  const total = result.data == null ? undefined : getTotal(result.data);
  const { fetching, error } = result;
  useEffect(() => {
    if (pause || fetching || error || total == null) return;
    const clamped = Math.max(1, Math.min(page, Math.ceil(total / limit)));
    if (page !== clamped) void setPage(clamped, { history: 'replace' });
  }, [page, total, limit, pause, fetching, error, setPage]);

  return {
    ...result,
    reexecuteQuery,
    pagination: {
      total: total ?? 0,
      limit,
      page: Math.max(1, page),
      onPageChange: setPage,
    },
  };
}
