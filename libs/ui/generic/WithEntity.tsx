import type { TypedDocumentNode } from '@graphql-typed-document-node/core';
import { ErrorPage } from '@app/ui/ErrorPage';
import { Loader } from 'lucide-react';
import React from 'react';
import { useQuery, type UseQueryResponse } from 'urql';

type EntityFetcher<U> = {
  useQuery: (id?: string) => UseQueryResponse<U, { id: string }>;
};

export const makeEntityFetcher =
  <T extends object>(query: TypedDocumentNode<T, { id: string }>) =>
  <U,>(get: (x: T | undefined) => U | undefined): EntityFetcher<U> => ({
    useQuery: (id: string = ''): UseQueryResponse<U, { id: string }> => {
      const [result, refetch] = useQuery({ query, variables: { id }, pause: !id });
      return [
        {
          ...result,
          data: get(result.data),
        },
        refetch,
      ];
    },
  });

export const WithEntity = <T,>({
  fetcher,
  id,
  children,
  onSuccess,
}: {
  fetcher: EntityFetcher<T>;
  id?: string;
  children:  React.ReactElement<{ data?: T; id?: string }>;
  onSuccess?: () => void;
}): JSX.Element => {
  const [{ data, fetching }] = fetcher.useQuery(id);
  const props = React.useMemo(() => ({ data, id, onSuccess }), [data, id, onSuccess]);
  if (fetching) return <Loader />;
  if (!fetching && !data) return <ErrorPage error="Nenalezeno" />;
  return React.cloneElement(children, { ...children.props, ...props });
};
