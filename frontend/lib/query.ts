import { type TypedDocumentNode } from '@graphql-typed-document-node/core';
import {
  QueryKey,
  useMutation,
  UseMutationOptions,
  UseMutationResult,
  useQuery,
  UseQueryOptions,
  type UseQueryResult,
} from '@tanstack/react-query';
import type { ExecutionResult } from 'graphql';
import { print } from 'graphql/language/printer';

const origin =
  typeof window === 'undefined'
    ? process.env.GRAPHQL_BACKEND || `http://localhost:${process.env.PORT || 3000}`
    : '';

export async function fetchGql<TResult, TVariables>(
  document: TypedDocumentNode<TResult, TVariables>,
  variables: TVariables,
): Promise<TResult> {
  const response = await fetch(origin + '/graphql', {
    method: 'POST',
    headers: {
      'content-type': 'application/json',
    },
    body: JSON.stringify({
      query: print(document),
      variables,
    }),
  });
  if (response.status !== 200) {
    throw new Error(
      `Failed to fetch: ${response.statusText}. Body: ${await response.text()}`,
    );
  }

  const result: ExecutionResult<TResult> = await response.json();
  if (result.errors?.length) {
    const rawError = result.errors[0];
    if (rawError) {
      throw rawError;
    }
  }
  return result.data!;
}

export function getGqlKey<TResult, TVariables>(
  document: TypedDocumentNode<TResult, TVariables>,
  variables: TVariables,
): QueryKey {
  return [(document.definitions[0] as any).name.value, variables];
}

export function useGqlQuery<TResult, TVariables>(
  document: TypedDocumentNode<TResult, TVariables>,
  variables: TVariables,
  options?: UseQueryOptions<TResult>,
): UseQueryResult<TResult> {
  const key = getGqlKey(document, variables);
  return useQuery(key, () => fetchGql(document, variables), options);
}

export const useGqlMutation = <TData, TVariables, TError = unknown, TContext = unknown>(
  document: TypedDocumentNode<TData, TVariables>,
  options?: UseMutationOptions<TData, TError, TVariables, TContext>,
): UseMutationResult<TData, TError, TVariables, TContext> => {
  return useMutation((variables: TVariables) => fetchGql(document, variables), options);
};
