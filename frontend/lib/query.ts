import { ValueTypes, GraphQLTypes, InputType, Chain, OperationOptions, chainOptions, ScalarDefinition, ZeusScalars } from 'lib/zeus';
import { useQuery, useMutation, QueryKey } from '@tanstack/react-query';
import type { UseQueryOptions, UseMutationOptions } from '@tanstack/react-query';

const scalars = ZeusScalars({
  BigInt: {
    encode: (e: unknown) => (e as number).toString(),
    decode: (e: unknown) => parseInt(e as string, 10),
  },
  JSON: {
    encode: (e: unknown) => JSON.stringify(e),
    decode: (e: unknown) => JSON.parse(e as string),
  },
  Date: {
    decode: (e: unknown) => new Date(e as string),
    encode: (e: unknown) => (e as Date).toDateString(),
  },
  Datetime: {
    decode: (e: unknown) => new Date(e as string),
    encode: (e: unknown) => (e as Date).toISOString(),
  },
  Time: {
    decode: (e: unknown) => new Date(e as string),
    encode: (e: unknown) => (e as Date).toTimeString(),
  },
});

export const origin = typeof window === 'undefined' ? `http://localhost:${process.env.PORT || 3000}` : '';
const source = origin + '/graphql';

export function fetchTypedQuery<
  O extends "Query",
  TData extends ValueTypes[O],
  TResult = InputType<GraphQLTypes[O], TData, typeof scalars>
>(
  query: TData | ValueTypes[O],
  graphqlOptions?: {
    variables: Record<string, unknown>;
    operationOptions?: OperationOptions;
    scalars?: ScalarDefinition;
  },
) {
  return Chain(source)("query", { scalars })(query, {
    variables: graphqlOptions?.variables || {},
  }) as Promise<TResult>;
}

export function useTypedQuery<
  O extends "Query",
  TData extends ValueTypes[O],
  TResult = InputType<GraphQLTypes[O], TData, typeof scalars>
>(
  queryKey: QueryKey,
  query: TData | ValueTypes[O],
  options?: Omit<UseQueryOptions<TResult, any>, 'queryKey' | 'queryFn'>,
  graphqlOptions?: {
    variables: Record<string, unknown>;
    operationOptions?: OperationOptions;
    scalars?: ScalarDefinition;
  },
) {
  return useQuery<TResult, any>(
    queryKey,
    () => Chain(source)("query", { scalars })(query, {
      variables: graphqlOptions?.variables || {},
      operationName: typeof queryKey === 'string' ? queryKey : (queryKey[0] as object).toString(),
    }) as Promise<TResult>,
    options,
  );
}

type Variables = { variables?: Record<string, unknown> };

export function useTypedMutation<
  O extends "Mutation",
  TData extends ValueTypes[O],
  TResult = InputType<GraphQLTypes[O], TData, typeof scalars>
>(
  mutationKey: QueryKey,
  mutation: TData | ValueTypes[O],
  options?: Omit<UseMutationOptions<TResult, any, Variables | void>, 'mutationKey' | 'mutationFn'>,
  graphqlOptions?: Variables & {
    operationOptions?: OperationOptions;
    scalars?: ScalarDefinition;
    host: string;
    hostOptions: chainOptions[1];
  },
) {
  return useMutation<TResult, any, Variables | void>(
    mutationKey,
    (opts) => Chain(source)("mutation", { scalars })(mutation, {
      variables: { ...(graphqlOptions?.variables || {}), ...(opts?.variables || {}) },
      operationName: typeof mutationKey === 'string' ? mutationKey : (mutationKey[0] as object).toString(),
    }) as Promise<TResult>,
    options,
  );
}
