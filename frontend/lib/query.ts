import { $, ValueTypes, GraphQLTypes, InputType, Chain, OperationOptions, chainOptions, ScalarDefinition, ZeusScalars } from 'lib/zeus';
import { useQuery, useMutation, QueryKey } from '@tanstack/react-query';
import type { UseQueryOptions, UseMutationOptions } from '@tanstack/react-query';

export { $ };

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
    host?: string;
    hostOptions?: chainOptions[1];
  },
) {
  return useQuery<TResult, any>(queryKey, () => Chain(
    graphqlOptions?.host || '/graphql',
    graphqlOptions?.hostOptions || {}
  )("query", { scalars })(query, {
    variables: graphqlOptions?.variables || {},
    operationName: typeof queryKey === 'string' ? queryKey : queryKey.join('_'),
  }) as Promise<TResult>, options);
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
  return useMutation<TResult, any, Variables | void>(mutationKey, (vars) => Chain(
    graphqlOptions?.host || '/graphql',
    graphqlOptions?.hostOptions || {}
  )("mutation", { scalars })(mutation, {
    variables: { ...(graphqlOptions?.variables || {}), ...(vars || {}) },
    operationName: typeof mutationKey === 'string' ? mutationKey : mutationKey.join('_'),
  }) as Promise<TResult>, options);
}
