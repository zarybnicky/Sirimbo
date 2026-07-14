import { getRequestTenant } from '@/tenant/server';
import type { TypedDocumentNode } from '@graphql-typed-document-node/core';
import { print } from 'graphql';
import { cookies } from 'next/headers';

type GraphqlError = {
  message: string;
};

type GraphqlResponse<TResult> = {
  data?: TResult;
  errors?: GraphqlError[];
};

function graphqlUrl() {
  const origin =
    process.env.GRAPHQL_BACKEND ??
    process.env.NEXT_PUBLIC_GRAPHQL_BACKEND ??
    `http://localhost:${process.env.PORT || 3000}`;
  return `${origin.replace(/\/graphql\/?$/, '').replace(/\/$/, '')}/graphql`;
}

export async function executeGraphql<
  TResult,
  TVariables extends Record<string, unknown> = Record<string, never>,
>(document: TypedDocumentNode<TResult, TVariables>, variables?: TVariables) {
  const tenant = await getRequestTenant();
  const cookieStore = await cookies();
  const token = cookieStore.get('rozpisovnik')?.value;
  const response = await fetch(graphqlUrl(), {
    method: 'POST',
    cache: 'no-store',
    headers: {
      'content-type': 'application/json',
      'x-tenant-id': String(tenant.id),
      ...(token ? { cookie: `rozpisovnik=${token}` } : {}),
    },
    body: JSON.stringify({
      query: print(document),
      variables: variables ?? {},
    }),
  });

  if (!response.ok) {
    throw new Error(`GraphQL request failed: ${response.status} ${response.statusText}`);
  }

  const result = (await response.json()) as GraphqlResponse<TResult>;
  if (result.errors?.length) {
    throw new Error(result.errors.map((error) => error.message).join('\n'));
  }
  if (!result.data) {
    throw new Error('GraphQL request returned no data.');
  }
  return result.data;
}
