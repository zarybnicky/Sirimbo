import 'server-only';

import { getRequestTenant } from '@/lib/server/tenant';
import { SESSION_COOKIE } from '@/lib/session-cookies';
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
>(
  document: TypedDocumentNode<TResult, TVariables>,
  variables?: TVariables,
  headers?: Record<string, string>,
) {
  const tenant = await getRequestTenant();
  const cookieStore = await cookies();
  const token = cookieStore.get(SESSION_COOKIE)?.value;
  const response = await fetch(graphqlUrl(), {
    method: 'POST',
    cache: 'no-store',
    headers: {
      ...headers,
      'content-type': 'application/json',
      'x-tenant-id': tenant.id.toString(),
      ...(token ? { cookie: `${SESSION_COOKIE}=${token}` } : {}),
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
