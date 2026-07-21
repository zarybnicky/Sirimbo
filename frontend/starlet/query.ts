import { print } from '@0no-co/graphql.web';
import type { ExecutionResult, TypedDocumentNode } from 'urql';
import { origin } from '@/lib/query';
import { starletTokenAtom } from '@/ui/starlet-importer/state';
import { storeRef } from '@/ui/state/auth';

export async function fetchStarlet<TResult, TVariables>(
  document: TypedDocumentNode<TResult, TVariables>,
  variables: TVariables,
): Promise<TResult> {
  const tokenAtom = storeRef.current.get(starletTokenAtom);
  const token = tokenAtom?.auth_ok ? tokenAtom.auth_token : undefined;

  const response = await fetch(origin + '/starlet/graphql', {
    method: 'POST',
    credentials: 'include',
    headers: {
      'content-type': 'application/json',
      ...(token
        ? {
            Authorization: `Bearer ${token}`,
          }
        : {}),
    },
    body: JSON.stringify({
      query: print(document),
      variables,
    }),
  });
  if (!response.ok) {
    throw new Error(
      `Failed to fetch: ${response.statusText}. Body: ${await response.text()}`,
    );
  }

  const result: ExecutionResult = await response.json();
  if (result.errors?.length) {
    const rawError = result.errors[0];
    if (rawError) {
      throw rawError;
    }
  }
  if (result.data === undefined) return result as unknown as TResult;
  return result.data! as TResult;
}
