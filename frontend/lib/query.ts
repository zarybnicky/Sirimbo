import type { ExecutionResult, TypedDocumentNode } from 'urql';
import { print } from '@0no-co/graphql.web';
import type { GraphCacheConfig } from 'lib/graphql';
import { CurrentUserDocument, CurrentUserQuery } from './graphql/CurrentUser';
import { simplePagination } from '@urql/exchange-graphcache/extras';

export const origin =
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

  const result: ExecutionResult = await response.json();
  if (result.errors?.length) {
    const rawError = result.errors[0];
    if (rawError) {
      throw rawError;
    }
  }
  return result.data! as TResult;
}

export const cacheConfig: Partial<GraphCacheConfig> = {
  keys: {
    Attachment: (x) => x.objectName || null,
  },
  resolvers: {
    Query: {
      upozornenis: simplePagination({
        limitArgument: 'offset',
      }),
    },
  },
  updates: {
    Mutation: {
      deleteRozpi(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Rozpis', id: args.input.rId});
      },
      updateSkupiny(_result, args, cache, _info) {
        if (args.input.patch.cohortGroup) {
          cache.invalidate({ __typename: 'CohortGroup', id: args.input.patch.cohortGroup});
        }
      },
      deleteSkupiny(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Skupiny', id: args.input.sId});
      },
      deleteUpozorneni(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Upozorneni', id: args.input.upId});
      },
      deleteUser(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'User', id: args.input.uId});
      },
      deleteEvent(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Event', id: args.input.id});
      },
      deleteCohortGroup(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'CohortGroup', id: args.input.id});
      },
      deleteNabidka(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Nabidka', id: args.input.nId});
      },
      createParticipationExternal(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Event', id: args.input.eventId});
      },
      createParticipation(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Event', id: args.input.eventId});
      },
      cancelParticipation(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Event', id: args.input.eventId});
      },

      login(_result, args, cache, _info) {
        cache.updateQuery({ query: CurrentUserDocument }, (old) => {
          const login = _result.login?.result;
          if (!login) return old;
          return {
            getCurrentUser: login.usr,
            getCurrentCouple: login.couple,
          } as CurrentUserQuery;
        });
      },
      logout(_result, args, cache, _info) {
        cache.updateQuery({query: CurrentUserDocument}, () => null);
      },
    },
  },
};
