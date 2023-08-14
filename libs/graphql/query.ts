import type { ClientOptions, ExecutionResult, SSRExchange, TypedDocumentNode } from 'urql';
import { print } from '@0no-co/graphql.web';
import type { GraphCacheConfig } from '@app/graphql';
import { CurrentUserDocument, CurrentUserQuery } from '@app/graphql/CurrentUser';
import { relayPagination } from '@urql/exchange-graphcache/extras';
import { makeDefaultStorage } from '@urql/exchange-graphcache/default-storage';
import { fetchExchange } from 'urql';
import { offlineExchange } from '@urql/exchange-graphcache';
import { retryExchange } from '@urql/exchange-retry';
import { refocusExchange } from '@urql/exchange-refocus';
import { devtoolsExchange } from '@urql/devtools';
import schema from './introspection.json';

export const origin =
  typeof window === 'undefined'
  ? (process.env.GRAPHQL_BACKEND ?? `http://localhost:${process.env.PORT || 3000}`)
  : (process.env.NEXT_PUBLIC_GRAPHQL_BACKEND ?? window.origin);

export async function fetchGql<TResult, TVariables>(
  document: TypedDocumentNode<TResult, TVariables>,
  variables: TVariables,
): Promise<TResult> {
  const response = await fetch(origin + '/graphql', {
    method: 'POST',
    credentials: 'include',
    headers: {
      'content-type': 'application/json',
      ...(process.env.NEXT_PUBLIC_TENANT_ID ? {
        'x-tenant-id': process.env.NEXT_PUBLIC_TENANT_ID,
      } : {}),
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

export const configureUrql = (ssrExchange?: SSRExchange): ClientOptions => ({
  url: `${origin}/graphql`,
  requestPolicy: 'cache-and-network',
  exchanges: [
    process.env.NODE_ENV !== 'production' ? devtoolsExchange : (({forward}) => forward),
    refocusExchange(),
    typeof window !== 'undefined' ? offlineExchange({
      schema,
      storage: makeDefaultStorage({
        idbName: 'graphcache-v3',
        maxAge: 7,
      }),
      ...cacheConfig,
    }) : (({forward}) => forward),
    retryExchange({
      initialDelayMs: 1000,
      maxDelayMs: 15000,
      randomDelay: true,
      maxNumberAttempts: 2,
      retryIf: (err) => !!err && !!err.networkError,
    }),
    ssrExchange ?? (({ forward }) => forward),
    fetchExchange,
  ],
  fetchOptions: {
    credentials: 'include',
    headers: {
      ...(process.env.NEXT_PUBLIC_TENANT_ID ? {
        'x-tenant-id': process.env.NEXT_PUBLIC_TENANT_ID,
      } : {}),
    },
  },
})

const cacheConfig: Partial<GraphCacheConfig> = {
  keys: {
    Attachment: (x) => x.objectName || null,
    GalerieFoto: (x) => x.gfId || null,
    DatetimeRangeBound: () => null,
    DatetimeRange: () => null,
  },
  resolvers: {
    Query: {
      upozornenis: relayPagination(),
    },
  },
  updates: {
    Mutation: {
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
      deleteEvent(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Event', id: args.input.id});
      },
      registerToEvent(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Event', id: args.input.registration.eventId});
      },
      setLessonDemand(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'EventRegistration', id: args.input.registrationId});
      },
      cancelRegistration(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'EventRegistration', id: args.input.registrationId});
      },
      deleteCohortGroup(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'CohortGroup', id: args.input.id});
      },
      createAttachment(_result, _args, cache, _info) {
        cache
          .inspectFields('Query')
          .filter(field => ['attachments'].includes(field.fieldName))
          .forEach(field => cache.invalidate('Query', field.fieldName, field.arguments));
      },

      updateUpozorneni(_result, _args, cache) {
        cache
          .inspectFields('Query')
          .filter(field => ['myAnnouncements', 'stickyAnnouncements'].includes(field.fieldName))
          .forEach(field => cache.invalidate('Query', field.fieldName, field.arguments));
      },
      login(result, _args, cache, _info) {
        cache.updateQuery({ query: CurrentUserDocument }, (old) => {
          const user = result.login?.result?.usr;
          console.log(result);
          if (!user) return old;
          return {
            getCurrentUser: user,
          } as CurrentUserQuery;
        });
      },
      logout(_result, _args, cache, _info) {
        cache.updateQuery({query: CurrentUserDocument}, () => null);
        cache.invalidate('Query', 'currentUserId');
        cache.invalidate('Query', 'currentTenantId');
        cache.invalidate('Query', 'currentSessionId');
        cache.invalidate('Query', 'getCurrentTenant');
        cache.invalidate('Query', 'getCurrentUser');
      },
    },
  },
};
