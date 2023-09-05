import type { ClientOptions, CombinedError, ExecutionResult, SSRExchange, TypedDocumentNode } from 'urql';
import { print } from '@0no-co/graphql.web';
import type { GraphCacheConfig } from '@app/graphql';
import { CurrentUserDocument, CurrentUserQuery } from '@app/graphql/CurrentUser';
import { relayPagination } from '@urql/exchange-graphcache/extras';
import { fetchExchange, mapExchange } from 'urql';
import { cacheExchange } from '@urql/exchange-graphcache';
import { retryExchange } from '@urql/exchange-retry';
import { authExchange } from '@urql/exchange-auth';
import { refocusExchange } from '@urql/exchange-refocus';
import { TypedEventTarget } from 'typescript-event-target';
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

export const authState = {
  token: typeof window === 'object' ? localStorage.getItem('token') : undefined,
}

export const configureUrql = (errorTarget: TypedEventTarget<{ error: CustomEvent<CombinedError> }>) => (ssrExchange?: SSRExchange): ClientOptions => ({
  url: `${origin}/graphql`,
  requestPolicy: 'cache-and-network',
  exchanges: [
    mapExchange({
      onError(error) {
        errorTarget.dispatchTypedEvent('error', new CustomEvent('error', { detail: error }))
      },
    }),
    refocusExchange(),
    typeof window !== 'undefined' ? cacheExchange({
      schema,
      // storage: makeDefaultStorage({
      //   idbName: 'graphcache-v4',
      //   maxAge: 7,
      // }),
      ...cacheConfig,
    }) : (({forward}) => forward),
    retryExchange({
      initialDelayMs: 1000,
      maxDelayMs: 15000,
      randomDelay: true,
      maxNumberAttempts: 2,
      retryIf: (err) => !!err && !!err.networkError,
    }),
    authExchange(async (utils) => {
      return {
        didAuthError() {
          return false;
        },
        async refreshAuth() {
        },
        addAuthToOperation(operation) {
          if (!authState.token) return operation;
          return utils.appendHeaders(operation, {
            Authorization: `Bearer ${authState.token}`,
          });
        },
      };
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
    Scoreboard: (x) => x.personId || null,
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

      deletePerson(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Person', id: args.input.id});
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

      createCouple(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Person', id: args.input.couple.manId});
        cache.invalidate({ __typename: 'Person', id: args.input.couple.womanId});
      },
      createCohortMembership(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Person', id: args.input.cohortMembership.personId});
      },
      createTenantMembership(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Person', id: args.input.tenantMembership.personId});
      },
      createTenantTrainer(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Person', id: args.input.tenantTrainer.personId});
      },
      createTenantAdministrator(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Person', id: args.input.tenantAdministrator.personId});
      },
      createUserProxy(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Person', id: args.input.userProxy.personId});
      },

      deleteCouple(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Couple', id: args.input.id});
      },
      deleteCohortMembership(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'CohortMembership', id: args.input.id});
      },
      deleteTenantMembership(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'TenantMembership', id: args.input.id});
      },
      deleteTenantTrainer(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'TenantTrainer', id: args.input.id});
      },
      deleteTenantAdministrator(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'TenantAdministrator', id: args.input.id});
      },
      deleteUserProxy(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'UserProxy', id: args.input.id});
      },

      createAttachment(_result, _args, cache, _info) {
        cache
          .inspectFields('Query')
          .filter(field => ['attachments'].includes(field.fieldName))
          .forEach(field => cache.invalidate('Query', field.fieldName, field.arguments));
      },

      createPerson(_result, _args, cache, _info) {
        cache
          .inspectFields('Query')
          .filter(field => field.fieldName.includes('filteredPeopleList'))
          .forEach(field => cache.invalidate('Query', field.fieldName, field.arguments));
      },

      createEvent(_result, _args, cache, _info) {
        cache
          .inspectFields('Query')
          .filter(field => field.fieldName.includes('eventInstances'))
          .forEach(field => cache.invalidate('Query', field.fieldName, field.arguments));
      },

      updateUpozorneni(_result, _args, cache) {
        cache
          .inspectFields('Query')
          .filter(field => ['myAnnouncements', 'stickyAnnouncements'].includes(field.fieldName))
          .forEach(field => cache.invalidate('Query', field.fieldName, field.arguments));
      },

      login(result, _args, cache, _info) {
        const { usr, jwt } = result.login?.result || {};
        if (jwt) {
          authState.token = jwt;
          localStorage.setItem('token', jwt);
        }
        cache.updateQuery({ query: CurrentUserDocument }, (old) => {
          return usr ? ({getCurrentUser: usr} as CurrentUserQuery) : old;
        });
      },

      registerUsingInvitation(result, _args, cache, _info) {
        const { usr, jwt } = result.registerUsingInvitation?.result || {};
        if (jwt) {
          authState.token = jwt;
          localStorage.setItem('token', jwt);
        }
        cache.updateQuery({ query: CurrentUserDocument }, (old) => {
          return usr ? ({getCurrentUser: usr} as CurrentUserQuery) : old;
        });
      },
    },
  },
};
