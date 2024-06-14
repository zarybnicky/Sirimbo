import type { GraphCacheConfig } from '@/graphql';
import { CurrentUserDocument, CurrentUserQuery } from '@/graphql/CurrentUser';
import { storeRef, tokenAtom } from '@/ui/state/auth';
import { print } from '@0no-co/graphql.web';
import { authExchange } from '@urql/exchange-auth';
import { cacheExchange } from '@urql/exchange-graphcache';
import { relayPagination } from '@urql/exchange-graphcache/extras';
import { retryExchange } from '@urql/exchange-retry';
import { TypedEventTarget } from 'typescript-event-target';
import type { ClientOptions, CombinedError, Exchange, ExecutionResult, Operation, SSRExchange, TypedDocumentNode } from 'urql';
import { fetchExchange, mapExchange } from 'urql';
import { pipe, tap } from 'wonka';
import schema from './introspection.json';
import { errorTarget } from '@/ui/ErrorNotifier';

export const origin =
  typeof window === 'undefined'
  ? (process.env.GRAPHQL_BACKEND ?? `http://localhost:${process.env.PORT || 3000}`)
  : (process.env.NEXT_PUBLIC_GRAPHQL_BACKEND ?? window.origin);

export async function fetchGql<TResult, TVariables>(
  document: TypedDocumentNode<TResult, TVariables>,
  variables: TVariables,
): Promise<TResult> {
  const token = storeRef.current.get(tokenAtom);
  const response = await fetch(origin + '/graphql', {
    method: 'POST',
    credentials: 'include',
    headers: {
      'content-type': 'application/json',
      ...(process.env.NEXT_PUBLIC_TENANT_ID ? {
        'x-tenant-id': process.env.NEXT_PUBLIC_TENANT_ID,
      } : {}),
      ...(token ? {
        Authorization: `Bearer ${token}`,
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

let devToolsExchange: Exchange = ({ forward }) => forward;
if (process.env.NODE_ENV === 'development') {
  devToolsExchange = require('@urql/devtools').devtoolsExchange;
}

type ErrorEventTarget = TypedEventTarget<{ error: CustomEvent<CombinedError> }>;
const errorEmitter = (errorTarget: ErrorEventTarget) => mapExchange({
  onError(error) {
    errorTarget.dispatchTypedEvent('error', new CustomEvent('error', { detail: error }))
  },
});

const noopExchange: Exchange = ({ forward }) => forward;
const refocusReloadExchange: Exchange = ({ client, forward }) => ops$ => {
  const watchedOperations = new Map<number, Operation>();
  const observedOperations = new Set<number>();

  window.addEventListener('focus', () => {
    if (typeof document !== 'object' || document.visibilityState === 'visible') {
      watchedOperations.forEach(op => {
        client.reexecuteOperation(
          client.createRequestOperation('query', op, {
            ...op.context,
            requestPolicy: 'cache-and-network',
          })
        );
      });
    }
  });

  const processIncomingOperation = (op: Operation) => {
    if (op.kind === 'query' && !observedOperations.has(op.key)) {
      observedOperations.add(op.key);
      watchedOperations.set(op.key, op);
    }
    if (op.kind === 'teardown' && observedOperations.has(op.key)) {
      observedOperations.delete(op.key);
      watchedOperations.delete(op.key);
    }
  };

  return forward(pipe<Operation, Operation>(ops$, tap(processIncomingOperation)));
};

export const configureUrql = (ssrExchange?: SSRExchange): ClientOptions => ({
  url: `${origin}/graphql`,
  requestPolicy: 'cache-and-network',
  exchanges: [
    devToolsExchange,
    errorEmitter(errorTarget),
    typeof window === 'undefined' ? noopExchange : refocusReloadExchange,
    typeof window === 'undefined' ? noopExchange : cacheExchange({
      schema,
      // storage: makeDefaultStorage({
      //   idbName: 'graphcache-v4',
      //   maxAge: 7,
      // }),
      ...cacheConfig,
    }),
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
          const token = storeRef.current.get(tokenAtom);
          if (!token) return operation;
          return utils.appendHeaders(operation, {
            Authorization: `Bearer ${token}`,
          });
        },
      };
    }),
    ssrExchange ?? noopExchange,
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
    AddressDomain: () => null,
    DatetimeRangeBound: () => null,
    EventInstanceAttendanceSummaryRecord: () => null,
    DatetimeRange: () => null,
    PersonWeeklyAttendanceRecord: () => null,
    Price: () => null,
    Scoreboard: (x) => x.personId || null,
  },
  resolvers: {
    Query: {
      upozornenis: relayPagination(),
    },
  },
  updates: {
    Mutation: {
      createMembershipApplication(_result, _args, cache, _info) {
        cache.invalidate('Query', 'membershipApplicationsList');
      },
      confirmMembershipApplication(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'MembershipApplication', id: args.input.applicationId! });
      },
      deleteMembershipApplication(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'MembershipApplication', id: args.input.id });
      },

      updateCohort(_result, args, cache, _info) {
        if (args.input.patch.cohortGroupId) {
          cache.invalidate({ __typename: 'CohortGroup', id: args.input.patch.cohortGroupId});
        }
      },

      deletePerson(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Person', id: args.input.id});
      },

      deleteCohort(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Cohort', id: args.input.id});
      },

      deleteUpozorneniById(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Upozorneni', id: args.input.id});
      },

      deleteEvent(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Event', id: args.input.id});
      },

      registerToEventMany(result, _args, cache, _info) {
        result.registerToEventMany?.eventRegistrations?.forEach(registration => {
          cache.invalidate({ __typename: 'Event', id: registration.eventId});
        })
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
      createTenantLocation(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Tenant', id: args.input.tenantLocation.tenantId});
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

      deleteEventInstance(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'EventInstance', id: args.input.id });
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
          .forEach(field => cache.invalidate('Query', field.fieldKey));
      },

      upsertEvent(_result, args, cache, _info) {
        if (!args.input.info?.id) {
          cache
            .inspectFields('Query')
            .filter(field => field.fieldName.includes('eventInstances'))
            .forEach(field => cache.invalidate('Query', field.fieldName, field.arguments));
        }
      },

      createPersonInvitation(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Person', id: args.input.personInvitation.personId! });
      },

      createCreditTransaction(_result, args, cache, _info) {
        cache.invalidate({ __typename: 'Account', id: args.input.vAccountId! });
      },

      updateUpozorneniById(_result, _args, cache) {
        cache
          .inspectFields('Query')
          .filter(field => ['myAnnouncements', 'stickyAnnouncements'].includes(field.fieldName))
          .forEach(field => cache.invalidate('Query', field.fieldName, field.arguments));
      },
      updateUpozorneni(_result, _args, cache) {
        cache
          .inspectFields('Query')
          .filter(field => ['myAnnouncements', 'stickyAnnouncements'].includes(field.fieldName))
          .forEach(field => cache.invalidate('Query', field.fieldName, field.arguments));
      },
      updatePayment(_result, _args, cache) {
        cache
          .inspectFields('Query')
          .filter(field => field.fieldName.includes('paymentDebtorsList'))
          .forEach(field => cache.invalidate('Query', field.fieldName, field.arguments));
      },

      login(result, _args, cache, _info) {
        const { usr, jwt } = result.login?.result || {};
        if (jwt) {
          storeRef.current.set(tokenAtom, jwt);
        }
        cache.updateQuery({ query: CurrentUserDocument }, (old) => {
          return usr ? ({getCurrentUser: usr, refreshJwt: jwt} as CurrentUserQuery) : old;
        });
      },
      otpLogin(result, _args, cache, _info) {
        const { usr, jwt } = result.otpLogin?.result || {};
        if (jwt) {
          storeRef.current.set(tokenAtom, jwt);
        }
        cache.updateQuery({ query: CurrentUserDocument }, (old) => {
          return usr ? ({getCurrentUser: usr, refreshJwt: jwt} as CurrentUserQuery) : old;
        });
      },
      logInAs(result, _args, cache, _info) {
        const { usr, jwt } = result.logInAs?.result || {};
        if (jwt) {
          storeRef.current.set(tokenAtom, jwt);
        }
        cache.updateQuery({ query: CurrentUserDocument }, (old) => {
          return usr ? ({getCurrentUser: usr, refreshJwt: jwt} as CurrentUserQuery) : old;
        });
      },

      registerUsingInvitation(result, _args, cache, _info) {
        const { usr, jwt } = result.registerUsingInvitation?.result || {};
        if (jwt) {
          storeRef.current.set(tokenAtom, jwt);
        }
        cache.updateQuery({ query: CurrentUserDocument }, (old) => {
          return usr ? ({getCurrentUser: usr, refreshJwt: jwt} as CurrentUserQuery) : old;
        });
      },

      registerWithoutInvitation(result, _args, cache, _info) {
        const { usr, jwt } = result.registerWithoutInvitation?.result || {};
        if (jwt) {
          storeRef.current.set(tokenAtom, jwt);
        }
        cache.updateQuery({ query: CurrentUserDocument }, (old) => {
          return usr ? ({getCurrentUser: usr, refreshJwt: jwt} as CurrentUserQuery) : old;
        });
      },
    },
  },
};
