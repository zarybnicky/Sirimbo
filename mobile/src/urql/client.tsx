import { authExchange } from '@urql/exchange-auth';
import React from 'react';
import {
  Provider,
  cacheExchange,
  createClient,
  dedupExchange,
  fetchExchange,
  type Client,
} from 'urql';
import { env } from '../config/env';
import { useAuth } from '../auth/AuthProvider';

function buildClient(token: string | null, refresh: () => Promise<unknown>, logout: () => Promise<void>): Client {
  return createClient({
    url: `${env.GRAPHQL_BACKEND}/graphql`,
    requestPolicy: 'cache-and-network',
    fetchOptions: () => ({
      headers: {
        'content-type': 'application/json',
        ...(env.TENANT_ID ? { 'x-tenant-id': env.TENANT_ID } : {}),
        ...(token ? { Authorization: `Bearer ${token}` } : {}),
      },
    }),
    exchanges: [
      dedupExchange,
      cacheExchange,
      authExchange(async (utils) => ({
        addAuthToOperation(operation) {
          if (!token) {
            return operation;
          }

          return utils.appendHeaders(operation, {
            Authorization: `Bearer ${token}`,
          });
        },
        didAuthError(error) {
          return error.graphQLErrors.some((err) =>
            err.extensions?.code === 'UNAUTHENTICATED' ||
            err.message?.toLowerCase().includes('jwt'),
          );
        },
        async refreshAuth() {
          if (!token) return;
          try {
            await refresh();
          } catch (err) {
            await logout();
            throw err;
          }
        },
      })),
      fetchExchange,
    ],
  });
}

export function UrqlProvider({ children }: React.PropsWithChildren) {
  const { token, refresh, logout } = useAuth();
  const client = React.useMemo(() => buildClient(token, refresh, logout), [token, refresh, logout]);

  return <Provider value={client}>{children}</Provider>;
}
