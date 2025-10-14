import AsyncStorage from '@react-native-async-storage/async-storage';
import type { DocumentNode } from 'graphql';
import { print } from 'graphql';
import React, {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
} from 'react';
import { env } from '../config/env';
import {
  CURRENT_USER_QUERY,
  LOGIN_MUTATION,
  type CurrentUserResponse,
  type LoginResponse,
  type UserAuth,
} from '../graphql/documents';

const TOKEN_KEY = 'rozpisovnik.mobile.token';

export type AuthStatus = 'initializing' | 'loading' | 'authenticated' | 'unauthenticated';

type AuthContextValue = {
  status: AuthStatus;
  token: string | null;
  user: UserAuth | null;
  error: Error | null;
  login: (login: string, passwd: string) => Promise<UserAuth>;
  logout: () => Promise<void>;
  refresh: () => Promise<UserAuth | null>;
};

const AuthContext = createContext<AuthContextValue | undefined>(undefined);

async function graphqlRequest<TResult>(
  document: DocumentNode,
  variables: Record<string, unknown>,
  token?: string | null,
): Promise<TResult> {
  const response = await fetch(`${env.GRAPHQL_BACKEND}/graphql`, {
    method: 'POST',
    headers: {
      'content-type': 'application/json',
      ...(env.TENANT_ID ? { 'x-tenant-id': env.TENANT_ID } : {}),
      ...(token ? { Authorization: `Bearer ${token}` } : {}),
    },
    body: JSON.stringify({
      query: print(document),
      variables,
    }),
  });

  if (!response.ok) {
    throw new Error(`GraphQL request failed with status ${response.status}`);
  }

  const payload = await response.json();
  if (payload.errors?.length) {
    const [first] = payload.errors;
    throw new Error(first?.message ?? 'Unexpected GraphQL error');
  }

  return payload.data as TResult;
}

export function AuthProvider({ children }: React.PropsWithChildren) {
  const [status, setStatus] = useState<AuthStatus>('initializing');
  const [token, setToken] = useState<string | null>(null);
  const [user, setUser] = useState<UserAuth | null>(null);
  const [error, setError] = useState<Error | null>(null);

  const persistToken = useCallback(async (nextToken: string | null) => {
    setToken(nextToken);
    if (nextToken) {
      await AsyncStorage.setItem(TOKEN_KEY, nextToken);
    } else {
      await AsyncStorage.removeItem(TOKEN_KEY);
    }
  }, []);

  const loadUser = useCallback(
    async (providedToken?: string | null) => {
      const activeToken = providedToken ?? token;
      if (!activeToken) {
        await persistToken(null);
        setUser(null);
        setStatus('unauthenticated');
        return null;
      }

      setStatus((current) => (current === 'initializing' ? 'initializing' : 'loading'));
      try {
        const data = await graphqlRequest<CurrentUserResponse>(
          CURRENT_USER_QUERY,
          { versionId: env.VERSION_ID },
          activeToken,
        );

        const refreshedToken = data.refreshJwt ?? activeToken;
        if (refreshedToken !== activeToken) {
          await persistToken(refreshedToken);
        }

        setUser(data.getCurrentUser ?? null);
        setStatus(data.getCurrentUser ? 'authenticated' : 'unauthenticated');
        setError(null);
        return data.getCurrentUser ?? null;
      } catch (err) {
        setError(err as Error);
        await persistToken(null);
        setUser(null);
        setStatus('unauthenticated');
        throw err;
      }
    },
    [persistToken, token],
  );

  useEffect(() => {
    (async () => {
      try {
        const storedToken = await AsyncStorage.getItem(TOKEN_KEY);
        if (storedToken) {
          await persistToken(storedToken);
          await loadUser(storedToken);
        } else {
          setStatus('unauthenticated');
        }
      } catch (err) {
        setError(err as Error);
        setStatus('unauthenticated');
      }
    })();
  }, [loadUser, persistToken]);

  const login = useCallback(
    async (loginValue: string, passwd: string) => {
      setStatus('loading');
      setError(null);
      try {
        const data = await graphqlRequest<LoginResponse>(LOGIN_MUTATION, {
          login: loginValue,
          passwd,
        });
        const result = data.login?.result;
        if (!result?.jwt || !result.usr) {
          const error = new Error('Přihlášení se nezdařilo. Zkontrolujte přihlašovací údaje.');
          setError(error);
          setStatus('unauthenticated');
          throw error;
        }

        await persistToken(result.jwt);
        setUser(result.usr);
        setStatus('authenticated');
        setError(null);
        return result.usr;
      } catch (err) {
        const error = err instanceof Error ? err : new Error(String(err));
        setUser(null);
        setStatus('unauthenticated');
        setError(error);
        throw error;
      }
    },
    [persistToken],
  );

  const logout = useCallback(async () => {
    await persistToken(null);
    setUser(null);
    setStatus('unauthenticated');
    setError(null);
  }, [persistToken]);

  const contextValue = useMemo<AuthContextValue>(
    () => ({
      status,
      token,
      user,
      error,
      login,
      logout,
      refresh: () => loadUser(),
    }),
    [status, token, user, error, login, logout, loadUser],
  );

  return <AuthContext.Provider value={contextValue}>{children}</AuthContext.Provider>;
}

export function useAuth() {
  const value = useContext(AuthContext);
  if (!value) {
    throw new Error('useAuth must be used within an AuthProvider');
  }
  return value;
}
