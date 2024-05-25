import * as React from 'react';
import {
  CurrentUserDocument,
  LoginDocument,
  OtpLoginDocument,
  UserAuthFragment,
} from '@/graphql/CurrentUser';
import { useMutation, useQuery } from 'urql';
import { AuthState, tokenAtom, authAtom, defaultAuthState } from '@/ui/state/auth';
import { useAtom } from 'jotai';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';

interface AuthContext extends AuthState {
  isLoading: boolean;
  signIn: (email: string, password: string) => Promise<UserAuthFragment | null>;
  signInWithOtp: (token: string) => Promise<UserAuthFragment | null>;
  signOut: () => void;
}

export const ProvideAuth = React.memo(function ProvideAuth({ children, onReset }: {
  onReset?: () => void;
  children: React.ReactNode;
}) {
  const [token] = useAtom(tokenAtom);
  const [auth, setAuth] = useAtom(authAtom);
  const [firstRender, setFirstRender] = React.useState(true);
  useLayoutEffect(() => setFirstRender(false), []);

  const [isLoading, setIsLoading] = React.useState<boolean>(true);

  const [{ data: currentUser, fetching }, refetch] = useQuery({ query: CurrentUserDocument, pause: !token });

  React.useEffect(() => {
    if (!fetching) {
      setIsLoading(false);
      if (currentUser) {
        setAuth(currentUser.refreshJwt, currentUser.getCurrentUser);
      }
    }
  }, [fetching, setAuth, currentUser]);

  React.useEffect(() => {
    const launchQuery = () => {
      if (typeof document === "undefined" ||
        document.visibilityState === undefined ||
        document.visibilityState === "visible"
      ) {
        refetch({ requestPolicy: 'network-only' });
      }
    };
    const interval = setInterval(launchQuery, 30000);
    return () => clearInterval(interval);
  }, [refetch]);

  const doSignIn = useMutation(LoginDocument)[1];
  const signIn = React.useCallback(
    async (login: string, passwd: string) => {
      setIsLoading(true);
      const result = await doSignIn({ login, passwd });
      setIsLoading(false);
      return result.data?.login?.result?.usr ?? null;
    },
    [doSignIn],
  );

  const doSignInWithOtp = useMutation(OtpLoginDocument)[1];
  const signInWithOtp = React.useCallback(
    async (token: string) => {
      setIsLoading(true);
      const result = await doSignInWithOtp({ token });
      setIsLoading(false);
      return result.data?.otpLogin?.result?.usr ?? null;
    },
    [doSignInWithOtp],
  );

  const signOut = React.useCallback(() => {
    localStorage.removeItem('token');
    setAuth(null, null);
    onReset?.();
  }, [onReset, setAuth]);

  const context = React.useMemo<AuthContext>(() => ({
    isLoading,
    signIn,
    signInWithOtp,
    signOut,
    ...(firstRender ? defaultAuthState : auth),
  }), [isLoading, signIn, signInWithOtp, signOut, auth, firstRender]);

  return (
    <authContext.Provider value={context}>
      {children}
    </authContext.Provider>
  );
});

const authContext = React.createContext<AuthContext | undefined>(undefined);

export const useAuth = () => {
  const auth = React.useContext(authContext);
  if (auth === undefined) {
    throw new Error('You can only use `useAuth` from inside an AuthProvider');
  }
  return auth;
};

