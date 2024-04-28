import * as React from 'react';
import {
  CurrentUserDocument,
  LoginDocument,
  OtpLoginDocument,
  UserAuthFragment,
} from '@/graphql/CurrentUser';
import { useMutation, useQuery } from 'urql';
import { CoupleFragment } from '@/graphql/Memberships';
import { PersonFragment } from '@/graphql/Person';
import { tenantConfig } from '@/tenant/config.js';
import { authState } from '@/graphql/query';

interface AuthContextType {
  isLoading: boolean;
  user: UserAuthFragment | null;
  signIn: (email: string, password: string) => Promise<UserAuthFragment | null>;
  signInWithOtp: (token: string) => Promise<UserAuthFragment | null>;
  signOut: () => void;

  persons: PersonFragment[];
  couples: CoupleFragment[];
  isMember: boolean;
  isTrainer: boolean;
  isAdmin: boolean;
  isTrainerOrAdmin: boolean;
  isLoggedIn: boolean;
  personIds: string[];
}

const authContext = React.createContext<AuthContextType | undefined>(undefined);

export const ProvideAuth = React.memo(function ProvideAuth({ children, onReset }: {
  onReset?: () => void;
  children: React.ReactNode;
}) {
  const [isLoading, setIsLoading] = React.useState<boolean>(true);

  const [{ data: currentUser, fetching }, refetch] = useQuery({ query: CurrentUserDocument, pause: !authState.token });

  React.useEffect(() => {
    if (!fetching) {
      setIsLoading(false);
      if (currentUser?.refreshJwt) {
        authState.token = currentUser?.refreshJwt;
      }
    }
  }, [fetching, currentUser?.refreshJwt]);

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
    authState.token = undefined;
    onReset?.();
    location.href = tenantConfig.enableHome ? '/' : '/dashboard';
  }, [onReset]);

  const context = React.useMemo<AuthContextType>(() => {
    const base64Url = authState.token?.split(".")[1];
    const base64 = base64Url?.replace("-", "+").replace("_", "/");
    const jwt = base64 ? JSON.parse(window.atob(base64)) : {};

    const user = currentUser?.getCurrentUser || null;
    const persons = user?.userProxiesList.flatMap(x => x.person ? [x.person] : []) || [];
    return {
      isLoading,
      signIn,
      signInWithOtp,
      signOut,

      user,
      persons,
      couples: persons.flatMap(x => x.allCouplesList || []),
      personIds: persons.map(x => x.id),

      isLoggedIn: !!user?.id,
      isMember: jwt.is_member,
      isTrainer: jwt.is_trainer,
      isAdmin: jwt.is_admin,
      isTrainerOrAdmin: jwt.is_admin || jwt.is_trainer,
    };
  }, [isLoading, currentUser, signIn, signInWithOtp, signOut])

  return (
    <authContext.Provider value={context}>
      {children}
    </authContext.Provider>
  );
});

export const useAuth = () => {
  const auth = React.useContext(authContext);
  if (auth === undefined) {
    throw new Error('You can only use `useAuth` from inside an AuthProvider');
  }
  return auth;
};

