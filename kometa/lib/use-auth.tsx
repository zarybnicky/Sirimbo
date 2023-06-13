import * as React from 'react';
import {
  CouplePartialFragment,
  CurrentUserDocument,
  LoginDocument,
  LogoutDocument,
  UserAuthFragment,
} from '@app/graphql/CurrentUser';
import { useRouter } from 'next/router';
import { useMutation, useQuery } from 'urql';

interface AuthContextType {
  isLoading: boolean;
  user: UserAuthFragment | null;
  couple: CouplePartialFragment | null;
  signIn: (email: string, password: string) => Promise<void>;
  signOut: () => Promise<void>;
}

const authContext = React.createContext<AuthContextType | undefined>(undefined);

export const ProvideAuth = ({ children }: React.PropsWithChildren) => {
  const router = useRouter();
  const [isLoading, setIsLoading] = React.useState<boolean>(true);

  const [{ data: currentUser, fetching }] = useQuery({query: CurrentUserDocument});
  React.useEffect(() => {
    if (!fetching) {
      setIsLoading(false);
    }
  }, [fetching]);

  const user = currentUser?.getCurrentUser || null;
  const couple = currentUser?.getCurrentCouple || null;
  const doSignIn = useMutation(LoginDocument)[1];
  const doSignOut = useMutation(LogoutDocument)[1];

  const signIn = React.useCallback(
    async (login: string, passwd: string) => {
      setIsLoading(true);
      await doSignIn({ login, passwd });
      setIsLoading(false);
    },
    [doSignIn],
  );
  const signOut = React.useCallback(async () => {
    router.push('/');
    await doSignOut({});
  }, [router, doSignOut]);

  const context = { isLoading, user, couple, signIn, signOut };
  return <authContext.Provider value={context}>{children}</authContext.Provider>;
};

export const useAuth = () => {
  const auth = React.useContext(authContext);
  if (auth === undefined) {
    throw new Error('You can only use `useAuth` from inside an AuthProvider');
  }
  return auth;
};
