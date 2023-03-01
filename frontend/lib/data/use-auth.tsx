import * as React from "react";
import { CouplePartialFragment, UserAuthFragment, useCurrentUserQuery, useLoginMutation, useLogoutMutation } from 'lib/graphql/CurrentUser';
import { useQueryClient } from "@tanstack/react-query";
import { useRouter } from "next/router";

interface AuthContextType {
  isLoading: boolean,
  user: UserAuthFragment | null;
  couple: CouplePartialFragment | null;
  signIn: (email: string, password: string) => Promise<void>;
  signOut: () => Promise<void>;
  // sendPasswordResetEmail: (email: string) => Promise<void>;
  // confirmPasswordReset: (code: string, password: string) => Promise<void>;
}

const authContext = React.createContext<AuthContextType | undefined>(undefined);

export const ProvideAuth: React.FC = ({ children }) => {
  const router = useRouter();
  const queryClient = useQueryClient();
  const [isLoading, setIsLoading] = React.useState<boolean>(true);

  const { data: currentUser } = useCurrentUserQuery({}, {
    onSettled: () => setIsLoading(false),
  });

  const user = currentUser?.getCurrentUser || null;
  const couple = currentUser?.getCurrentCouple || null;
  const { mutateAsync: doSignIn } = useLoginMutation({
    onSuccess: (data) => {
      queryClient.setQueryData(['CurrentUser', {}], {
        getCurrentUser: data.login?.result?.usr,
        getCurrentCouple: data.login?.result?.couple,
      })
    },
  });
  const { mutateAsync: doSignOut } = useLogoutMutation({
    onSuccess: () => {
      queryClient.resetQueries(['CurrentUser', {}]);
    },
  });

  const signIn = React.useCallback(async (login: string, passwd: string) => {
    setIsLoading(true);
    await doSignIn({ login, passwd });
  }, [doSignIn]);
  const signOut = React.useCallback(async () => {
    await doSignOut({});
    router.push('/');
  }, [doSignOut]);

  const context = { isLoading, user, couple, signIn, signOut };
  return <authContext.Provider value={context}>{children}</authContext.Provider>;
}

export const useAuth = () => {
  const auth = React.useContext(authContext);
  if (auth === undefined) {
    throw new Error('You can only use `useAuth` from inside an AuthProvider');
  }
  return auth;
}
