import * as React from "react";
import { UserAuthFragment, CouplePartialFragment, useCurrentUserQuery, useLoginMutation, useLogoutMutation } from 'lib/graphql/CurrentUser';
import { useQueryClient } from "@tanstack/react-query";

export interface AuthContextType {
  isLoading: boolean,
  user: UserAuthFragment | null;
  couple: CouplePartialFragment | null;
  signIn: (email: string, password: string) => Promise<void>;
  signOut: () => Promise<void>;
  sendPasswordResetEmail: (email: string) => Promise<void>;
  confirmPasswordReset: (code: string, password: string) => Promise<void>;
}

const authContext = React.createContext<AuthContextType | undefined>(undefined);

export const ProvideAuth: React.FC = ({ children }) => {
  const auth = useApiAuth();
  return <authContext.Provider value={auth}>{children}</authContext.Provider>;
}

export const useAuth = () => {
  const auth = React.useContext(authContext);
  if (auth === undefined) {
    throw new Error('You can only use `useAuth` from inside an AuthProvider');
  }
  return auth;
}

function useApiAuth(): AuthContextType {
  const queryClient = useQueryClient();
  const [isLoading, setIsLoading] = React.useState<boolean>(true);

  const { data: currentUser } = useCurrentUserQuery({}, {
    onSettled: () => setIsLoading(false),
  });

  const user = currentUser?.getCurrentUser || null;
  const couple = currentUser?.getCurrentCouple || null;
  const { mutateAsync: signIn } = useLoginMutation({
    onSuccess: (data) => {
      queryClient.setQueryData(['CurrentUser', {}], {
        getCurrentUser: data.login?.result?.usr,
        getCurrentCouple: data.login?.result?.couple,
      })
    },
  });
  const { mutateAsync: signOut } = useLogoutMutation({
    onSuccess: () => {
      queryClient.resetQueries(['CurrentUser', {}]);
    },
  });

  return {
    isLoading,
    user,
    couple,
    async signIn(login: string, passwd: string) {
      setIsLoading(true);
      await signIn({ login, passwd });
    },
    async signOut() {
      await signOut({});
    },
    async sendPasswordResetEmail(email: string) {
      // await sendPasswordResetEmail(email);
    },
    async confirmPasswordReset(code: string, password: string) {
      // await confirmPasswordReset(code, password)
    }
  };
}
