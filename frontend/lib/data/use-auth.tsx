import * as React from "react";
import { UserAuthFragment, CouplePartialFragment, useCurrentUserQuery, useLoginMutation, useLogoutMutation } from 'lib/graphql';
import posthog from 'posthog-js';

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
  const [user, setUser] = React.useState<UserAuthFragment | null>(null);
  const [couple, setCouple] = React.useState<CouplePartialFragment | null>(null);
  const [isLoading, setIsLoading] = React.useState<boolean>(true);

  useCurrentUserQuery({}, {
    onSuccess: (data) => {
      if (data.getCurrentUser) {
        posthog.identify(data.getCurrentUser.uLogin, data.getCurrentUser);
      }
      setUser(data.getCurrentUser);
    },
    onSettled: (data) => {
      setIsLoading(false)
    },
  });
  const { mutateAsync: signIn } = useLoginMutation({
    onSuccess: (data) => {
      setUser(data.login?.result?.usr!);
      setCouple(data.login?.result?.couple!);
    },
    onSettled: () => setIsLoading(false),
  });
  const { mutateAsync: signOut } = useLogoutMutation({
    onSuccess: () => {
      setUser(null);
      setCouple(null);
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
