import * as React from "react";
import { $, InputType, GraphQLTypes } from './zeus';
import { useTypedMutation, useTypedQuery } from './zeus/apollo';
import { AppUser, UserPartial, UserMock } from "./client";

export interface AuthContextType {
  isLoading: boolean,
  user: AppUser | null;
  signIn: (email: string, password: string) => Promise<AppUser>;
  signUp: (email: string, password: string) => Promise<AppUser>;
  signOut: () => Promise<void>;
  sendPasswordResetEmail: (email: string) => Promise<void>;
  confirmPasswordReset: (code: string, password: string) => Promise<void>;
}

const authContext = React.createContext<AuthContextType | undefined>(undefined);

export const ProvideAuth = ({ mock = false, children }: {
  mock?: boolean;
  children: React.ReactChild | React.ReactChild[];
}) => {
  const auth = mock ? useMockAuth() : useApiAuth();
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
  const [user, setUser] = React.useState<AppUser | null>(null);
  const [isLoading, setIsLoading] = React.useState<boolean>(true);
  const [signIn] = useTypedMutation({
    login: [
      { input: { login: $`login`, passwd: $`passwd` } },
      { result: { usr: UserPartial } },
    ]
  });
  const [signOut] = useTypedMutation({
    logout: [{ input: {} }, { __typename: true }],
  });
  useTypedQuery({
    getCurrentUser: UserPartial,
  }, {
    onCompleted: (data) => {
      setUser(data.getCurrentUser as AppUser);
      setIsLoading(false);
    },
    onError: () => setIsLoading(false),
  });
  /* useEffect(() => {
   *   const unsubscribe = firebase.auth().onAuthStateChanged(setUser);
   *   return () => unsubscribe();
   * }, []); */

  return {
    isLoading,
    user,
    async signIn(login: string, passwd: string) {
      const { data } = await signIn({ variables: { login, passwd } });
      setUser(data?.login?.result?.usr!!);
      return data?.login?.result?.usr!!;
    },
    async signUp(email: string, password: string) {
      // const response = await createUserWithEmailAndPassword(email, password);
      // const response = { user: { name: "Jakub Zárybnický" } };
      setUser(UserMock as any);
      return UserMock as any;
    },
    async signOut() {
      await signOut();
      setUser(null);
    },
    async sendPasswordResetEmail(email: string) {
      // await sendPasswordResetEmail(email);
    },
    async confirmPasswordReset(code: string, password: string) {
      // await confirmPasswordReset(code, password)
    }
  };
}

export function useMockAuth(): AuthContextType {
  const [user, setUser] = React.useState<AppUser | null>(null);
  const [isLoading, setIsLoading] = React.useState<boolean>(false);
  return {
    isLoading,
    user,
    async signIn() {
      setUser(UserMock as any);
      return UserMock as any;
    },
    async signUp() {
      setUser(UserMock as any);
      return UserMock as any;
    },
    async signOut() {
      setUser(null);
    },
    async sendPasswordResetEmail(email: string) {
    },
    async confirmPasswordReset() {
    },
  };
}
