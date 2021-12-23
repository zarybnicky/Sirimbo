import * as React from "react";
import { useMutation, gql } from "@apollo/client";
import { AkceItemsConnection, AkceItemsEdge, PageInfo, User } from "./graphql/graphql";
export type { User };

const SIGN_IN = gql(`
mutation SignIn($login: String!, $passwd: String!) {
  login(input: {login: $login, passwd: $passwd}) {
    query {
      currentUserId
    }
  }
}`);

export interface AuthContextType {
  user: User | null;
  signIn: (email: string, password: string) => Promise<User>;
  signUp: (email: string, password: string) => Promise<User>;
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

const mockPageInfo: PageInfo = { hasNextPage: false, hasPreviousPage: false };
const mockConnection = { edges: [], nodes: [], pageInfo: mockPageInfo, totalCount: 0 };
const mockUser: User = {
  akceItemsByAiUser: mockConnection,
  aktualitiesByAtKdo: mockConnection,
  dokumentiesByDKdo: mockConnection,
  galerieFotosByGfKdo: mockConnection,
  nabidkasByNTrener: mockConnection,
  pariesByPIdPartner: mockConnection,
  paryNavrhsByPnNavrhl: mockConnection,
  paryNavrhsByPnPartner: mockConnection,
  paryNavrhsByPnPartnerka: mockConnection,

  nodeId: 'u123',
  uJmeno: 'Jakub',
  uPrijmeni: 'Zárybnický',
};
export function useMockAuth(): AuthContextType {
  const [user, setUser] = React.useState<User | null>(null);
  return {
    user,
    async signIn() {
      setUser(mockUser);
      return mockUser;
    },
    async signUp() {
      setUser(mockUser);
      return mockUser;
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

function useApiAuth(): AuthContextType {
  const [user, setUser] = React.useState<User | null>(null);
  const [signIn, { }] = useMutation(SIGN_IN);

  /* useEffect(() => {
   *   const unsubscribe = firebase.auth().onAuthStateChanged(setUser);
   *   return () => unsubscribe();
   * }, []); */

  return {
    user,
    async signIn(login: string, passwd: string) {
      const response = await signIn({ variables: { login, passwd: passwd } });
      setUser((response as any).user);
      return (response as any).user;
    },
    async signUp(email: string, password: string) {
      // const response = await createUserWithEmailAndPassword(email, password);
      // const response = { user: { name: "Jakub Zárybnický" } };
      setUser(mockUser); // response.user);
      return mockUser; // response.user;
    },
    async signOut() {
      // await signOut()
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
