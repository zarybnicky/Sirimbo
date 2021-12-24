import * as React from "react";
import { useMutation, gql, useQuery } from "@apollo/client";
import { User, signInMutationMock, SignInMutation } from "./graphql/graphql";
import { UserQuery } from "./client";

const SIGN_OUT = gql(`
mutation SignOut {
  logout(input: {}) {
    __typename
  }
}`);

const SIGN_IN = gql(`
mutation SignIn($login: String!, $passwd: String!) {
  login(input: {login: $login, passwd: $passwd}) {
    result {
      usr {
        permissionByUGroup {
          peAkce
          peAnkety
          peAktuality
          peDescription
          peDokumenty
          peGalerie
          peId
          peKonzole
          peInzerce
          peNabidka
          peMain
          peName
          peNastenka
          peNovinky
          pePary
          pePermissions
          pePlatby
          peRozpis
          peSkupiny
          peUsers
        }
        uTimestamp
        uSystem
        uTelefon
        uTeacher
        uStreet
        uRodneCislo
        uSkupina
        uPrijmeni
        uPoznamky
        uPostalCode
        uPohlavi
        uPass
        uOrientationNumber
        uNationality
        uNarozeni
        uMemberUntil
        uLogin
        uMemberSince
        uLock
        uLevel
        uJmeno
        uGroup
        uId
        uGdprSignedAt
        uEmail
        uDancer
        uDistrict
        uCreatedAt
        uConfirmed
        uConscriptionNumber
        uBan
        uCity
      }
    }
  }
}`);

export type AppUser = NonNullable<NonNullable<NonNullable<NonNullable<SignInMutation>['login']>['result']>['usr']>;
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
  const [signIn] = useMutation(SIGN_IN);
  const [signOut] = useMutation(SIGN_OUT);
  useQuery(UserQuery, {
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
      setUser(signInMutationMock as any);
      return signInMutationMock as any;
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
  const [user, setUser] = React.useState<User | null>(null);
  const [isLoading, setIsLoading] = React.useState<boolean>(false);
  return {
    isLoading,
    user,
    async signIn() {
      setUser(signInMutationMock as any);
      return signInMutationMock as any;
    },
    async signUp() {
      setUser(signInMutationMock as any);
      return signInMutationMock as any;
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
