import * as React from "react";
import { $, Selector, InputType, GraphQLTypes } from '../zeus';
import { useTypedMutation, useTypedQuery } from '../zeus/apollo';

export interface AuthContextType {
  isLoading: boolean,
  user: AppUser | null;
  couple: AppCouple | null;
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
  const [couple, setCouple] = React.useState<AppCouple | null>(null);
  const [isLoading, setIsLoading] = React.useState<boolean>(true);
  const [signIn] = useTypedMutation({
    login: [
      { input: { login: $`login`, passwd: $`passwd` } },
      { result: { usr: UserPartial, couple: CouplePartial } },
    ]
  }, {
    onError: () => setIsLoading(false),
  });
  const [signOut] = useTypedMutation({
    logout: [{ input: {} }, { __typename: true }],
  });
  useTypedQuery({ getCurrentUser: UserPartial }, {
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
    couple,
    async signIn(login: string, passwd: string) {
      setIsLoading(true);
      const { data } = await signIn({ variables: { login, passwd } });
      setIsLoading(false);
      setUser(data?.login?.result?.usr!!);
      setCouple(data?.login?.result?.couple!!);
      return data?.login?.result?.usr!!;
    },
    async signUp(email: string, password: string) {
      // const response = await createUserWithEmailAndPassword(email, password);
      // const response = { user: { name: "Jakub Zárybnický" } };
      setUser(UserMock as any);
      setCouple(CoupleMock as any);
      return UserMock as any;
    },
    async signOut() {
      await signOut();
      setUser(null);
      setCouple(null);
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
  const [couple, setCouple] = React.useState<AppCouple | null>(null);
  const [isLoading] = React.useState<boolean>(false);
  return {
    isLoading,
    user,
    couple,
    async signIn() {
      setUser(UserMock as any);
      setCouple(CoupleMock as any);
      return UserMock as any;
    },
    async signUp() {
      setUser(UserMock as any);
      setCouple(CoupleMock as any);
      return UserMock as any;
    },
    async signOut() {
      setUser(null);
      setCouple(null);
    },
    async sendPasswordResetEmail(email: string) {
    },
    async confirmPasswordReset() {
    },
  };
}

export type AppUser = InputType<GraphQLTypes["User"], typeof UserPartial>;
export type AppCouple = InputType<GraphQLTypes["Pary"], typeof CouplePartial>;

export const CoupleMock: AppCouple = {
  pId: 1,
  pIdPartner: 1,
  pIdPartnerka: 0,
  pArchiv: false,
};

export const CouplePartial = Selector("Pary")({
  pId: true,
  pIdPartner: true,
  pIdPartnerka: true,
  pArchiv: true,
});

export const UserMock: AppUser = {
  permissionByUGroup: {
    peAkce: 1,
    peAnkety: 1,
    peAktuality: 1,
    peDescription: "Guest",
    peDokumenty: 1,
    peGalerie: 1,
    peId: 5,
    peKonzole: 1,
    peInzerce: 1,
    peNabidka: 1,
    peMain: 1,
    peName: "Guest",
    peNastenka: 1,
    peNovinky: 1,
    pePary: 1,
    pePermissions: 1,
    pePlatby: 1,
    peRozpis: 1,
    peSkupiny: 1,
    peUsers: 1,
  },
  uTimestamp: true,
  uSystem: false,
  uTelefon: "734408237",
  uTeacher: true,
  uStreet: "Street",
  uRodneCislo: "9999990000",
  uSkupina: 5,
  uPrijmeni: "Surname",
  uPoznamky: "...",
  uPostalCode: "77777",
  uPohlavi: "m",
  uOrientationNumber: "4",
  uNationality: "CZ",
  uNarozeni: "2010-02-01",
  uMemberUntil: null,
  uLogin: "guest",
  uMemberSince: "2010-02-01T12:12:12Z",
  uLock: true,
  uLevel: 5,
  uJmeno: "Guest",
  uGroup: "5",
  uId: "5",
  uGdprSignedAt: "2010-02-01T12:12:12Z",
  uEmail: "a@a.com",
  uDancer: true,
  uDistrict: "District",
  uCreatedAt: "2010-02-01T12:12:12Z",
  uConfirmed: true,
  uConscriptionNumber: "4",
  uBan: false,
  uCity: "City",
};

export const UserPartial = Selector("User")({
  permissionByUGroup: {
    peAkce: true,
    peAnkety: true,
    peAktuality: true,
    peDescription: true,
    peDokumenty: true,
    peGalerie: true,
    peId: true,
    peKonzole: true,
    peInzerce: true,
    peNabidka: true,
    peMain: true,
    peName: true,
    peNastenka: true,
    peNovinky: true,
    pePary: true,
    pePermissions: true,
    pePlatby: true,
    peRozpis: true,
    peSkupiny: true,
    peUsers: true,
  },
  uTimestamp: true,
  uSystem: true,
  uTelefon: true,
  uTeacher: true,
  uStreet: true,
  uRodneCislo: true,
  uSkupina: true,
  uPrijmeni: true,
  uPoznamky: true,
  uPostalCode: true,
  uPohlavi: true,
  uOrientationNumber: true,
  uNationality: true,
  uNarozeni: true,
  uMemberUntil: true,
  uLogin: true,
  uMemberSince: true,
  uLock: true,
  uLevel: true,
  uJmeno: true,
  uGroup: true,
  uId: true,
  uGdprSignedAt: true,
  uEmail: true,
  uDancer: true,
  uDistrict: true,
  uCreatedAt: true,
  uConfirmed: true,
  uConscriptionNumber: true,
  uBan: true,
  uCity: true,
});

export const UserQuery = Selector("Query")({
  getCurrentUser: UserPartial,
});
