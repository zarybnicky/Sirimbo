import * as React from "react";
import { $, Selector, InputType, GraphQLTypes } from '../zeus';
import { useTypedMutation, useTypedQuery } from '../zeus/apollo';

type AppUser = InputType<GraphQLTypes["User"], typeof UserPartial>;
type AppCouple = InputType<GraphQLTypes["Pary"], typeof CouplePartial>;

export interface AuthContextType {
  isLoading: boolean,
  user: AppUser | null;
  couple: AppCouple | null;
  signIn: (email: string, password: string) => Promise<AppUser>;
  signOut: () => Promise<void>;
}

const authContext = React.createContext<AuthContextType | undefined>(undefined);

export const ProvideAuth = ({ mock = false, children }: {
  mock?: boolean;
  children: React.ReactChild | React.ReactChild[];
}) => {
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

  const auth = {
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
    async signOut() {
      await signOut();
      setUser(null);
      setCouple(null);
    },
  };
  return <authContext.Provider value={auth}>{children}</authContext.Provider>;
}

export const useAuth = () => {
  const auth = React.useContext(authContext);
  if (auth === undefined) {
    throw new Error('You can only use `useAuth` from inside an AuthProvider');
  }
  return auth;
}

export const CouplePartial = Selector("Pary")({
  pId: true,
  pIdPartner: true,
  pIdPartnerka: true,
  pArchiv: true,
});

export const UserPartial = Selector("User")({
  permissionByUGroup: {
    peAkce: true,
    peAnkety: true,
    peAktuality: true,
    peDescription: true,
    peDokumenty: true,
    peGalerie: true,
    peId: true,
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
