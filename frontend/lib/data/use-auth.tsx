import * as React from "react";
import { $, Selector, InputType, GraphQLTypes } from 'lib/zeus';
import { useTypedMutation, useTypedQuery } from "lib/query";

export interface AuthContextType {
  isLoading: boolean,
  user: AppUser | null;
  couple: AppCouple | null;
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
  const [user, setUser] = React.useState<AppUser | null>(null);
  const [couple, setCouple] = React.useState<AppCouple | null>(null);
  const [isLoading, setIsLoading] = React.useState<boolean>(true);

  const { mutateAsync: signIn } = useTypedMutation(['login'], {
    login: [
      { input: { login: $('login', 'String!'), passwd: $('passwd', 'String!') } },
      { result: { usr: UserPartial, couple: CouplePartial } },
    ]
  }, {
    onSuccess: (data) => {
      setUser(data?.login?.result?.usr!!);
      setCouple(data?.login?.result?.couple!!);
    },
    onSettled: () => setIsLoading(false),
  });

  const { mutateAsync: signOut } = useTypedMutation(['logout'], {
    logout: [{ input: {} }, { __typename: true }],
  }, {
    onSuccess: () => {
      setUser(null);
      setCouple(null);
    },
  });

  useTypedQuery(['currentUser'], { getCurrentUser: UserPartial }, {
    onSuccess: (data) => setUser(data.getCurrentUser as AppUser),
    onSettled: () => setIsLoading(false),
  });

  return {
    isLoading,
    user,
    couple,
    async signIn(login: string, passwd: string) {
      setIsLoading(true);
      await signIn({ variables: { login, passwd } });
    },
    async signOut() {
      await signOut()
    },
    async sendPasswordResetEmail(email: string) {
      // await sendPasswordResetEmail(email);
    },
    async confirmPasswordReset(code: string, password: string) {
      // await confirmPasswordReset(code, password)
    }
  };
}

export type AppUser = InputType<GraphQLTypes["User"], typeof UserPartial>;
export type AppCouple = InputType<GraphQLTypes["Pary"], typeof CouplePartial>;

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
