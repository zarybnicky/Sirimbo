import * as React from 'react';
import {
  CurrentUserDocument,
  LoginDocument,
  UserAuthFragment,
} from '@app/graphql/CurrentUser';
import { useMutation, useQuery } from 'urql';
import { CoupleFragment } from '@app/graphql/Memberships';
import { CohortBasicFragment } from '@app/graphql/Cohorts';
import { PersonFragment } from '@app/graphql/Person';
import { tenantConfig } from '@app/tenant/config.js';
import { authState } from '@app/graphql/query';

interface AuthContextType {
  isLoading: boolean;
  user: UserAuthFragment | null;
  persons: PersonFragment[];
  cohorts: CohortBasicFragment[];
  couples: CoupleFragment[];
  signIn: (email: string, password: string) => Promise<void>;
  signOut: () => void;
  perms: {
    isMember: boolean;
    isTrainer: boolean;
    isAdmin: boolean;
    isTrainerOrAdmin: boolean;
    isLoggedIn: boolean;
    isCurrentPerson: (id: string | null | undefined) => boolean;
    isCurrentCouple: (id: string | null | undefined) => boolean;
  };
}

const authContext = React.createContext<AuthContextType | undefined>(undefined);

export const ProvideAuth = React.memo(function ProvideAuth({ children, onReset }: {
  onReset?: () => void;
  children: React.ReactNode;
}) {
  const [isLoading, setIsLoading] = React.useState<boolean>(true);

  const [{ data: currentUser, fetching }, refetch] = useQuery({ query: CurrentUserDocument, pause: !authState.token });
  React.useEffect(() => {
    if (!fetching) {
      setIsLoading(false);
      if (currentUser?.refreshJwt) {
        authState.token = currentUser?.refreshJwt;
      }
    }
  }, [fetching]);

  React.useEffect(() => {
    const launchQuery = () => {
      if (typeof document === "undefined" ||
        document.visibilityState === undefined ||
        document.visibilityState === "visible"
      ) {
        refetch({ requestPolicy: 'network-only' });
      }
    };
    const interval = setInterval(launchQuery, 30000);
    return () => clearInterval(interval);
  }, [refetch]);

  const doSignIn = useMutation(LoginDocument)[1];
  const signIn = React.useCallback(
    async (login: string, passwd: string) => {
      setIsLoading(true);
      await doSignIn({ login, passwd });
      setIsLoading(false);
    },
    [doSignIn],
  );

  const signOut = React.useCallback(() => {
    localStorage.removeItem('token');
    authState.token = undefined;
    onReset?.();
    location.href = tenantConfig.enableHome ? '/' : '/dashboard';
  }, [onReset]);

  const context = React.useMemo(() => {
    const base64Url = authState.token?.split(".")[1];
    const base64 = base64Url?.replace("-", "+").replace("_", "/");
    const jwt = base64 ? JSON.parse(window.atob(base64)) : {};

    const user = currentUser?.getCurrentUser || null;
    const persons = user?.userProxiesList.flatMap(x => x.person ? [x.person] : []) || [];
    const cohorts = persons.flatMap(x => x.cohortMembershipsList.flatMap(x => x.cohort ? [x.cohort] : []));
    const couples = persons.flatMap(x => x.allCouplesList || []);
    const tenants = uniq(persons.flatMap(x =>
      x.tenantMembershipsList.flatMap(x => x.tenant ? [x.tenant] : [])
       .concat(x.tenantAdministratorsList.flatMap(x => x.tenant ? [x.tenant] : []))
       .concat(x.tenantMembershipsList.flatMap(x => x.tenant ? [x.tenant] : []))
    ));
    return {
      isLoading,
      user,
      signIn,
      signOut,
      persons,
      cohorts,
      couples,
      tenants,
      perms: {
        isLoggedIn: !!user?.id,
        isMember: jwt.is_member,
        isTrainer: jwt.is_trainer,
        isAdmin: jwt.is_admin,
        isTrainerOrAdmin: jwt.is_admin || jwt.is_trainer,
        coupleIds: couples.map(x => x.id),
        personIds: persons.map(x => x.id),
        tenantIds: tenants.map(x => x.id),
        isCurrentPerson(id: string | null | undefined) {
          return !!id && persons.some(x => x.id === id);
        },
        isCurrentCouple(id: string | null | undefined) {
          return !!id && couples.some(x => x.id === id);
        },
      },
    };
  }, [isLoading, currentUser, signIn, signOut])

  return <authContext.Provider value={context}>{children}</authContext.Provider>;
});

const uniq = <T extends {id: string}>(array: T[]): T[] =>
  [...new Map(array.map(item => [item.id, item])).values()]

export const useAuth = () => {
  const auth = React.useContext(authContext);
  if (auth === undefined) {
    throw new Error('You can only use `useAuth` from inside an AuthProvider');
  }
  return auth;
};
