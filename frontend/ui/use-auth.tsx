import * as React from 'react';
import {
  CurrentUserDocument,
  LoginDocument,
  UserAuthFragment,
} from '@app/graphql/CurrentUser';
import { useRouter } from 'next/router';
import { PermissionChecker } from './use-permissions';
import { useMutation, useQuery } from 'urql';
import { TenantBasicFragment } from '@app/graphql/Tenant';
import { CoupleFragment } from '@app/graphql/Memberships';
import { CohortFragment } from '@app/graphql/Cohorts';
import { PersonFragment } from '@app/graphql/Person';
import { tenantConfig } from '@app/tenant/config.js';
import { authState } from '@app/graphql/query';

interface AuthContextType {
  isLoading: boolean;
  user: UserAuthFragment | null;
  persons: PersonFragment[];
  cohorts: CohortFragment[];
  tenants: TenantBasicFragment[];
  couples: CoupleFragment[];
  signIn: (email: string, password: string) => Promise<void>;
  signOut: () => Promise<void>;
  perms: PermissionChecker;
}

const authContext = React.createContext<AuthContextType | undefined>(undefined);

export const ProvideAuth = ({
  children,
  onReset,
}: {
  onReset?: () => void;
  children: React.ReactNode;
}) => {
  const router = useRouter();
  const [isLoading, setIsLoading] = React.useState<boolean>(true);

  const [{ data: currentUser, fetching }, refetch] = useQuery({ query: CurrentUserDocument, pause: !authState.token });
  React.useEffect(() => {
    if (!fetching) {
      setIsLoading(false);
    }
  }, [fetching]);

  React.useEffect(() => {
    const launchQuery = () => {
      if (typeof document === "undefined" ||
        document.visibilityState === undefined ||
        document.visibilityState === "visible"
      ) {
        refetch();
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

  const signOut = React.useCallback(async () => {
    localStorage.removeItem('token');
    authState.token = undefined;
    onReset?.();
    location.href = tenantConfig.enableHome ? '/' : '/dashboard';
  }, [router, onReset]);

  const context = React.useMemo(() => {
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
      perms: new PermissionChecker(
        user?.id || '',
        {
          isTrainer: persons.some(x => x.tenantTrainersList.length > 0),
          isAdministrator: persons.some(x => x.tenantAdministratorsList.length > 0),
          coupleIds: couples.map(x => x.id),
          personIds: persons.map(x => x.id),
          tenantIds: tenants.map(x => x.id),
        },
      ),
    };
  }, [isLoading, currentUser, signIn, signOut])

  return <authContext.Provider value={context}>{children}</authContext.Provider>;
};

const uniq = <T extends {id: string}>(array: T[]): T[] =>
  [...new Map(array.map(item => [item.id, item])).values()]

export const useAuth = () => {
  const auth = React.useContext(authContext);
  if (auth === undefined) {
    throw new Error('You can only use `useAuth` from inside an AuthProvider');
  }
  return auth;
};
