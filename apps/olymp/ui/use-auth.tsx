import * as React from 'react';
import {
  CurrentUserDocument,
  LoginDocument,
  LogoutDocument,
  UserAuthFragment,
} from '@app/graphql/CurrentUser';
import { useRouter } from 'next/router';
import { PermissionChecker } from './use-permissions';
import { useMutation, useQuery } from 'urql';
import { TenantFragment } from '@app/graphql/Tenant';
import { CoupleFragment } from '@app/graphql/Couple';
import { CohortFragment } from '@app/graphql/Cohorts';
import { PersonFragment } from '@app/graphql/Person';

interface AuthContextType {
  isLoading: boolean;
  user: UserAuthFragment | null;
  persons: PersonFragment[];
  cohorts: CohortFragment[];
  tenants: TenantFragment[];
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

  const [{ data: currentUser, fetching }] = useQuery({ query: CurrentUserDocument });
  React.useEffect(() => {
    if (!fetching) {
      setIsLoading(false);
    }
  }, [fetching]);

  const doSignIn = useMutation(LoginDocument)[1];
  const signIn = React.useCallback(
    async (login: string, passwd: string) => {
      setIsLoading(true);
      await doSignIn({ login, passwd });
      setIsLoading(false);
    },
    [doSignIn],
  );

  const doSignOut = useMutation(LogoutDocument)[1];
  const signOut = React.useCallback(async () => {
    await doSignOut({});
    onReset?.();
    await router.push('/');
  }, [router, doSignOut, onReset]);

  const context = React.useMemo(() => {
    const user = currentUser?.getCurrentUser || null;
    const persons = user?.userProxiesList.flatMap(x => x.person ? [x.person] : []) || [];
    const cohorts = persons.flatMap(x => x.cohortMembershipsList.flatMap(x => x.cohort ? [x.cohort] : []));
    const couples = persons.flatMap(x => x.couplesList || []);
    const tenants = persons.flatMap(x => x.tenantMembershipsList.flatMap(x => x.tenant ? [x.tenant] : []));
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

export const useAuth = () => {
  const auth = React.useContext(authContext);
  if (auth === undefined) {
    throw new Error('You can only use `useAuth` from inside an AuthProvider');
  }
  return auth;
};