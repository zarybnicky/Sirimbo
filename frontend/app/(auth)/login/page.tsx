'use client';

import { LoginForm } from '@/ui/forms/LoginForm';
import { Layout } from '@/ui/Layout';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import { useTenantConfig } from '@/ui/state/auth';
import { useRouter, useSearchParams } from 'next/navigation';
import * as React from 'react';

export default function LoginPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const { enableHome } = useTenantConfig();

  const onSuccess = React.useCallback(
    (user: UserAuthFragment | null) => {
      const from = searchParams?.get('from') || undefined;
      const defaultRedirect = enableHome ? '/dashboard' : '/rozpis';
       router.push(!user?.userProxiesList.length ? '/profil' : from || defaultRedirect);
    },
    [enableHome, router, searchParams],
  );

  const personCount = auth.personIds.length;

  React.useEffect(() => {
    if (!authLoading && auth.user) {
      router.replace(personCount === 0 ? '/profil' : '/dashboard');
    }
  }, [authLoading, auth.user, personCount, router]);

  return (
    <Layout className="grow content relative content-stretch">
      <LoginForm onSuccess={onSuccess} />
    </Layout>
  );
}
