import { LoginForm } from '@/ui/forms/LoginForm';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { Layout } from '@/ui/Layout';
import { NextSeo } from 'next-seo';
import { useRouter } from 'next/router';
import * as React from 'react';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import { useTenant } from '@/ui/state/auth';

export default function LoginPage() {
  const router = useRouter();
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const { publicSite } = useTenant();

  const onSuccess = React.useCallback(
    (user: UserAuthFragment | null) => {
      const redirect = router.query?.from as string | undefined;
      const defaultRedirect = publicSite ? '/dashboard' : '/rozpis';
      void router.push(
        !user?.userProxiesList.length
          ? '/profil'
          : ((redirect || defaultRedirect) as Parameters<typeof router.push>[0]),
      );
    },
    [publicSite, router],
  );

  const personCount = auth.personIds.length;

  React.useEffect(() => {
    if (!router.isReady) {
      return;
    }

    if (!authLoading && auth.user) {
      void router.replace(personCount === 0 ? '/profil' : '/dashboard');
    }
  }, [authLoading, auth.user, personCount, router, router.isReady]);

  return (
    <Layout className="grow content relative content-stretch">
      <NextSeo title="Přihlášení" />
      <LoginForm onSuccess={onSuccess} />
    </Layout>
  );
}
