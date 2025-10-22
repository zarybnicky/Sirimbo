import { LoginForm } from '@/ui/forms/LoginForm';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { Layout } from '@/ui/Layout';
import { NextSeo } from 'next-seo';
import { useRouter } from 'next/router';
import * as React from 'react';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import { LinkProps } from 'next/link';
import { useAtomValue } from 'jotai';
import { tenantConfigAtom } from '@/ui/state/auth';

export default function LoginPage() {
  const router = useRouter();
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const { enableHome } = useAtomValue(tenantConfigAtom);

  const onSuccess = React.useCallback((user: UserAuthFragment | null) => {
    const redirect = router.query?.from as string | undefined;
    const defaultRedirect = enableHome ? '/dashboard' : '/rozpis';
    void router.push(!user?.userProxiesList.length ? '/profil' : (redirect || defaultRedirect) as LinkProps['href']);
  }, [enableHome, router]);

  const personCount = auth.personIds.length;

  React.useEffect(() => {
    if (!router.isReady) {
      return;
    }

    if (!authLoading && auth.user) {
      const destination = personCount === 0 ? '/profil' : '/dashboard';
      void router.replace(destination);
    }
  }, [authLoading, auth.user, personCount, router, router.isReady]);

  return (
    <Layout className="grow content relative content-stretch">
      <NextSeo title="Přihlášení" />
      <LoginForm onSuccess={onSuccess} />
    </Layout>
  );
}
