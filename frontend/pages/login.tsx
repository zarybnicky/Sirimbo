import { LoginForm } from '@/ui/forms/LoginForm';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { useRouter } from 'next/router';
import * as React from 'react';
import { useTenant } from '@/tenant/runtime';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import { LinkProps } from 'next/link';

export default function LoginPage() {
  const router = useRouter();
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const tenant = useTenant();

  const onSuccess = React.useCallback((user: UserAuthFragment | null) => {
    const redirect = router.query?.from as string | undefined;
    const defaultRedirect = tenant.features.articles ? '/dashboard' : '/rozpis';
    void router.push(!user?.userProxiesList.length ? '/profil' : (redirect || defaultRedirect) as LinkProps['href']);
  }, [router, tenant.features.articles]);

  if (!authLoading && auth.user) {
    void router.replace(!auth.personIds.length ? '/profil' :'/dashboard');
  }

  return (
    <Layout className="grow content relative content-stretch">
      <NextSeo title="Přihlášení" />
      <LoginForm onSuccess={onSuccess} />
    </Layout>
  );
}
