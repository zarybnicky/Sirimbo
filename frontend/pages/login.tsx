import { LoginForm } from '@/ui/forms/LoginForm';
import { useAuth } from '@/ui/use-auth';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { useRouter } from 'next/router';
import * as React from 'react';
import { tenantConfig } from '@/tenant/config';
import { UserAuthFragment } from '@/graphql/CurrentUser';

const Page = () => {
  const router = useRouter();
  const auth = useAuth();

  const onSuccess = React.useCallback((user: UserAuthFragment | null) => {
    const redirect = router.query?.from as string | undefined;
    const defaultRedirect = tenantConfig.enableArticles ? '/dashboard' : '/rozpis';
    void router.push(!user?.userProxiesList.length ? '/profil' : (redirect || defaultRedirect));
  }, [router]);

  if (!auth.isLoading && auth.user) {
    void router.replace(!auth.personIds.length ? '/profil' :'/dashboard');
  }

  return (
    <Layout className="grow content relative content-stretch">
      <NextSeo title="Přihlášení" />
      <LoginForm onSuccess={onSuccess} />
    </Layout>
  );
}

export default Page;
