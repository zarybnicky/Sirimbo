import { LoginForm } from '@app/ui/LoginForm';
import { useAuth } from '@app/ui/use-auth';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { useRouter } from 'next/router';
import * as React from 'react';
import { tenantConfig } from '@/tenant/config';

const Page = () => {
  const router = useRouter();
  const { user, isLoading } = useAuth();
  const onSuccess = React.useCallback(() => {
    const redirect = router.query?.from as string | undefined;
    const defaultRedirect = tenantConfig.enableArticles ? '/dashboard' : '/rozpis';
    void router.push(redirect || defaultRedirect);
  }, [router])

  if (!isLoading && user) {
    void router.replace('/dashboard');
  }

  return (
    <Layout className="grow content relative content-stretch">
      <NextSeo title="Přihlášení" />
      <LoginForm onSuccess={onSuccess} />
    </Layout>
  );
}

export default Page;
