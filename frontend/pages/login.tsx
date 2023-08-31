import { LoginForm } from '@app/ui/LoginForm';
import { useAuth } from '@app/ui/use-auth';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { useRouter } from 'next/router';
import * as React from 'react';

const Page = () => {
  const router = useRouter();
  const { user, isLoading } = useAuth();
  const onSuccess = React.useCallback(() => {
    const redirect = router.query?.from as string | undefined;
    void router.push(redirect || '/dashboard');
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
