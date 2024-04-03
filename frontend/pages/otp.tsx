import { useAuth } from '@/ui/use-auth';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { useRouter } from 'next/router';
import * as React from 'react';
import { tenantConfig } from '@/tenant/config';
import { Spinner } from '@/ui/Spinner';

function OtpPage() {
  const router = useRouter();
  const { signInWithOtp, user, isLoading } = useAuth();
  const [status, setStatus] = React.useState('Načítám...');

  React.useEffect(() => {
    (async () => {
      if (router.isReady) {
        setStatus('Přihlašuji...');
        const user = await signInWithOtp(router.query.token as string);
        setStatus('Přesměrovávám...');
        const redirect = router.query?.from as string | undefined;
        const defaultRedirect = tenantConfig.enableArticles ? '/dashboard' : '/rozpis';
        void router.push(!user?.userProxiesList.length ? '/profil' : (redirect || defaultRedirect));
      }
    })();
  }, [router, signInWithOtp]);

  if (!isLoading && user) {
    void router.replace(!user.userProxiesList.length ? '/profil' :'/dashboard');
  }

  return (
    <Layout className="grow content relative content-stretch">
      <NextSeo title="Přihlášení" />
      <Spinner />
      {status}
    </Layout>
  );
}

export default OtpPage;
