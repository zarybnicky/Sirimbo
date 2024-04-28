import { useAuth } from '@/ui/use-auth';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { useRouter } from 'next/router';
import * as React from 'react';
import { tenantConfig } from '@/tenant/config';
import { Spinner } from '@/ui/Spinner';

function OtpPage() {
  const router = useRouter();
  const auth = useAuth();
  const [loading, setLoading] = React.useState(true);
  const [status, setStatus] = React.useState('Načítám...');

  React.useEffect(() => {
    (async () => {
      if (router.isReady) {
        setStatus('Přihlašuji...');
        const user = await auth.signInWithOtp(router.query.token as string);
        if (!user) {
          setStatus('Použitý odkaz již vypršel nebo je neplatný.');
          setLoading(false);
          return;
        }
        setStatus('Přesměrovávám...');
        const redirect = router.query?.from as string | undefined;
        const defaultRedirect = tenantConfig.enableArticles ? '/dashboard' : '/rozpis';
        void router.push(!user?.userProxiesList.length ? '/profil' : (redirect || defaultRedirect));
      }
    })();
  }, [router, auth]);

  if (!auth.isLoading && auth.user) {
    void router.replace(!auth.personIds.length ? '/profil' : '/dashboard');
  }

  return (
    <Layout className="grow content relative content-stretch">
      <NextSeo title="Přihlášení" />
      <div className="flex h-[calc(100vh-80px)] items-center justify-center p-5 bg-neutral-1 w-full">
        {loading && <Spinner />}
        {status}
      </div>
    </Layout>
  );
}

export default OtpPage;
