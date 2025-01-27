import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { useRouter } from 'next/router';
import * as React from 'react';
import { tenantConfig } from '@/tenant/config';
import { Spinner } from '@/ui/Spinner';
import { useMutation } from 'urql';
import { OtpLoginDocument } from '@/graphql/CurrentUser';
import { LinkProps } from 'next/link';

export default function OtpPage() {
  const router = useRouter<'/otp'>();
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const [loading, setLoading] = React.useState(true);
  const [status, setStatus] = React.useState('Načítám...');
  const doSignInWithOtp = useMutation(OtpLoginDocument)[1];

  React.useEffect(() => {
    (async () => {
      if (router.isReady) {
        setStatus('Přihlašuji...');
        const { data: user } = await doSignInWithOtp({ token: router.query.token });
        if (!user) {
          setStatus('Použitý odkaz již vypršel nebo je neplatný.');
          setLoading(false);
          return;
        }
        setStatus('Přesměrovávám...');
        const redirect = router.query.from;
        const defaultRedirect = tenantConfig.enableArticles ? '/dashboard' : '/rozpis';
        void router.push(!user.otpLogin?.result?.usr?.userProxiesList.length ? '/profil' : (redirect || defaultRedirect) as LinkProps['href']);
      }
    })();
  }, [router, doSignInWithOtp]);

  if (!authLoading && auth.user) {
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
