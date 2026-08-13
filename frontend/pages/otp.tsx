import { useRedirectLoggedIn } from '@/ui/use-auth';
import { Layout } from '@/ui/Layout';
import { NextSeo } from 'next-seo';
import { useRouter } from 'next/router';
import * as React from 'react';
import { Spinner } from '@/ui/Spinner';
import { useMutation } from 'urql';
import { OtpLoginDocument } from '@/graphql/CurrentUser';
import { useTenantConfig } from '@/ui/state/auth';

export default function OtpPage() {
  const router = useRouter();
  useRedirectLoggedIn();
  const { publicSite } = useTenantConfig();
  const [loading, setLoading] = React.useState(true);
  const [status, setStatus] = React.useState('Načítám...');
  const doSignInWithOtp = useMutation(OtpLoginDocument)[1];

  React.useEffect(() => {
    if (!router.isReady) {
      return;
    }

    const token = router.query.token;

    if (!token) {
      setStatus('Použitý odkaz již vypršel nebo je neplatný.');
      setLoading(false);
      return;
    }

    void (async () => {
      setStatus('Přihlašuji...');
      const { data: user } = await doSignInWithOtp({ token });
      if (!user) {
        setStatus('Použitý odkaz již vypršel nebo je neplatný.');
        setLoading(false);
        return;
      }

      setStatus('Přesměrovávám...');
      const redirect = router.query?.from as string | undefined;
      const defaultRedirect = publicSite ? '/dashboard' : '/rozpis';
      void router.push(
        !user.otpLogin?.result?.usr?.userProxiesList.length
          ? '/profil'
          : ((redirect || defaultRedirect) as Parameters<typeof router.push>[0]),
      );
    })();
  }, [
    doSignInWithOtp,
    publicSite,
    router,
    router.isReady,
    router.query.from,
    router.query.token,
  ]);

  return (
    <Layout className="grow content relative content-stretch">
      <NextSeo title="Přihlášení" />
      <div className="flex h-[calc(100dvh-80px)] items-center justify-center p-5 bg-neutral-1 w-full">
        {loading && <Spinner />}
        {status}
      </div>
    </Layout>
  );
}
