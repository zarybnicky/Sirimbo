'use client';

import { Layout } from '@/ui/Layout';
import { Spinner } from '@/ui/Spinner';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { otpLoginAction } from '@/lib/server/auth-actions';
import { useRouter, useSearchParams } from 'next/navigation';
import { sessionPresentAtom, useTenantConfig } from '@/ui/state/auth';
import * as React from 'react';

export default function OtpPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const { publicSite } = useTenantConfig();
  const setSessionPresent = useSetAtom(sessionPresentAtom);
  const [loading, setLoading] = React.useState(true);
  const [status, setStatus] = React.useState('Načítám...');
  const ranRef = React.useRef(false);

  React.useEffect(() => {
    if (ranRef.current) return;
    ranRef.current = true;

    const token = searchParams?.get('token');
    if (!token) {
      setStatus('Použitý odkaz již vypršel nebo je neplatný.');
      setLoading(false);
      return;
    }

    void (async () => {
      setStatus('Přihlašuji...');
      const result = await otpLoginAction(token);
      if (result.status === 'error') {
        setStatus(result.error);
        setLoading(false);
        return;
      }
      setSessionPresent(true);
      setStatus('Přesměrovávám...');
      const from = searchParams?.get('from') || undefined;
      const defaultRedirect = publicSite ? '/dashboard' : '/rozpis';
      router.push(
        !result.user?.userProxiesList.length ? '/profil' : from || defaultRedirect,
      );
    })();
  }, [searchParams, publicSite, router, setSessionPresent]);

  const personCount = auth.personIds.length;

  React.useEffect(() => {
    if (!authLoading && auth.user) {
      router.replace(personCount === 0 ? '/profil' : '/dashboard');
    }
  }, [authLoading, auth.user, personCount, router]);

  return (
    <Layout className="grow content relative content-stretch">
      <div className="flex h-[calc(100dvh-80px)] items-center justify-center p-5 bg-neutral-1 w-full">
        {loading && <Spinner />}
        {status}
      </div>
    </Layout>
  );
}
