'use client';

import { LoginForm } from '@/ui/forms/LoginForm';
import { Layout } from '@/ui/Layout';
import { useRedirectLoggedIn } from '@/ui/use-auth';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import { useTenantConfig } from '@/ui/state/auth';
import { useRouter, useSearchParams } from 'next/navigation';
import * as React from 'react';

export default function LoginPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { publicSite } = useTenantConfig();
  useRedirectLoggedIn();

  const onSuccess = React.useCallback(
    (user: UserAuthFragment | null) => {
      const from = searchParams?.get('from') || undefined;
      const defaultRedirect = publicSite ? '/dashboard' : '/rozpis';
      router.push(!user?.userProxiesList.length ? '/profil' : from || defaultRedirect);
    },
    [publicSite, router, searchParams],
  );

  return (
    <Layout className="grow content relative content-stretch">
      <LoginForm onSuccess={onSuccess} />
    </Layout>
  );
}
