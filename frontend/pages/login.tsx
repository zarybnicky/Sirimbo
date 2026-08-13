import { LoginForm } from '@/ui/forms/LoginForm';
import { useRedirectLoggedIn } from '@/ui/use-auth';
import { Layout } from '@/ui/Layout';
import { NextSeo } from 'next-seo';
import { useRouter } from 'next/router';
import * as React from 'react';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import { useTenantConfig } from '@/ui/state/auth';

export default function LoginPage() {
  const router = useRouter();
  useRedirectLoggedIn();
  const { publicSite } = useTenantConfig();

  const onSuccess = React.useCallback(
    (user: UserAuthFragment | null) => {
      const redirect = router.query?.from as string | undefined;
      const defaultRedirect = publicSite ? '/dashboard' : '/rozpis';
      void router.push(
        !user?.userProxiesList.length
          ? '/profil'
          : ((redirect || defaultRedirect) as Parameters<typeof router.push>[0]),
      );
    },
    [publicSite, router],
  );

  return (
    <Layout className="grow content relative content-stretch">
      <NextSeo title="Přihlášení" />
      <LoginForm onSuccess={onSuccess} />
    </Layout>
  );
}
