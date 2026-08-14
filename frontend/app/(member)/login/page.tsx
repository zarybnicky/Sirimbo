/* eslint-disable import-x/no-unused-modules */
import { LoginDocument, type LoginMutationVariables } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';
import { getRequestTenant } from '@/tenant/server';
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { redirect } from 'next/navigation';
import { LoginForm } from './LoginForm';

export const metadata: Metadata = {
  title: 'Přihlášení',
  robots: { index: false, follow: false },
};

export default async function LoginPage({
  searchParams,
}: {
  searchParams: Promise<{ from?: string | string[] }>;
}) {
  const [tenant, search] = await Promise.all([getRequestTenant(), searchParams]);
  const from = Array.isArray(search.from) ? search.from[0] : search.from;
  const destination = from || (tenant.config.publicSite ? '/dashboard' : '/rozpis');

  async function login(values: LoginMutationVariables) {
    'use server';

    const data = await executeGraphql(LoginDocument, values).catch(() => null);
    const result = data?.login?.result;
    if (!result?.jwt) return 'Nesprávné jméno nebo heslo';

    await setSessionCookie(result.jwt);
    redirect(!result.usr?.userProxiesList.length ? '/profil' : destination);
  }

  return (
    <Layout
      className="grow content relative content-stretch"
      includeTenantSeo={false}
    >
      <LoginForm
        login={login}
        enableRegistration={tenant.config.enableRegistration}
      />
    </Layout>
  );
}
