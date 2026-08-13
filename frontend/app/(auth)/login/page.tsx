/* eslint-disable import-x/no-unused-modules, unicorn/consistent-function-scoping */
import { LoginDocument, type LoginMutationVariables } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';
import { getRequestTenant } from '@/tenant/server';
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { LoginForm } from './LoginForm';

export const metadata: Metadata = { title: 'Přihlášení' };

export default async function LoginPage({
  searchParams,
}: {
  searchParams: Promise<{ from?: string | string[] }>;
}) {
  const [tenant, search] = await Promise.all([getRequestTenant(), searchParams]);
  const from = Array.isArray(search.from) ? search.from[0] : search.from;

  async function login(values: LoginMutationVariables) {
    'use server';

    try {
      const data = await executeGraphql(LoginDocument, values);
      const result = data.login?.result;
      if (!result?.jwt) return { error: 'Nesprávné jméno nebo heslo' } as const;

      await setSessionCookie(result.jwt);
      return { user: result.usr ?? null };
    } catch {
      return { error: 'Nesprávné jméno nebo heslo' } as const;
    }
  }

  return (
    <Layout className="grow content relative content-stretch">
      <LoginForm
        login={login}
        enableRegistration={tenant.config.enableRegistration}
        from={from}
        defaultRedirect={tenant.config.publicSite ? '/dashboard' : '/rozpis'}
      />
    </Layout>
  );
}
