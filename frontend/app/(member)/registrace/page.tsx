/* eslint-disable import-x/no-unused-modules, unicorn/consistent-function-scoping */
import {
  RegisterWithoutInvitationDocument,
  type RegisterWithoutInvitationInput,
} from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';
import { getRequestTenant } from '@/tenant/server';
import { ErrorPage } from '@/ui/ErrorPage';
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { redirect } from 'next/navigation';
import { RegistrationForm } from './RegistrationForm';

export const metadata: Metadata = {
  title: 'Přihláška nového člena',
  robots: { index: false, follow: false },
};

export default async function RegisterPage() {
  const tenant = await getRequestTenant();

  async function register(input: RegisterWithoutInvitationInput) {
    'use server';

    const data = await executeGraphql(RegisterWithoutInvitationDocument, {
      input,
    }).catch(() => null);
    const jwt = data?.registerWithoutInvitation?.result?.jwt;
    if (!jwt) return 'Registraci se nepodařilo dokončit';

    await setSessionCookie(jwt);
    redirect('/dashboard');
  }

  return (
    <Layout
      className="grow content relative content-stretch"
      includeTenantSeo={false}
    >
      {tenant.config.enableRegistration ? (
        <RegistrationForm register={register} />
      ) : (
        <ErrorPage
          error="Registrace je uzavřena"
          details="Nové registrace aktuálně nepřijímáme."
        />
      )}
    </Layout>
  );
}
