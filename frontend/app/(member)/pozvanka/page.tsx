/* eslint-disable import-x/no-unused-modules */
import {
  InvitationInfoDocument,
  RegisterUsingInvitationDocument,
  type RegisterUsingInvitationInput,
} from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { redirect } from 'next/navigation';
import { InvitationRegistrationForm } from './InvitationRegistrationForm';

export const metadata: Metadata = {
  title: 'Registrace',
  robots: { index: false, follow: false },
};

export default async function InvitationPage({
  searchParams,
}: {
  searchParams: Promise<{ token?: string | string[] }>;
}) {
  const search = await searchParams;
  const token = Array.isArray(search.token) ? search.token[0] : search.token;
  const invitation = token
    ? await executeGraphql(InvitationInfoDocument, { token }).catch(() => null)
    : null;

  async function register(input: RegisterUsingInvitationInput) {
    'use server';

    const data = await executeGraphql(RegisterUsingInvitationDocument, {
      input,
    }).catch(() => null);
    const jwt = data?.registerUsingInvitation?.result?.jwt;
    if (!jwt) return 'Registraci se nepodařilo dokončit';

    await setSessionCookie(jwt);
    redirect('/dashboard');
  }

  return (
    <Layout
      className="grow content relative content-stretch"
      includeTenantSeo={false}
    >
      <InvitationRegistrationForm
        register={register}
        token={token}
        email={invitation?.invitationInfo ?? undefined}
        name={invitation?.invitationName ?? undefined}
      />
    </Layout>
  );
}
