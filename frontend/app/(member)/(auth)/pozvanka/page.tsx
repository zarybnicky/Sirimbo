import { InvitationInfoDocument } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { getRequestContext } from '@/lib/server/tenant';
import { ErrorPage } from '@/ui/ErrorPage';
import type { Metadata } from 'next';
import Link from 'next/link';
import { redirect } from 'next/navigation';
import { z } from 'zod';
import { InvitationRegistrationForm } from './InvitationRegistrationForm';

export const metadata: Metadata = {
  title: 'Registrace',
};

export default async function InvitationPage({
  searchParams,
}: {
  searchParams: Promise<{ token?: string | string[] }>;
}) {
  const { claims } = await getRequestContext();
  if (claims?.user_id) redirect('/dashboard');

  const search = await searchParams;
  const token = Array.isArray(search.token) ? search.token[0] : search.token;
  const invitation = z.uuid().safeParse(token).success
    ? await executeGraphql(InvitationInfoDocument, { token })
    : null;

  return token && invitation?.invitationInfo && invitation.invitationName ? (
    <InvitationRegistrationForm
      token={token}
      email={invitation.invitationInfo}
      name={invitation.invitationName}
    />
  ) : (
    <ErrorPage
      error="Neplatná pozvánka"
      details={
        <>
          Vaše pozvánka je neplatná nebo již použitá. Pokud jste se již registrovali,{' '}
          <Link href="/login">přihlaste se zde</Link>.
        </>
      }
    />
  );
}
