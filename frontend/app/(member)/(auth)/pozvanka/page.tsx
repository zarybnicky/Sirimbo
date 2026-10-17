import { InvitationInfoDocument } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { getRequestContext } from '@/lib/server/tenant';
import { ErrorPage } from '@/ui/ErrorPage';
import type { Metadata } from 'next';
import Link from 'next/link';
import { z } from 'zod';
import { InvitationAcceptanceForm } from './InvitationAcceptanceForm';
import { InvitationRegistrationForm } from './InvitationRegistrationForm';

export const metadata: Metadata = {
  title: 'Registrace',
};

export default async function InvitationPage({
  searchParams,
}: {
  searchParams: Promise<{ token?: string | string[]; from?: string | string[] }>;
}) {
  const { auth } = await getRequestContext();

  const search = await searchParams;
  const token = Array.isArray(search.token) ? search.token[0] : search.token;
  const from = Array.isArray(search.from) ? search.from[0] : search.from;
  const invitation = z.uuid().safeParse(token).success
    ? await executeGraphql(InvitationInfoDocument, { token })
    : null;

  if (token && invitation?.invitationInfo && invitation.invitationName) {
    return auth.isLoggedIn ? (
      <InvitationAcceptanceForm
        token={token}
        from={from}
        name={invitation.invitationName}
        accountEmail={auth.email ?? ''}
      />
    ) : (
      <InvitationRegistrationForm
        token={token}
        from={from}
        email={invitation.invitationInfo}
        name={invitation.invitationName}
      />
    );
  }

  return (
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
