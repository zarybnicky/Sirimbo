/* eslint-disable import-x/no-unused-modules */
import { InvitationInfoDocument } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { ErrorPage } from '@/ui/ErrorPage';
import type { Metadata } from 'next';
import Link from 'next/link';
import { InvitationRegistrationForm } from './InvitationRegistrationForm';

export const metadata: Metadata = {
  title: 'Registrace',
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
          Vaše pozvánka je neplatná nebo již použitá.{' '}
          Pokud jste se již registrovali,{' '}
          <Link href="/login">přihlaste se zde</Link>.
        </>
      }
    />
  );
}
