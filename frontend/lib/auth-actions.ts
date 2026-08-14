'use server';

import { RegisterUsingInvitationInput, RegisterWithoutInvitationInput } from '@/graphql';
import { LoginDocument, RegisterUsingInvitationDocument, RegisterWithoutInvitationDocument, type LoginMutationVariables } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';
import { getRequestTenant } from '@/tenant/server';
import { redirect } from 'next/navigation';

export async function loginAction(values: LoginMutationVariables, from?: string | null) {
  const data = await executeGraphql(LoginDocument, values).catch(() => null);
  const result = data?.login?.result;
  if (!result?.jwt) return 'Nesprávné jméno nebo heslo';

  await setSessionCookie(result.jwt);

  const tenant = await getRequestTenant();
  redirect(
    !result.usr?.userProxiesList.length
      ? '/profil'
      : from || (tenant.config.publicSite ? '/dashboard' : '/rozpis'),
  );
}

export async function registerAction(input: RegisterWithoutInvitationInput) {
  const data = await executeGraphql(RegisterWithoutInvitationDocument, {
    input,
  }).catch(() => null);
  const jwt = data?.registerWithoutInvitation?.result?.jwt;
  if (!jwt) return 'Registraci se nepodařilo dokončit';

  await setSessionCookie(jwt);

  redirect('/profil');
}

export async function registerUsingInvitationAction(input: RegisterUsingInvitationInput) {
  const data = await executeGraphql(RegisterUsingInvitationDocument, {
    input,
  }).catch(() => null);
  const result = data?.registerUsingInvitation?.result;
  if (!result?.jwt) return 'Registraci se nepodařilo dokončit';

  await setSessionCookie(result.jwt);
  const tenant = await getRequestTenant();
  redirect(
    !result.usr?.userProxiesList.length
      ? '/profil'
      : (tenant.config.publicSite ? '/dashboard' : '/rozpis'),
  );
}
