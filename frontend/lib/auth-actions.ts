'use server';

import { RegisterUsingInvitationInput, RegisterWithoutInvitationInput } from '@/graphql';
import {
  LoginDocument,
  RegisterUsingInvitationDocument,
  RegisterWithoutInvitationDocument,
  type LoginMutationVariables,
} from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';
import { getRequestTenant } from '@/lib/server/tenant';
import { redirect } from 'next/navigation';

export async function loginAction(values: LoginMutationVariables, from?: string | null) {
  try {
    const data = await executeGraphql(LoginDocument, values);
    const result = data.login?.result;
    if (!result?.jwt) return 'Přihlášení se nezdařilo';

    await setSessionCookie(result.jwt);
  } catch (error) {
    return error instanceof Error ? error.message : 'Přihlášení se nezdařilo';
  }
  const tenant = await getRequestTenant();
  redirect(from || (tenant.config.publicSite ? '/dashboard' : '/rozpis'));
}

export async function registerAction(input: RegisterWithoutInvitationInput) {
  try {
    const data = await executeGraphql(RegisterWithoutInvitationDocument, { input });
    const jwt = data.registerWithoutInvitation?.result?.jwt;
    if (!jwt) return 'Registraci se nepodařilo dokončit';

    await setSessionCookie(jwt);
  } catch (error) {
    return error instanceof Error ? error.message : 'Registraci se nepodařilo dokončit';
  }
  redirect('/profil');
}

export async function registerUsingInvitationAction(input: RegisterUsingInvitationInput) {
  try {
    const data = await executeGraphql(RegisterUsingInvitationDocument, { input });
    const result = data.registerUsingInvitation?.result;
    if (!result?.jwt) return 'Registraci se nepodařilo dokončit';

    await setSessionCookie(result.jwt);
  } catch (error) {
    return error instanceof Error ? error.message : 'Registraci se nepodařilo dokončit';
  }
  const tenant = await getRequestTenant();
  redirect(tenant.config.publicSite ? '/dashboard' : '/rozpis');
}
