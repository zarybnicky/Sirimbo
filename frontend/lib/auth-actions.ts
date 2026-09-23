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

type AuthActionResult = { error: string } | { redirectTo: string };

export async function loginAction(
  values: LoginMutationVariables,
  from?: string | null,
): Promise<AuthActionResult> {
  try {
    const data = await executeGraphql(LoginDocument, values);
    const result = data.login?.result;
    if (!result?.jwt) return { error: 'Přihlášení se nezdařilo' };

    await setSessionCookie(result.jwt);
  } catch (error) {
    return {
      error: error instanceof Error ? error.message : 'Přihlášení se nezdařilo',
    };
  }
  return {
    redirectTo: from?.startsWith('/') && !from.startsWith('//') ? from : '/dashboard',
  };
}

export async function registerAction(
  input: RegisterWithoutInvitationInput,
): Promise<AuthActionResult> {
  try {
    const data = await executeGraphql(RegisterWithoutInvitationDocument, { input });
    const jwt = data.registerWithoutInvitation?.result?.jwt;
    if (!jwt) return { error: 'Registraci se nepodařilo dokončit' };

    await setSessionCookie(jwt);
  } catch (error) {
    return {
      error:
        error instanceof Error ? error.message : 'Registraci se nepodařilo dokončit',
    };
  }
  return { redirectTo: '/profil' };
}

export async function registerUsingInvitationAction(
  input: RegisterUsingInvitationInput,
): Promise<AuthActionResult> {
  try {
    const data = await executeGraphql(RegisterUsingInvitationDocument, { input });
    const result = data.registerUsingInvitation?.result;
    if (!result?.jwt) return { error: 'Registraci se nepodařilo dokončit' };

    await setSessionCookie(result.jwt);
  } catch (error) {
    return {
      error:
        error instanceof Error ? error.message : 'Registraci se nepodařilo dokončit',
    };
  }
  return { redirectTo: '/dashboard' };
}
