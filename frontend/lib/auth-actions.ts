'use server';

import type { RegisterUsingInvitationInput, RegisterWithoutInvitationInput } from '@/graphql';
import {
  CurrentUserDocument,
  LoginDocument,
  LogInAsDocument,
  RegisterUsingInvitationDocument,
  RegisterWithoutInvitationDocument,
  type LoginMutationVariables,
} from '@/graphql/CurrentUser';
import type { RequestAuthState } from '@/lib/auth';
import { normalizeClaims } from '@/lib/auth-claims';
import { buildId } from '@/lib/build-id';
import { executeGraphql } from '@/lib/server/graphql';
import { sanitizeReturnURL } from '@/lib/sanitize';
import { clearSessionCookie, setSessionCookie } from '@/lib/server/session';
import { getRequestContext } from '@/lib/server/tenant';
import { isDeepStrictEqual } from 'node:util';

type AuthActionResult = { error: string } | { redirectTo: string };

export async function logoutAction(): Promise<void> {
  await clearSessionCookie();
}

export async function logInAsAction(id: string): Promise<AuthActionResult> {
  if (typeof id !== 'string' || !id) return { error: 'Chybí ID uživatele' };

  try {
    const { logInAs } = await executeGraphql(LogInAsDocument, { id });
    const token = logInAs?.result?.jwt;
    if (!token) return { error: 'Přihlášení selhalo' };

    await setSessionCookie(token);
  } catch {
    return { error: 'Přihlášení selhalo' };
  }
  return { redirectTo: '/dashboard' };
}

export async function refreshSessionAction(): Promise<RequestAuthState> {
  const context = await getRequestContext();
  if (!context.claims) {
    return { claims: null, user: null };
  }

  const data = await executeGraphql(CurrentUserDocument, {
    versionId: buildId,
    withRefreshJwt: true,
  });
  const auth = {
    claims: normalizeClaims(data.currentClaims),
    user: data.getCurrentUser,
  };
  if (
    auth.claims &&
    !isDeepStrictEqual(context.claims, auth.claims) &&
    typeof data.refreshJwt === 'string'
  ) {
    await setSessionCookie(data.refreshJwt);
  }

  return auth;
}

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
  const redirectTo = sanitizeReturnURL(from, 'https://app.invalid') ?? '/dashboard';
  return { redirectTo };
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

export async function acceptInvitationAction(
  input: RegisterUsingInvitationInput,
  from?: string | null,
): Promise<AuthActionResult> {
  try {
    const data = await executeGraphql(RegisterUsingInvitationDocument, { input });
    const result = data.registerUsingInvitation?.result;
    if (!result?.jwt) return { error: 'Pozvánku se nepodařilo přijmout' };

    await setSessionCookie(result.jwt);
  } catch (error) {
    return {
      error:
        error instanceof Error ? error.message : 'Pozvánku se nepodařilo přijmout',
    };
  }
  return { redirectTo: sanitizeReturnURL(from, 'https://app.invalid') ?? '/dashboard' };
}
