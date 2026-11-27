'use server';

import type {
  RegisterUsingInvitationInput,
  RegisterWithoutInvitationInput,
} from '@/graphql';
import {
  LogInAsDocument,
  LoginDocument,
  type LoginMutationVariables,
  RegisterUsingInvitationDocument,
  RegisterWithoutInvitationDocument,
} from '@/graphql/CurrentUser.ts';
import type { RequestAuthState } from '@/lib/auth.ts';
import { normalizeClaims } from '@/lib/auth-claims.ts';
import { executeGraphql } from '@/lib/server/graphql.ts';
import { sanitizeReturnURL } from '@/lib/sanitize.ts';
import {
  clearSessionCookie,
  loadCurrentUser,
  setSessionCookie,
} from '@/lib/server/session.ts';
import { getRequestContext } from '@/lib/server/tenant.ts';

type AuthActionResult = { error: string } | { redirectTo: string };

export async function logoutAction(): Promise<void> {
  await clearSessionCookie();
}

export async function logInAsAction(id: string): Promise<
  { error: string } | { redirectTo: string; auth: RequestAuthState }
> {
  if (typeof id !== 'string' || !id) return { error: 'Chybí ID uživatele' };

  try {
    const { pgSettings } = await getRequestContext();
    const { logInAs } = await executeGraphql(LogInAsDocument, { id }, {
      pgSettings: process.env.NODE_ENV === 'development'
        ? { ...pgSettings, role: 'system_admin' }
        : pgSettings,
    });
    const token = logInAs?.result?.jwt;
    if (!token) return { error: 'Přihlášení selhalo' };

    await setSessionCookie(token);
    return {
      redirectTo: '/dashboard',
      auth: {
        claims: normalizeClaims(logInAs?.result?.usr?.claims),
        user: logInAs?.result?.usr ?? null,
      },
    };
  } catch {
    return { error: 'Přihlášení selhalo' };
  }
}

export async function refreshSessionAction(): Promise<RequestAuthState> {
  const { auth, stale, refreshJwt } = await loadCurrentUser(true);
  if (stale && refreshJwt) await setSessionCookie(refreshJwt);
  return auth;
}

export async function loginAction(
  values: LoginMutationVariables,
  from?: string | null,
): Promise<AuthActionResult> {
  try {
    const data = await executeGraphql(LoginDocument, values);
    const result = data.login?.result;
    if (!result?.jwt) return { error: 'INVALID_CREDENTIALS' };

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
