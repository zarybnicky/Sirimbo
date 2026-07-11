'use server';

import { headers } from 'next/headers';
import {
  LoginDocument,
  OtpLoginDocument,
  RegisterUsingInvitationDocument,
  RegisterWithoutInvitationDocument,
  type UserAuthFragment,
} from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';

export type AuthResult =
  | { status: 'error'; error: string }
  | { status: 'success'; user: UserAuthFragment | null };

const INVALID_CREDENTIALS = 'Nesprávné jméno nebo heslo';
const INVALID_LINK = 'Použitý odkaz již vypršel nebo je neplatný.';
const REGISTRATION_FAILED = 'Registraci se nepodařilo dokončit';

// Auth mutations run server-to-server so the resulting JWT can be planted in an
// httpOnly cookie the client never sees. Next protects server actions against
// CSRF (Origin/Host check), so no manual guard is needed here.
async function persistSession(jwt: string) {
  const host = (await headers()).get('host');
  await setSessionCookie(jwt, host);
}

export async function loginAction(input: {
  login: string;
  passwd: string;
}): Promise<AuthResult> {
  const login = input.login?.trim();
  const passwd = input.passwd;
  if (!login || !passwd) {
    return { status: 'error', error: 'Zadejte jméno a heslo' };
  }
  try {
    const data = await executeGraphql(LoginDocument, { login, passwd });
    const result = data.login?.result;
    if (!result?.jwt) return { status: 'error', error: INVALID_CREDENTIALS };
    await persistSession(result.jwt);
    return { status: 'success', user: result.usr ?? null };
  } catch {
    return { status: 'error', error: INVALID_CREDENTIALS };
  }
}

export async function otpLoginAction(token: string): Promise<AuthResult> {
  if (!token) return { status: 'error', error: INVALID_LINK };
  try {
    const data = await executeGraphql(OtpLoginDocument, { token });
    const result = data.otpLogin?.result;
    if (!result?.jwt) return { status: 'error', error: INVALID_LINK };
    await persistSession(result.jwt);
    return { status: 'success', user: result.usr ?? null };
  } catch {
    return { status: 'error', error: INVALID_LINK };
  }
}

export async function registerWithoutInvitationAction(input: {
  email: string;
  passwd: string;
}): Promise<AuthResult> {
  try {
    const data = await executeGraphql(RegisterWithoutInvitationDocument, { input });
    const result = data.registerWithoutInvitation?.result;
    if (!result?.jwt) return { status: 'error', error: REGISTRATION_FAILED };
    await persistSession(result.jwt);
    return { status: 'success', user: result.usr ?? null };
  } catch (e) {
    return {
      status: 'error',
      error: e instanceof Error ? e.message : REGISTRATION_FAILED,
    };
  }
}

export async function registerUsingInvitationAction(input: {
  email: string;
  passwd: string;
  token: string;
}): Promise<AuthResult> {
  try {
    const data = await executeGraphql(RegisterUsingInvitationDocument, { input });
    const result = data.registerUsingInvitation?.result;
    if (!result?.jwt) return { status: 'error', error: REGISTRATION_FAILED };
    await persistSession(result.jwt);
    return { status: 'success', user: result.usr ?? null };
  } catch (e) {
    return {
      status: 'error',
      error: e instanceof Error ? e.message : REGISTRATION_FAILED,
    };
  }
}
