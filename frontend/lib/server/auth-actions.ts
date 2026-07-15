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
import { decodeClaims, type SessionClaims } from '@/lib/session-claims';

export type AuthResult =
  | { status: 'error'; error: string }
  | { status: 'success'; user: UserAuthFragment | null; claims: SessionClaims | null };

const INVALID_CREDENTIALS = 'Nesprávné jméno nebo heslo';
const INVALID_LINK = 'Použitý odkaz již vypršel nebo je neplatný.';
const REGISTRATION_FAILED = 'Registraci se nepodařilo dokončit';

async function persistSession(jwt: string): Promise<SessionClaims | null> {
  await setSessionCookie(jwt, (await headers()).get('host'));
  return decodeClaims(jwt);
}

export async function loginAction(input: {
  login: string;
  passwd: string;
}): Promise<AuthResult> {
  const login = input.login?.trim();
  if (!login || !input.passwd) return { status: 'error', error: 'Zadejte jméno a heslo' };
  try {
    const data = await executeGraphql(LoginDocument, { login, passwd: input.passwd });
    const result = data.login?.result;
    if (!result?.jwt) return { status: 'error', error: INVALID_CREDENTIALS };
    return { status: 'success', user: result.usr ?? null, claims: await persistSession(result.jwt) };
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
    return { status: 'success', user: result.usr ?? null, claims: await persistSession(result.jwt) };
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
    return { status: 'success', user: result.usr ?? null, claims: await persistSession(result.jwt) };
  } catch (e) {
    return { status: 'error', error: e instanceof Error ? e.message : REGISTRATION_FAILED };
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
    return { status: 'success', user: result.usr ?? null, claims: await persistSession(result.jwt) };
  } catch (e) {
    return { status: 'error', error: e instanceof Error ? e.message : REGISTRATION_FAILED };
  }
}
