'use server';

import { headers } from 'next/headers';
import {
  LoginDocument,
  OtpLoginDocument,
  RegisterUsingInvitationDocument,
  RegisterWithoutInvitationDocument,
  type UserAuthFragment,
} from '@/graphql/CurrentUser';
import { executeGraphql, GraphqlOperationError } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';

export type AuthResult =
  | { status: 'error'; error: string }
  | { status: 'success'; user: UserAuthFragment | null };

const INVALID_CREDENTIALS = 'Nesprávné jméno nebo heslo';
const INVALID_LINK = 'Použitý odkaz již vypršel nebo je neplatný.';
const REGISTRATION_FAILED = 'Registraci se nepodařilo dokončit';
const SERVICE_UNAVAILABLE = 'Přihlášení je momentálně nedostupné, zkuste to prosím později.';

async function persistSession(jwt: string) {
  await setSessionCookie(jwt, (await headers()).get('host'));
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
    await persistSession(result.jwt);
    return { status: 'success', user: result.usr ?? null };
  } catch (e) {
    if (e instanceof GraphqlOperationError) {
      return { status: 'error', error: INVALID_CREDENTIALS };
    }
    return { status: 'error', error: SERVICE_UNAVAILABLE };
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
  } catch (e) {
    if (e instanceof GraphqlOperationError) return { status: 'error', error: INVALID_LINK };
    return { status: 'error', error: SERVICE_UNAVAILABLE };
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
    if (e instanceof GraphqlOperationError) return { status: 'error', error: e.message };
    return { status: 'error', error: SERVICE_UNAVAILABLE };
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
    if (e instanceof GraphqlOperationError) return { status: 'error', error: e.message };
    return { status: 'error', error: SERVICE_UNAVAILABLE };
  }
}
