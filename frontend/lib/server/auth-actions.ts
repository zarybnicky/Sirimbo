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
const SERVICE_UNAVAILABLE =
  'Přihlášení je momentálně nedostupné, zkuste to prosím později.';

type MutationResult =
  | { jwt?: string | null; usr?: UserAuthFragment | null }
  | null
  | undefined;

// Runs an auth mutation, plants the session cookie from the returned token, and
// maps failures: a GraphQL-level rejection reads as `rejected` (or its own
// message when `rejected` is unset), transport failures as service unavailable.
async function establishSession(
  run: () => Promise<MutationResult>,
  rejected?: string,
): Promise<AuthResult> {
  try {
    const result = await run();
    if (!result?.jwt) return { status: 'error', error: rejected ?? REGISTRATION_FAILED };
    const requestHeaders = await headers();
    await setSessionCookie(result.jwt, requestHeaders.get('host'));
    return { status: 'success', user: result.usr ?? null };
  } catch (e) {
    if (e instanceof GraphqlOperationError) {
      return { status: 'error', error: rejected ?? e.message };
    }
    return { status: 'error', error: SERVICE_UNAVAILABLE };
  }
}

export async function loginAction(input: {
  login: string;
  passwd: string;
}): Promise<AuthResult> {
  const login = input.login?.trim();
  if (!login || !input.passwd) return { status: 'error', error: 'Zadejte jméno a heslo' };
  return establishSession(async () => {
    const data = await executeGraphql(LoginDocument, { login, passwd: input.passwd });
    return data.login?.result;
  }, INVALID_CREDENTIALS);
}

export async function otpLoginAction(token: string): Promise<AuthResult> {
  if (!token) return { status: 'error', error: INVALID_LINK };
  return establishSession(async () => {
    const data = await executeGraphql(OtpLoginDocument, { token });
    return data.otpLogin?.result;
  }, INVALID_LINK);
}

export async function registerWithoutInvitationAction(input: {
  email: string;
  passwd: string;
}): Promise<AuthResult> {
  return establishSession(async () => {
    const data = await executeGraphql(RegisterWithoutInvitationDocument, { input });
    return data.registerWithoutInvitation?.result;
  });
}

export async function registerUsingInvitationAction(input: {
  email: string;
  passwd: string;
  token: string;
}): Promise<AuthResult> {
  return establishSession(async () => {
    const data = await executeGraphql(RegisterUsingInvitationDocument, { input });
    return data.registerUsingInvitation?.result;
  });
}
