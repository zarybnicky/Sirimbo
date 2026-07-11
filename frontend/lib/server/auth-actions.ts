'use server';

import { headers } from 'next/headers';
import { LoginDocument, type UserAuthFragment } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';

export type LoginState =
  | { status: 'error'; error: string }
  | { status: 'success'; user: UserAuthFragment | null };

// Server action: runs the login mutation server-to-server and plants the JWT in
// an httpOnly cookie the client never sees. Next provides CSRF protection for
// server actions (Origin/Host check), so no manual guard is needed here. Other
// API consumers keep using the bearer token against /graphql directly.
export async function loginAction(input: {
  login: string;
  passwd: string;
}): Promise<LoginState> {
  const login = input.login?.trim();
  const passwd = input.passwd;
  if (!login || !passwd) {
    return { status: 'error', error: 'Zadejte jméno a heslo' };
  }

  try {
    const data = await executeGraphql(LoginDocument, { login, passwd });
    const result = data.login?.result;
    if (!result?.jwt) {
      return { status: 'error', error: 'Nesprávné jméno nebo heslo' };
    }
    const host = (await headers()).get('host');
    await setSessionCookie(result.jwt, host);
    return { status: 'success', user: result.usr ?? null };
  } catch {
    return { status: 'error', error: 'Nesprávné jméno nebo heslo' };
  }
}
