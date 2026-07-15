import { NextResponse, type NextRequest } from 'next/server';
import { LogInAsDocument } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { sameOrigin, setSessionCookie } from '@/lib/server/session';
import { decodeClaims } from '@/lib/session-claims';

// Impersonation runs with the admin's own cookie (forwarded by executeGraphql);
// the returned token replaces the session cookie.
export async function POST(req: NextRequest) {
  if (!sameOrigin(req)) return NextResponse.json({ error: 'Invalid origin' }, { status: 403 });

  const { id } = await req.json().catch(() => ({ id: null }));
  if (typeof id !== 'string' || !id) {
    return NextResponse.json({ error: 'Chybí ID uživatele' }, { status: 400 });
  }

  try {
    const { logInAs } = await executeGraphql(LogInAsDocument, { id });
    const result = logInAs?.result;
    if (!result?.jwt) {
      return NextResponse.json({ error: 'Nepodařilo se přihlásit' }, { status: 403 });
    }
    await setSessionCookie(result.jwt, req.headers.get('host'));
    return NextResponse.json({ user: result.usr ?? null, claims: decodeClaims(result.jwt) });
  } catch {
    return NextResponse.json({ error: 'Nepodařilo se přihlásit' }, { status: 403 });
  }
}
