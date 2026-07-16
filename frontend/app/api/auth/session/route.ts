/* eslint-disable import-x/no-unused-modules */
import { NextResponse, type NextRequest } from 'next/server';
import { CurrentUserDocument } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { sameOrigin, setSessionCookie } from '@/lib/server/session';

// "Who am I": current user + claims, resolved from the session cookie.
export async function GET() {
  try {
    const data = await executeGraphql(CurrentUserDocument);
    return NextResponse.json({
      user: data.getCurrentUser ?? null,
      claims: data.currentClaims ?? null,
    });
  } catch {
    return NextResponse.json({ user: null, claims: null });
  }
}

// TODO(cookie-migration): upgrade shim — plants the httpOnly cookie from a
// legacy localStorage token. The token isn't verified here (the backend
// verifies on every request); delete together with tokenAtom.
export async function POST(req: NextRequest) {
  if (!sameOrigin(req)) return NextResponse.json({ error: 'Invalid origin' }, { status: 403 });

  const { token } = await req.json().catch(() => ({ token: null }));
  if (typeof token !== 'string' || token.split('.').length !== 3) {
    return NextResponse.json({ error: 'Neplatný token' }, { status: 400 });
  }

  await setSessionCookie(token, req.headers.get('host'));
  return NextResponse.json({ ok: true });
}
